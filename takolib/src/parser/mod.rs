use better_std::include_strs;
use error::ParseError;
use log::debug;
use semantics::op_from_assign_op;
use semantics::Literal;
use smallvec::smallvec;
use smallvec::SmallVec;
use std::collections::VecDeque;
use std::path::Path;
use std::sync::Arc;
use strum::IntoEnumIterator;

use crate::ast::location::Location;
use crate::ast::Ast;
use crate::ast::NodeId;
use crate::ast::Op;
use crate::ast::CALL_ARGS_STANDARD_ITEM_NUM;
use crate::error::TError;

pub mod error;
pub mod lexer;
pub mod semantics;
pub mod tokens;

use tokens::{Symbol, Token, TokenType};

use super::ast::nodes::FMT_STR_STANDARD_ITEM_NUM;

pub const KEYWORDS: &[&str] = include_strs!("keywords.txt");

/*
#[derive(Copy, Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum OpBinding {
    // TODO: Remove for parser library errors!
    PostfixOp,
    PrefixOp,
    PrefixOrInfixBinOp,
    InfixOrPostfixBinOp,
    InfixBinOp,
    Open(Symbol),  // the associated Closer
    Close(Symbol), // the associated Opener
}
*/

#[derive(Clone, Default, Copy, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum NudKind {
    #[default]
    None,
    Ident,                                      // ~
    Lit(Literal),                                      // ~
    Drop,                                            // ~a => a
    Prefix,                                          // ~a
    Nested(/* continue */ Symbol, /* end */ Symbol), // (a, b, c, d, e, )
}
#[derive(Clone, Default, Copy, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum LedKind {
    #[default]
    None,
    Infix,                                                // a~b
    Postfix,                                              // a~
    NestedInfix(/* continue */ Symbol, /* end */ Symbol), // a(b, c, d, )
}

#[derive(Clone, Default, Copy, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct OpSetup {
    led: LedKind,
    nud: NudKind,
    // ??? contains: &'static [Symbol],
}

impl OpSetup {
    pub fn set_nud(&mut self, kind: NudKind) -> Result<(), ()> {
        if self.nud != NudKind::None {
            return Err(()); // todo!("Set Nud Error {kind:?} {:?}", self.nud); // TODO: Error
        }
        self.nud = kind;
        Ok(())
    }
    pub fn set_led(&mut self, kind: LedKind) -> Result<(), ()> {
        if self.led != LedKind::None {
            return Err(()); // todo!("Set Led Error {kind:?} {:?}", self.led); // TODO: Error
        }
        self.led = kind;
        Ok(())
    }
}

// In precedence order least tightly binding to most.
use Symbol::*;

// TODO: Use
const PREFIX_MATHEMATICAL: &[Symbol] = &[Add, Sub];

const MATHEMATICAL: &[Symbol] = &[Add, Sub, Exp, Div, Mul, Modulo];

const LOGICAL: &[Symbol] = &[LogicalAnd, LogicalOr];
const PREFIX_LOGICAL: &[Symbol] = &[LogicalAnd, LogicalOr];

const BIT: &[Symbol] = &[And, Or, BitXor];
const PREFIX_BIT: &[Symbol] = &[BitNot];
const SHIFT: &[Symbol] = &[LeftShift, RightShift];
const COMPARISONS: &[Symbol] = &[Eqs, NotEqs, Lt, LtEqs, Gt, GtEqs];

const ASSIGN: &[Symbol] = &[
    Assign,
    AddAssign,
    SubAssign,
    DivAssign,
    MulAssign,
    AndAssign,
    OrAssign,
    BitXorAssign,
    LogicalAndAssign,
    LogicalOrAssign,
    ModuloAssign,
];
const BINDINGS: &[Symbol] = &[Sigma, Lambda, Forall, Pi, Exists];

const FUNCS: &[Symbol] = &[
    Arrow,
    DoubleArrow, // In case value level and type level must be different.
];

const SPECIAL: &[Symbol] = &[
    HasType,
    Range,
    Dot,
    Sequence,
];
const PREFIX_SPECIAL: &[Symbol] = &[
    GetAddress,
    Try,
    Spread,
];

const ANY_VALUE: &[Symbol] = constcat::concat_slices!(
[Symbol]:
PREFIX_MATHEMATICAL,
MATHEMATICAL,
LOGICAL,
PREFIX_LOGICAL,
BIT,
PREFIX_BIT,
SHIFT,
COMPARISONS,
ASSIGN,
FUNCS,
SPECIAL,
PREFIX_SPECIAL,
);

const OPS: &[OpSetup] = &[
    // OpSetup::new(COMPARISONS, Infix, ANY_VALUE),
    // OpSetup::new(ASSIGN, Infix, ANY_VALUE),
];

#[derive(Clone, Debug, Eq, PartialEq)]
struct Parser {
    ast: Ast,
    parse_rules: ParserConfigTable,
    // TODO: Move tokens here, and make immutable, allowing multiple ParseHeads.
    head: ParseHead,
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct ParseHead {
    tokens: VecDeque<Token>,
    location: Location,
}

type Handler = Arc<dyn Fn(&mut Parser, NodeId) -> Result<NodeId, ParseError>>;
type HandlerTable = Vec<Option<Handler>>;

type ParserConfigTable = Vec<OpSetup>;

impl ParseHead {
    fn peek(&self) -> Result<Token, ParseError> {
        let Some(tok) = self.tokens.get(0).copied() else {
            todo!("expected eof")
        };
        Ok(tok)
    }

    fn peek_op(&self) -> Result<Symbol, ParseError> {
        let Token {
            kind,
            start,
            length,
        } = self.peek()?;
        let TokenType::OpType(op) = kind else {
            return Err(ParseError::UnexpectedTokenTypeExpectedOperator {
                got: kind,
                location: Location { start, length },
            });
        };
        Ok(op)
    }

    fn eat(&mut self) -> Result<Token, ParseError> {
        let Some(tok) = self.tokens.pop_front() else {
            todo!("expected eof")
        };
        self.location.start += tok.location().length as u16;
        return Ok(tok);
    }
}

fn make_tables() -> Result<ParserConfigTable, ()> {
    let mut config = ParserConfigTable::new();
    config.resize(Symbol::iter().len(), OpSetup::default());
    let mut table = HandlerTable::new();
    table.resize(Symbol::iter().len(), None);

    config[Symbol::Shebang as usize] = OpSetup {
        nud: NudKind::Drop,
        led: LedKind::None,
    };
    config[Symbol::OpenParen as usize] = OpSetup {
        nud: NudKind::Nested(Symbol::Comma, Symbol::CloseParen), // ( Value(s) )
        led: LedKind::NestedInfix(Symbol::Comma, Symbol::CloseParen), // Call
    };
    config[Symbol::OpenCurly as usize] = OpSetup {
        nud: NudKind::Nested(Symbol::Comma, Symbol::CloseCurly), // Unordered Set
        led: LedKind::None,                                      // TODO: Constructor? a{ b }
    };
    config[Symbol::OpenBracket as usize] = OpSetup {
        nud: NudKind::Nested(Symbol::Comma, Symbol::CloseBracket), // Ordered Values
        led: LedKind::NestedInfix(Symbol::Comma, Symbol::CloseBracket), // Index
    };
    for op in PREFIX_MATHEMATICAL {
        config[*op as usize].set_nud(NudKind::Prefix).expect(&format!("{op:?}"));
    }
    for op in MATHEMATICAL {
        config[*op as usize].set_led(LedKind::Infix).expect(&format!("{op:?}"));
    }
    for op in PREFIX_BIT {
        config[*op as usize].set_nud(NudKind::Prefix).expect(&format!("{op:?}"));
    }
    for op in BIT {
        config[*op as usize].set_led(LedKind::Infix).expect(&format!("{op:?}"));
    }
    for op in SHIFT {
        config[*op as usize].set_led(LedKind::Infix).expect(&format!("{op:?}"));
    }
    for op in COMPARISONS {
        config[*op as usize].set_led(LedKind::Infix).expect(&format!("{op:?}"));
    }
    for op in PREFIX_LOGICAL {
        config[*op as usize].set_nud(NudKind::Prefix).expect(&format!("{op:?}"));
    }
    for op in LOGICAL {
        config[*op as usize].set_led(LedKind::Infix).expect(&format!("{op:?}"));
    }
    for op in PREFIX_SPECIAL {
        config[*op as usize].set_nud(NudKind::Prefix).expect(&format!("{op:?}"));
    }
    for op in SPECIAL {
        config[*op as usize].set_led(LedKind::Infix).expect(&format!("{op:?}"));
    }
    for op in ASSIGN {
        config[*op as usize].set_led(LedKind::Infix).expect(&format!("{op:?}"));
    }
    for op in BINDINGS {
        config[*op as usize].set_nud(NudKind::Prefix).expect(&format!("{op:?}"));
    }
    Ok(config)
}

impl Parser {
    fn rule(&self, op: Symbol) -> OpSetup {
        self.parse_rules[op as usize]
    }

    fn tok(&mut self, expected: TokenType) -> Result<Location, ParseError> {
        let Token {
            kind,
            start,
            length,
        } = self.head.peek()?;
        if kind != expected {
            return Err(ParseError::UnexpectedTokenType {
                got: kind,
                location: Location { start, length },
                expected,
            });
        }
        self.head.eat().unwrap();
        Ok(Location { start, length })
    }

    fn op(&mut self, expected: Symbol) -> Result<Location, ParseError> {
        self.tok(TokenType::OpType(expected))
    }

    fn identifier(&mut self, token: Token) -> NodeId {
        let loc = token.location();
        // TODO: Get the str from the Token
        let s = &self.ast.contents[loc.to_range()];
        let id = self.ast.string_interner.register_str(s);
        self.ast.add_identifier(id, loc)
    }

    fn lit(&mut self, token: Token, lit: Literal) -> NodeId {
        let loc = token.location();
        // TODO: Get the str from the Token
        let s = &self.ast.contents[loc.to_range()];
        let _ = self.ast.string_interner.register_str_by_loc(s, loc.start);
        self.ast.add_literal(lit, loc)
    }

    // End of low level parsers.

    /*
    // TODO: Make into parse configs.
    fn shebang(&mut self) -> () {
        let _ = self.op(Symbol::Shebang);
    }

    fn comma(&mut self) -> Result<Location, ParseError> {
        self.op(Symbol::Comma)
    }

    fn fmt_str(&mut self) -> Result<NodeId, ParseError> {
        // TODO: This could be made 'non-special' with a little flag for what is expected next.
        let start = self.fmt_str_start()?;

        let mut nodes: SmallVec<NodeId, FMT_STR_STANDARD_ITEM_NUM> = SmallVec::new();
        nodes.push(start);
        nodes.push(self.fmt_str_start()?);
        nodes.push(self.expr()?);
        while let Ok(mid) = self.fmt_str_mid() {
            nodes.push(mid);
            nodes.push(self.expr()?);
        }
        nodes.push(self.fmt_str_end()?);
        let loc = self.ast[start].location;
        // TODO: Switch from op to call with built ins?
        Ok(self.ast.add_op(
            Op {
                op: Symbol::Group,
                args: nodes,
            },
            loc,
        ))
    }

    // High level parsers

    fn led(&mut self, left: NodeId, inside: Symbol) -> Result<NodeId, ParseError> {
        let tok = self.head.peek()?;
        // OR
        let curr = self.head.peek_op().ok();
        debug!("{curr:?} {tok:?}");
        debug!("led {:?}", self.rule(curr));
        match tok.kind {
            _ => todo!("led {tok:?}")
        }
    }

    fn nud(&mut self, inside: Symbol) -> Result<NodeId, ParseError> {
        let tok = self.head.peek()?;
        // OR
        let curr = self.head.peek_op().ok();
        debug!("{curr:?} {tok:?}");
        debug!("nud {:?}", self.rule(curr));
        match tok.kind {
            TokenType::Ident => self.identifier(),
            TokenType::NumberLit => self.number(),
            _ => todo!("nud {tok:?}")
        }
    }
    */

    pub fn parse(&mut self) -> Result<(), ParseError> {
        // Based on https://en.wikipedia.org/wiki/Shunting_yard_algorithm
        //
        let mut output: Vec<NodeId> = Vec::new(); // Done subtrees.
        let mut stack: Vec<Token> = Vec::new(); // Working stack / WIP.

        /* The functions referred to in this algorithm are simple single argument functions such as sine, inverse or factorial. */
        /* This implementation does not implement composite functions, functions with a variable number of arguments, or unary operators. */

        while let Some(token) = self.head.tokens.pop_front() {
            let config = match token.kind {
                TokenType::OpType(op) => self.rule(op),
                // TODO: Move all these to the rule table
                TokenType::Ident => OpSetup {
                    led: LedKind::None,
                    nud: NudKind::Ident,
                },
                TokenType::NumberLit => OpSetup {
                    led: LedKind::None,
                    nud: NudKind::Lit(Literal::Numeric),
                },
                TokenType::ColorLit => OpSetup {
                    led: LedKind::None,
                    nud: NudKind::Lit(Literal::Color),
                },
                TokenType::StringLit | TokenType::FmtStringLitStart | TokenType::FmtStringLitMid | TokenType::FmtStringLitEnd => OpSetup {
                    led: LedKind::None,
                    nud: NudKind::Lit(Literal::String),
                },
            };

            // If we can contain the previous subtree, we should.
            match config.led {
                LedKind::None => {}, // Can't do anything here.
                LedKind::Infix => {
                    let Some(left) = output.last().copied() else {
                        break
                    };
                    let can_contain = true;
                    if !can_contain {
                        break; // DONE
                    }
                    output.pop(); // Use up the left.
                    let TokenType::OpType(sym) = token.kind else {
                        todo!("?");
                    };
                    let op = self.ast.add_op(
                        Op {
                            op: sym,
                            args: smallvec![left],
                        },
                        token.location()
                    );
                    stack.push(token);
                    continue;
                }
                LedKind::Postfix => {
                    let Some(left) = output.last().copied() else {
                        break
                    };
                    let can_contain = true;
                    if !can_contain {
                        break; // DONE
                    }
                    output.pop(); // Use up the left.
                    let TokenType::OpType(sym) = token.kind else {
                        todo!("?");
                    };
                    let op = self.ast.add_op(
                        Op {
                            op: sym,
                            args: smallvec![left],
                        },
                        token.location()
                    );
                    stack.push((op, token)); // Might be done but might also be a Infix...
                    continue;
                }
                LedKind::NestedInfix(separator, end) => {
                    todo!("nested infix");
                }
            }
            // If we can't, prepare to eat the next subtree.
            match config.nud {
                NudKind::None => {}, // Can't do anything here.
                NudKind::Drop => {
                    continue;
                }
                NudKind::Ident => {
                    output.push(self.identifier(token));
                    continue;
                }
                NudKind::Lit(literal) => {
                    output.push(self.lit(token, literal));
                    continue;
                }
                NudKind::Prefix => {
                    todo!("prefix");
                }
                NudKind::Nested(separator, end) => {
                    todo!("nested");
                }
            }
        }
        /* After the while loop, pop the remaining items from the operator stack into the output queue. */
        /*
        while (there are tokens on the operator stack) {
            /* If the operator token on the top of the stack is a parenthesis, then there are mismatched parentheses. */
            {assert the_operator_on_top_of_the_stack_is_not_a__left__parenthesis}
            pop the operator from the operator stack onto the output queue
        }
        */

        // Add all the valid trees that are not subtrees as roots.
        self.ast.roots.extend(output);
        Ok(())
    }

    /*
    pub fn file(&mut self) -> Result<(), ParseError> {
        self.shebang();
        while self.head.peek().is_ok() {
            let root = self.expr()?;
            self.ast.roots.push(root);
        }
        Ok(())
    }*/
}

/*
    // Special Tokens
    let call = expr
        .clone()
        .then(args.clone()) // foo(a, b, c)
        .map_with(|(inner, ((open, args), _close)), extra| {
            let ast: &mut Ast = std::ops::DerefMut::deref_mut(extra.state());
            let loc = open; // TODO: Merge with _close
                            // TODO: Switch from op to call with built ins?
            ast.add_call(
                Call {
                    inner,
                    args,
                },
                loc,
            )
        })
        .labelled("a call");

    let index = expr
        .clone()
        .then(args) // foo[a, b, c]
        .map_with(|(inner, ((open, args), _close)), extra| {
            let ast: &mut Ast = std::ops::DerefMut::deref_mut(extra.state());
            let loc = open; // TODO: Merge with _close
                            // TODO: Switch from op to call with built ins?
            ast.add_call(
                Call {
                    inner,
                    args,
                },
                loc,
            )
        })
        .labelled("a call");

    /*
    For reference, some interesting ideas about partial ordering of precedences:
        - https://www.foonathan.net/2017/07/operator-precedence/
    */

    // TODO: Add semver.
    // TODO: Add headings: /====*[^='"]*====*\/,

    use Symbol::*;

    let params = many
        .clone()
        .delimited_by(op(Symbol::OpenParen), op(Symbol::CloseParen))
        .labelled("a set of parameters");

    let callable = expr
        .clone()
        .then(params)
        .map_with(|(lhs, rhs), extra| {
            let ast: &mut Ast = std::ops::DerefMut::deref_mut(extra.state());
            ast.add_args(lhs, rhs)
        })
        .labelled("a callable");

    let assign = select_ref! {
        Token {
            kind: OpType ( op @ (
                Assign
                | AddAssign
                | SubAssign
                | DivAssign
                | MulAssign
                | AndAssign
                | OrAssign
                | BitXorAssign
                | LogicalAndAssign
                | LogicalOrAssign
                | ModuloAssign
            )),
            start, length
        } => {
            (op, Location { start: *start, length: *length })
        },
    };

    // TODO: Binding types?
    let assignment = expr
        .clone()
        .then(assign)
        .then(expr.clone())
        .map_with(|((lhs, (assign, loc)), rhs), extra| {
            let ast: &mut Ast = std::ops::DerefMut::deref_mut(extra.state());
            let rhs = if let Some(op) = op_from_assign_op(*assign) {
                ast.add_op(
                    Op {
                        op,
                        args: smallvec![lhs, rhs],
                    },
                    loc,
                )
            } else {
                rhs
            };
            ast.add_implementation(lhs, rhs)
        })
        .labelled("definition");

    let untyped_atom = parens
        .or(callable)
        .or(assignment)
        .or(container)
        .or(call)
        .or(index)
        .or(unordered_container)
        .or(simple_value)
        .or(fmt_str)
        .labelled("a value");

    let typed_atom = untyped_atom
        .clone()
        .then_ignore(op(HasType))
        .then(untyped_atom.clone())
        .map_with(|(lhs, rhs), extra| {
            let ast: &mut Ast = std::ops::DerefMut::deref_mut(extra.state());
            ast.add_annotation(lhs, rhs)
        })
        .labelled("a typed value");

    let atom = typed_atom.or(untyped_atom);

    let prefix_op = |prec, op_type: Symbol| {
        prefix(prec, op(op_type), move |loc, rhs, extra| {
            let ast: &mut Ast = std::ops::DerefMut::deref_mut(extra.state());
            ast.add_op(
                Op {
                    op: op_type,
                    args: smallvec![rhs],
                },
                loc,
            )
        })
    };

    let infix_op = |prec, op_type: Symbol| {
        infix(prec, op(op_type), move |lhs, loc, rhs, extra| {
            let ast: &mut Ast = std::ops::DerefMut::deref_mut(extra.state());
            ast.add_op(
                Op {
                    op: op_type,
                    args: smallvec![lhs, rhs],
                },
                loc,
            )
        })
    };

    // TODO: Do these manually?
    expr.define(
        atom.pratt((
            // TODO: infix_op(left(13), Spread),
            // TODO: postfix_op(11, Try),
            // TODO: infix_op(left(9), DoubleArrow),
            // TODO: infix_op(left(8), Arrow),
            // TODO: left(4), Calls // a.b(x, y) | a.b[x, y] | a.b {x: y} => (a.b)<args>, a : b() => a : (b())
            // TOOD: try: 16,
            // TODO: spread: 15,
            // TODO: range: 15,
            infix_op(left(2), HasType), // @a : c => (@a) : c, a.b : c => (a.b) : c, a:b.c => a : (b.c)
            prefix_op(2, GetAddress),   // @a.b => @(a.b), @a.b() => @(a())
            infix_op(left(17), Dot),     // a.b
            prefix_op(15, BitNot),
            // TODO: infix_op(left(7), BitXor), // TODO: Check
            infix_op(left(7), Or),
            infix_op(left(9), And),
            // TODO: infix_op(left(10), LeftShift),
            // TODO: infix_op(left(10), RightShift),
            // TODO: prefix_op(15, LogicalNot),
            // TODO: infix_op(left(4), LogicalOr),
            // TODO: infix_op(left(5), LogicalAnd),
            prefix_op(11, Add),
            prefix_op(11, Sub),
            infix_op(left(12), Modulo),
            infix_op(right(13), Exp),
            infix_op(left(12), Div),
            infix_op(left(12), Mul),
            infix_op(left(15), Sub),
            infix_op(left(1), Add),
            // TODO: Not chaining.
            infix_op(left(6), Eqs),
            infix_op(left(6), NotEqs),
            infix_op(left(6), Lt),
            infix_op(left(6), LtEqs),
            infix_op(left(6), Gt),
            infix_op(left(6), GtEqs),
        ))
    );

    let roots =
        expr.separated_by(op(Symbol::Sequence))
            .collect()
            .map_with(|roots: Vec<NodeId>, extra| {
                let ast: &mut Ast = std::ops::DerefMut::deref_mut(extra.state());
                for root in roots {
                    ast.roots.push(root);
                }
            });

    let source_file = roots.clone().or(shebang.ignore_then(roots));

    Box::new(source_file)
}
*/

pub fn parse(file: &Path, input: &str, tokens: &[Token]) -> Result<Ast, TError> {
    let mut parser = Parser {
        head: ParseHead {
            location: Location {
                start: 0,
                length: 0,
            },
            tokens: VecDeque::from_iter(tokens.into_iter().copied()),
        },
        parse_rules: make_tables().expect("default config broken"),
        ast: Ast::new(file.to_path_buf(), input.to_string()),
    };

    // TODO: Support for streams?
    parser.parse()?;

    let ast = parser.ast;

    // TODO: Handle errors
    Ok(ast)
}

// TODO: Recover tests from ./old_mod.rs
