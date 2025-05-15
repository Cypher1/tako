use better_std::include_strs;
use error::ParseError;
use smallvec::smallvec;
use semantics::op_from_assign_op;
use semantics::Literal;
use smallvec::SmallVec;
use strum::IntoEnumIterator;
use std::collections::VecDeque;
use std::path::Path;
use std::sync::Arc;

use crate::ast::location::Location;
use crate::ast::NodeId;
use crate::ast::Op;
use crate::ast::Ast;
use crate::ast::CALL_ARGS_STANDARD_ITEM_NUM;
use crate::error::TError;

pub mod error;
pub mod lexer;
pub mod semantics;
pub mod tokens;

use tokens::{Symbol, Token, TokenType};

use super::ast::nodes::FMT_STR_STANDARD_ITEM_NUM;

pub const KEYWORDS: &[&str] = include_strs!("keywords.txt");

#[derive(Clone, Default, Copy, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum NudKind {
    #[default]
    None,
    Drop, // ~a => a
    Prefix, // ~a
    Nested(/* continue */ Symbol, /* end */ Symbol), // (a, b, c, d, e, )
}
#[derive(Clone, Default, Copy, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum LedKind {
    #[default]
    None,
    Infix, // a~b
    Postfix, // a~
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
            todo!("Error {kind:?}"); // TODO: Error
        }
        self.nud = kind;
        Ok(())
    }
    pub fn set_led(&mut self, kind: LedKind) -> Result<(), ()> {
        if self.led != LedKind::None {
            todo!("Error {kind:?}"); // TODO: Error
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

    const LOGICAL: &[Symbol] = &[LogicalNot, LogicalAnd, LogicalOr];

    const BIT: &[Symbol] = &[And, Or, BitNot, BitXor];

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
    const BINDINGS: &[Symbol] = &[
        Sigma,
        Lambda,
        Forall,
        Pi,
        Exists,
    ];

const FUNCS: &[Symbol] = &[
        Arrow,
        DoubleArrow, // In case value level and type level must be different.
    ];

const ANY_VALUE: &[Symbol] = constcat::concat_slices!(
[Symbol]:
PREFIX_MATHEMATICAL,
MATHEMATICAL,
LOGICAL,
BIT,
SHIFT,
COMPARISONS,
ASSIGN,
FUNCS,
&[
    // Special...
    GetAddress, Try, Dot, Range, Spread, Sequence, HasType,
]);

const OPS: &[OpSetup] = &[
    // OpSetup::new(COMPARISONS, Infix, ANY_VALUE),
    // OpSetup::new(ASSIGN, Infix, ANY_VALUE),
];

#[derive(Clone, Debug, Eq, PartialEq)]
struct Parser {
    ast: Ast,
    config: ParserConfigTable,
    // TODO: Move tokens here, and make immutable, allowing multiple ParseHeads.
    head: ParseHead,
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct ParseHead {
    tokens: VecDeque<Token>,
    location: Location,
    stack: Vec<Symbol>, // TODO: The stack.
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
        let kind = self.peek()?.kind;
        let TokenType::OpType(op) = kind else {
            todo!("expected op")
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
        led: LedKind::None // TODO: Constructor? a{ b }
    };
    config[Symbol::OpenBracket as usize] = OpSetup {
        nud: NudKind::Nested(Symbol::Comma, Symbol::CloseBracket), // Ordered Values
        led: LedKind::NestedInfix(Symbol::Comma, Symbol::CloseBracket), // Index
    };
    config[Symbol::Sequence as usize] = OpSetup {
        nud: NudKind::None,
        led: LedKind::Infix, // Index
    };
    for pref in PREFIX_MATHEMATICAL {
        config[*pref as usize].set_nud(NudKind::Prefix)?;
    }
    for assign in ASSIGN {
        config[*assign as usize].set_led(LedKind::Infix)?;
    }
    for bind in BINDINGS {
        config[*bind as usize].set_nud(NudKind::Prefix)?;
    }
    Ok(config)
}

impl Parser {
    fn tok(&mut self, expected: TokenType) -> Result<Location, ParseError> {
        let Token { kind, start, length } = self.head.peek()?;
        if kind != expected {
            return Err(ParseError::UnexpectedTokenType { got: kind, location: Location {start, length }, expected });
        }
        self.head.eat().unwrap();
        Ok(Location {start, length})
    }

    fn op(&mut self, expected: Symbol) -> Result<Location, ParseError> {
        self.tok(TokenType::OpType(expected))
    }

    fn identifier(&mut self) -> Result<NodeId, ParseError> {
        let Location { start, length } = self.tok(TokenType::Ident)?;
        // TODO: Get the str from the Token
        let s = &self.ast.contents[(start as usize)..(start+(length as u16)) as usize];
        let id = self.ast.string_interner.register_str(s);
        Ok(self.ast.add_identifier(id, Location { start, length }))
    }

    fn lit(&mut self, kind: TokenType, lit: Literal) -> Result<NodeId, ParseError> {
        let Location { start, length } = self.tok(kind)?;
        // TODO: Get the str from the Token
        let s = &self.ast.contents[(start as usize)..(start+(length as u16)) as usize];
        let _ = self.ast.string_interner.register_str_by_loc(s, start);
        Ok(self.ast.add_literal(lit, Location { start, length}))
    }
    fn number(&mut self) -> Result<NodeId, ParseError> {
        self.lit(TokenType::NumberLit, Literal::Numeric)
    }

    fn color(&mut self) -> Result<NodeId, ParseError> {
        self.lit(TokenType::ColorLit, Literal::Color)
    }

    fn str(&mut self) -> Result<NodeId, ParseError> {
        self.lit(TokenType::StringLit, Literal::String)
    }

    fn fmt_str_start(&mut self) -> Result<NodeId, ParseError> {
        self.lit(TokenType::FmtStringLitStart, Literal::String)
    }

    fn fmt_str_mid(&mut self) -> Result<NodeId, ParseError> {
        self.lit(TokenType::FmtStringLitMid, Literal::String)
    }

    fn fmt_str_end(&mut self) -> Result<NodeId, ParseError> {
        self.lit(TokenType::FmtStringLitEnd, Literal::String)
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

    fn shebang(&mut self) -> () {
        let _ = self.op(Symbol::Shebang);
    }

    fn comma(&mut self) -> Result<Location, ParseError> {
        self.op(Symbol::Comma)
    }

    // End of low level parsers.

    fn led(&mut self, left: NodeId, inside: Symbol) -> Result<NodeId, ParseError> {

        todo!("led")
    }

    fn nud(&mut self, inside: Symbol) -> Result<NodeId, ParseError> {

        todo!("nud")
    }

    pub fn expr(&mut self) -> Result<NodeId, ParseError> {
        let inside = self.head.stack.last().copied().unwrap_or(Symbol::OpenParen);
        let mut curr = self.nud(inside)?;

        while let Ok(next) = self.led(curr, inside) {
            curr = next;
        }

        Ok(curr)
    }

    pub fn file(&mut self) -> Result<(), ParseError> {
        self.shebang();
        while self.head.peek().is_ok() {
            let root = self.expr()?;
            self.ast.roots.push(root);
        }
        Ok(())
    }
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
            stack: Vec::new(),
        },
        config: make_tables().expect("default config broken"),
        ast: Ast::new(file.to_path_buf(), input.to_string()),
    };

    // TODO: Support for streams?
    parser.file()?;

    let ast = parser.ast;

    // TODO: Handle errors
    Ok(ast)
}

// TODO: Recover tests from ./old_mod.rs
