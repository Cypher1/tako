use error::ParseError;
use log::{debug as trace, error};
// use log::{trace, error};
use semantics::{BindingMode, Literal};
use smallvec::smallvec;
use smallvec::SmallVec;
use std::collections::VecDeque;
use std::path::Path;
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

use tokens::{Symbol, Token};

#[derive(Clone, Default, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Rule {
    is_prefix: bool,
    is_infix: bool,
    is_ident: bool,
    can_drop: bool,
    warn: bool,
    is_right: bool,
    // TODO: Bit flag the above!
    lit: Option<Literal>,
    pair: Option<Symbol>,
    bind: Option<BindingMode>, // Reset to this level when parsing inside.
    level: Option<usize>, // Reset to this level when parsing inside.
}

impl Rule {
    fn can_drop(&mut self) -> &mut Self {
        self.can_drop = true;
        self
    }
    fn warn(&mut self) -> &mut Self {
        self.warn = true; // If drop: is extra.
        self
    }
    fn pair(&mut self, symbol: Symbol) -> &mut Self {
        assert!(self.pair.is_none());
        self.pair = Some(symbol);
        self
    }
    fn ident(&mut self) -> &mut Self {
        self.is_ident = true;
        self.prefix()
    }
    fn infix(&mut self) -> &mut Self {
        self.is_infix = true;
        self
    }
    fn prefix(&mut self) -> &mut Self {
        self.is_prefix = true;
        self
    }
    fn bind(&mut self, bind: BindingMode) -> &mut Self {
        assert!(self.is_prefix);
        assert!(self.bind.is_none());
        self.bind = Some(bind);
        self
    }
    fn right(&mut self) -> &mut Self {
        self.is_right = true;
        self
    }
    fn lit(&mut self, lit: Literal) -> &mut Self {
        assert!(self.lit.is_none());
        self.lit = Some(lit);
        self.prefix()
    }
    fn level(&mut self, level: usize) -> &mut Self {
        assert!(self.is_prefix || self.is_infix);
        assert!(self.level.is_none());
        self.level = Some(level);
        self
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
const BINDINGS: &[(Symbol, BindingMode)] = &[
    (Sigma, BindingMode::With),
    (Forall, BindingMode::Forall),
    (Pi, BindingMode::Forall),
    (Lambda, BindingMode::Given),
    (Exists, BindingMode::With),
];

const FUNCS: &[Symbol] = &[
    Arrow,
    DoubleArrow, // In case value level and type level must be different.
];

const SPECIAL: &[Symbol] = &[HasType, Range, Dot, Sequence, Comma];
const PREFIX_SPECIAL: &[Symbol] = &[GetAddress, Try, Spread];

/*
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

const OPS: &[Rule] = &[
    // OpSetup::new(COMPARISONS, Infix, ANY_VALUE),
    // OpSetup::new(ASSIGN, Infix, ANY_VALUE),
];
*/

#[derive(Debug)]
struct Parser {
    ast: Ast,
    config: ParserConfigTable,
    // TODO: Move tokens here, and make immutable, allowing multiple ParseHeads for backtracking.
    head: ParseHead,
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct ParseHead {
    tokens: VecDeque<Token>,
    location: Location,
}

#[derive(Debug)]
struct ParserConfigTable {
    rules: Vec<Rule>,
    precedences: Vec<Symbol>,
}

impl ParseHead {
    fn peek(&self) -> Result<Token, ParseError> {
        let Some(tok) = self.tokens.front().copied() else {
            return Err(ParseError::UnexpectedEof);
        };
        Ok(tok)
    }

    fn eat(&mut self) -> Result<Token, ParseError> {
        let Some(tok) = self.tokens.pop_front() else {
            todo!("expected eof")
        };
        self.location.start += tok.location().length as u16;
        Ok(tok)
    }
}

fn make_tables() -> Result<ParserConfigTable, ()> {
    let mut config = Vec::<Rule>::new();
    config.resize(Symbol::iter().len(), Rule::default());
    config[Symbol::NumberLit as usize].lit(Literal::Numeric);
    config[Symbol::ColorLit as usize].lit(Literal::Color);
    config[Symbol::StringLit as usize].lit(Literal::String);
    config[Symbol::FmtStringLitStart as usize].lit(Literal::String);
    config[Symbol::FmtStringLitMid as usize].lit(Literal::String);
    config[Symbol::FmtStringLitEnd as usize].lit(Literal::String);
    config[Symbol::Shebang as usize].can_drop();
    config[Symbol::OpenParen as usize]
        .pair(Symbol::CloseParen)
        .prefix()
        .infix()
        .level(0); // values or call
    config[Symbol::CloseParen as usize].can_drop().warn();
    config[Symbol::OpenBracket as usize]
        .pair(Symbol::CloseBracket)
        .prefix()
        .infix()
        .level(0); // container or index
    config[Symbol::OpenCurly as usize]
        .pair(Symbol::CloseCurly)
        .prefix()
        .level(0); // unordered container
    for op in PREFIX_MATHEMATICAL {
        config[*op as usize].prefix();
    }
    for op in MATHEMATICAL {
        config[*op as usize].infix();
    }
    config[Symbol::Exp as usize].right();
    for op in PREFIX_BIT {
        config[*op as usize].prefix();
    }
    for op in BIT {
        config[*op as usize].infix();
    }
    for op in SHIFT {
        config[*op as usize].infix();
    }
    for op in FUNCS {
        config[*op as usize].infix();
    }
    for op in COMPARISONS {
        config[*op as usize].infix();
    }
    for op in PREFIX_LOGICAL {
        config[*op as usize].prefix();
    }
    for op in LOGICAL {
        config[*op as usize].infix();
    }
    for op in PREFIX_SPECIAL {
        config[*op as usize].prefix();
    }
    for op in SPECIAL {
        config[*op as usize].infix();
    }
    for op in ASSIGN {
        config[*op as usize].infix();
    }
    for (op, bind) in BINDINGS {
        config[*op as usize].prefix().bind(*bind);
    }
    config[Ident as usize].ident();

    /*
    For reference, some interesting ideas about partial ordering of precedences:
        - https://www.foonathan.net/2017/07/operator-precedence/
    */

    Ok(ParserConfigTable {
        rules: config,
        precedences: vec![
            Comma, Sequence, Assign, Arrow,
            DoubleArrow, Add, Sub, Div, Mul, Exp, HasType,
            Eqs, NotEqs, Lt, LtEqs, Gt, GtEqs,
            Sigma, Lambda, Forall, Pi, Exists,
            CloseParen, // TODO: Add all
            OpenParen, NumberLit, ColorLit, StringLit, Ident,
        ],
    })
}

impl Parser {
    fn rule(&self, op: Symbol) -> &Rule {
        &self.config.rules[op as usize]
    }

    fn tok(&mut self, expected: Symbol) -> Result<Location, ParseError> {
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

    fn identifier(&mut self, loc: Location) -> NodeId {
        // TODO: Get the str from the Token
        let s = &self.ast.contents[loc.to_range()];
        let id = self.ast.string_interner.register_str(s);
        self.ast.add_identifier(id, loc)
    }

    fn lit(&mut self, lit: Literal, loc: Location) -> NodeId {
        // TODO: Get the str from the Token
        let s = &self.ast.contents[loc.to_range()];
        let _ = self.ast.string_interner.register_str_by_loc(s, loc.start);
        self.ast.add_literal(lit, loc)
    }

    // End of low level parsers.

    pub fn get_one(&mut self, results: SmallVec<NodeId, CALL_ARGS_STANDARD_ITEM_NUM>) -> Result<NodeId, ParseError> {
        if results.len() != 1 {
            for res in &results {
                trace!("RESULT(S): {}", self.ast.pretty_node(*res));
            }
        }
        assert_eq!(results.len(), 1);
        Ok(results[0])
    }

    /// We return a small vec of nodes to support commas (e.g. `a,b,c`) and prefixed words like `a b`.
    pub fn parse_step(&mut self, level: usize, depth: usize) -> Result<SmallVec<NodeId, CALL_ARGS_STANDARD_ITEM_NUM>, ParseError> {
        let location = self.head.location;
        if level >= self.config.precedences.len() {
            if let Some(tok) = self.head.tokens.pop_front() {
                return Err(ParseError::UnparsedTokens {
                    token: tok.kind,
                    location: tok.location(),
                });
            }
            return Err(ParseError::UnexpectedEof);
        }
        let op = self.config.precedences[level];
        // TODO: If we looked up the rule based on the next token, not the level
        // this would just be dynamic Pratt parsing.
        let rule = self.rule(op).clone();

        //trace!(
            //"{indent}> {op:?} for {tokens:?}",
            //indent = " ".repeat(depth),
            //tokens = self.head.tokens.front().unwrap_or(&Token { kind: Symbol::Escape, start: 0, length: 0 })
        //);
        let old_level = level;
        let level = if let Some(level) = rule.level {
            level
        } else {
            level + 1
        };
        let depth = depth+1;

        if rule.can_drop {
            while let Ok(loc) = self.tok(op) {
                if rule.warn {
                    // TODO: Add a warning
                    error!("DROP: {op} at {loc:?}");
                }
            }
        }

        let mut left = if rule.is_prefix {
            if let Ok(mut start_loc) = self.tok(op) {
                trace!(
                    "{indent}P> {op:?} #{level:?} {tokens:?}", // => {rule:?}",
                    indent = " ".repeat(depth),
                    tokens = self.head.tokens.front()
                );
                if rule.is_ident {
                    self.identifier(start_loc)
                } else if let Some(lit) = rule.lit {
                    self.lit(lit, start_loc)
                } else {
                    let inner = self.parse_step(level, depth)?;
                    if let Some(pair) = rule.pair {
                        let end_loc = self.tok(pair)?; // TODO: Handle missing
                        trace!(
                            "{indent}PC> {op:?} #{level:?} {tokens:?}", // => {rule:?}",
                            indent = " ".repeat(depth),
                            tokens = self.head.tokens.front()
                        );
                        start_loc = start_loc.merge(end_loc);
                    }
                    if let Some(bind) = rule.bind {
                        let inner = self.get_one(inner)?;
                        self.ast.set_bind(inner, bind)
                    } else if op != Symbol::OpenParen {
                        self.ast.add_op(
                            Op {
                                op,
                                args: inner,
                            },
                            start_loc,
                        )
                    } else {
                        self.get_one(inner)?
                    }
                }
            } else {
                let inner = self.parse_step(old_level + 1, depth)?;
                self.get_one(inner)?
            }
        } else {
            let inner = self.parse_step(old_level + 1, depth)?;
            self.get_one(inner)?
        };
        if rule.is_infix {
            let mut args: SmallVec<(SmallVec<NodeId, CALL_ARGS_STANDARD_ITEM_NUM>, Location), CALL_ARGS_STANDARD_ITEM_NUM> = smallvec![];
            while let Ok(mut loc) = self.tok(op) {
                trace!(
                    "{indent}I> {op:?} #{level:?} {tokens:?}", // => {rule:?}",
                    indent = " ".repeat(depth),
                    tokens = self.head.tokens.front()
                );
                let rights = self.parse_step(level, depth)?;
                if let Some(pair) = rule.pair {
                    let end_loc = self.tok(pair)?; // TODO: Handle missing
                    trace!(
                        "{indent}PC> {op:?} #{level:?} {tokens:?}", // => {rule:?}",
                        indent = " ".repeat(depth),
                        tokens = self.head.tokens.front()
                    );
                    loc = loc.merge(end_loc);
                }
                args.push((rights, loc));
            }

            if op == Symbol::Comma {
                let mut all_args: SmallVec<NodeId, CALL_ARGS_STANDARD_ITEM_NUM> = smallvec![left];
                for (rights, _loc) in args {
                    all_args.extend(rights);
                }
                return Ok(all_args);
            } else {
                if rule.is_right && !args.is_empty() {
                    // Reverse the args if needs be.
                    args.insert(0, (smallvec![left], location));
                    let (lefts, _) = args.pop().unwrap();
                    left = self.get_one(lefts)?;
                    args.reverse();
                }
                for (arg_set, loc) in args {
                    // TODO: Functions on Asts?
                    if op == Symbol::OpenParen {
                        left = self.ast.add_args(left, arg_set); // TODO: Handle commas...
                        continue;
                    }
                    let mut arg = self.get_one(arg_set)?;
                    left = match op {
                        Symbol::HasType => self.ast.add_annotation(left, arg),
                        Symbol::Assign => self.ast.add_implementation(left, arg),
                        _ => {
                            if rule.is_right {
                                (left, arg) = (arg, left);
                            }
                            self.ast.add_op(
                                Op {
                                    op,
                                    args: smallvec![left, arg],
                                },
                                loc,
                            )
                        }
                    };
                }
            }
        };
        return Ok(smallvec![left]);
    }

    pub fn parse(&mut self) -> Result<(), ParseError> {
        // Dynamic recursive descent
        // TODO: Data stack for debugging.
        while !self.head.tokens.is_empty() {
            let roots = self.parse_step(0, 0)?;
            trace!("roots {0:?}", roots.len());

            // Add all the valid trees that are not subtrees as roots.
            self.ast.roots.extend(roots);
        }
        Ok(())
    }
}

pub fn parse(file: &Path, input: &str, tokens: &[Token]) -> Result<Ast, TError> {
    let mut parser = Parser {
        head: ParseHead {
            location: Location {
                start: 0,
                length: 0,
            },
            tokens: VecDeque::from_iter(tokens.iter().copied()),
        },
        config: make_tables().expect("default config broken"),
        ast: Ast::new(file.to_path_buf(), input.to_string()),
    };

    // TODO: Support for streams?
    parser.parse()?;

    let ast = parser.ast;

    // TODO: Handle errors
    Ok(ast)
}

// TODO: Recover tests from ./old_mod.rs
