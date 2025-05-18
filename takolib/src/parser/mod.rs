use error::ParseError;
use log::{debug as trace, warn};
// use log::{trace, warn};
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

#[derive(Clone, Default, Copy, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
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
        self
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
        assert!(self.is_infix);
        self.is_right = true;
        self
    }
    fn lit(&mut self, lit: Literal) -> &mut Self {
        assert!(self.lit.is_none());
        self.lit = Some(lit);
        self
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
            Comma, Sequence, Assign, Arrow, DoubleArrow, Add, Sub, Div, Mul, Exp, HasType,
            Sigma, Lambda, Forall, Pi, Exists,
            OpenParen, NumberLit, ColorLit,
            StringLit, Ident, CloseParen, // TODO: Add all
        ],
    })
}

impl Parser {
    fn rule(&self, op: Symbol) -> Rule {
        self.config.rules[op as usize]
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

    fn identifier(&mut self, token: Token) -> NodeId {
        let loc = token.location();
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

    pub fn parse_step_one(&mut self, level: usize) -> Result<NodeId, ParseError> {
        let results = self.parse_step(level)?;
        assert_eq!(results.len(), 1);
        Ok(results[0])
    }

    pub fn parse_step(&mut self, level: usize) -> Result<SmallVec<NodeId, CALL_ARGS_STANDARD_ITEM_NUM>, ParseError> {
        // NOTE: If support (a b) or something like that we need to return a vector of nodes

        if level >= self.config.precedences.len() {
            if let Some(tok) = self.head.tokens.pop_front() {
                return Err(ParseError::UnparsedTokens {
                    token: tok.kind,
                    location: tok.location(),
                });
            }
            todo!("NO PARSE: {level:?} {tokens:?}", tokens = self.head.tokens);
        }
        let op = self.config.precedences[level];
        let rule = self.rule(op);

        let old_level = level;
        let level = level + 1; // Inners.

        if rule.is_infix {
            let mut left = self.parse_step_one(level)?;
            let level = if let Some(level) = rule.level {
                level
            } else {
                level
            };
            while let Ok(mut loc) = self.tok(op) {
                trace!(
                    "I> {tokens:?} #{level:?} {op:?}", // => {rule:?}",
                    tokens = self.head.tokens.front()
                );
                let right = self.parse_step_one(if rule.is_right { old_level } else { level })?;

                if let Some(pair) = rule.pair {
                    let end_loc = self.tok(pair)?; // TODO: Handle missing
                    trace!(
                        "C> {tokens:?} #{level:?} {op:?}", // => {rule:?}",
                        tokens = self.head.tokens.front()
                    );
                    loc = loc.merge(end_loc);
                }

                // TODO: Functions on Asts?
                if op == Symbol::OpenParen {
                    left = self.ast.add_args(left, smallvec![right]); // TODO: Handle commas...
                    continue;
                }
                if op == Symbol::HasType {
                    left = self.ast.add_annotation(left, right);
                    continue;
                }
                if op == Symbol::Assign {
                    left = self.ast.add_implementation(left, right);
                    continue;
                }

                left = self.ast.add_op(
                    Op {
                        op,
                        args: smallvec![left, right],
                    },
                    loc,
                );
            }
            return Ok(smallvec![left]);
        }
        if rule.is_prefix {
            let mut locs = Vec::new();
            while let Ok(loc) = self.tok(op) {
                trace!(
                    "P> {tokens:?} #{level:?} {op:?}", // => {rule:?}",
                    tokens = self.head.tokens.front()
                );
                locs.push(loc);
            }
            let level = if let Some(level) = rule.level {
                level
            } else {
                level
            };
            let mut inner = self.parse_step_one(level)?;
            if let Some(pair) = rule.pair {
                for start_loc in locs.iter_mut().rev() {
                    let end_loc = self.tok(pair)?; // TODO: Handle missing
                    *start_loc = start_loc.merge(end_loc);
                }
            }

            for loc in locs.into_iter().rev() {
                if let Some(bind) = rule.bind {
                    inner = self.ast.set_bind(inner, bind);
                } else {
                    inner = self.ast.add_op(
                        Op {
                            op,
                            args: smallvec![inner],
                        },
                        loc,
                    );
                }
            }
            return Ok(smallvec![inner]);
        }
        if rule.can_drop {
            while let Ok(loc) = self.tok(op) {
                if rule.warn {
                    // TODO: Add a warning
                    warn!("DROP: {op} at {loc:?}");
                }
            }
            return self.parse_step(level);
        }
        if let Some(lit) = rule.lit {
            if let Ok(loc) = self.tok(op) {
                trace!(
                    "L> {tokens:?} #{level:?} {op:?}", // => {rule:?}",
                    tokens = self.head.tokens.front()
                );
                return Ok(smallvec![self.lit(lit, loc)]);
            }
            return self.parse_step(level);
        }

        if rule.is_ident {
            trace!(
                "V> {tokens:?} #{level:?} {op:?}", // => {rule:?}",
                tokens = self.head.tokens.front()
            );
            let next = self.head.eat()?;
            return Ok(smallvec![self.identifier(next)]);
        }

        todo!("Don't know what to do here {op:?} {rule:?}")
        // self.parse_step(level) // More specific
    }

    pub fn parse(&mut self) -> Result<(), ParseError> {
        // Dynamic recursive descent
        // TODO: Data stack for debugging.
        while !self.head.tokens.is_empty() {
            let roots = self.parse_step(0)?;

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
