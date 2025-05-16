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

use tokens::{Symbol, Token};

use super::ast::nodes::FMT_STR_STANDARD_ITEM_NUM;

pub const KEYWORDS: &[&str] = include_strs!("keywords.txt");

/*
#[derive(Copy, Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum OpBinding {
    // TODO: Remove for parser library errors!
    PrefixOp,
    PrefixOrInfixBinOp,
    InfixBinOp,
    Open(Symbol),  // the associated Closer
    Close(Symbol), // the associated Opener
}
*/

type TokenType = Symbol;

#[derive(Clone, Default, Copy, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Rule {
    is_prefix: bool,
    is_infix: bool,
    lit: Option<Literal>,
    pair: Option<Symbol>,
    can_drop: bool,
    is_left: bool,
}

impl Rule {
    fn drop(&mut self) -> &mut Self {
        self.can_drop = true;
        self
    }
    fn pair(&mut self, symbol: Symbol) -> &mut Self {
        assert!(self.pair.is_none());
        self.pair = Some(symbol);
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
    fn left(&mut self) -> &mut Self {
        self.is_left = true;
        self
    }
    fn lit(&mut self, lit: Literal) -> &mut Self {
        assert!(self.lit.is_none());
        self.lit = Some(lit);
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
const BINDINGS: &[Symbol] = &[Sigma, Lambda, Forall, Pi, Exists];

const FUNCS: &[Symbol] = &[
    Arrow,
    DoubleArrow, // In case value level and type level must be different.
];

const SPECIAL: &[Symbol] = &[HasType, Range, Dot, Sequence];
const PREFIX_SPECIAL: &[Symbol] = &[GetAddress, Try, Spread];

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
        let Some(tok) = self.tokens.get(0).copied() else {
            todo!("expected eof")
        };
        Ok(tok)
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
    let mut config = Vec::<Rule>::new();
    config.resize(Symbol::iter().len(), Rule::default());
    config[Symbol::Shebang as usize].drop();
    config[Symbol::OpenParen as usize]
        .pair(Symbol::CloseParen)
        .prefix()
        .infix(); // values or call
    config[Symbol::OpenBracket as usize]
        .pair(Symbol::CloseBracket)
        .prefix()
        .infix(); // container or index
    config[Symbol::OpenCurly as usize]
        .pair(Symbol::CloseCurly)
        .prefix(); // unordered container
    for op in PREFIX_MATHEMATICAL {
        config[*op as usize].prefix();
    }
    for op in MATHEMATICAL {
        config[*op as usize].infix();
    }
    for op in PREFIX_BIT {
        config[*op as usize].prefix();
    }
    for op in BIT {
        config[*op as usize].infix();
    }
    for op in SHIFT {
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
    for op in BINDINGS {
        config[*op as usize].prefix();
    }

    /*
    For reference, some interesting ideas about partial ordering of precedences:
        - https://www.foonathan.net/2017/07/operator-precedence/
    */

    let mut config = ParserConfigTable {
        rules: config,
        precedences: vec![
            Sequence, Add, Sub, Div, Mul, NumberLit,
            // TODO: Add all
        ],
    };
    Ok(config)
}

impl Parser {
    fn op_rule(&self, op: Symbol) -> Rule {
        self.config.rules[op as usize]
    }

    fn rule(&self, kind: Symbol) -> Rule {
        match kind {
            // TODO: Move all these to the rule table
            TokenType::Ident => Rule::default(), // If we don't know what a thing is it is an identifier, and must be defined elsewhere.
            TokenType::NumberLit => *Rule::default().lit(Literal::Numeric),
            TokenType::ColorLit => *Rule::default().lit(Literal::Color),
            TokenType::StringLit
            | TokenType::FmtStringLitStart
            | TokenType::FmtStringLitMid
            | TokenType::FmtStringLitEnd => *Rule::default().lit(Literal::String),
            op => self.op_rule(op),
        }
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

    pub fn parse_step(&mut self, level: usize) -> Result<NodeId, ParseError> {
        if level >= self.config.precedences.len() {
            todo!("NO PARSE");
        }
        let op = self.config.precedences[level];
        let rule = self.op_rule(op);

        debug!("{level:?} => {rule:?}");
        if rule.is_prefix {
            if let Ok(loc) = self.tok(op) {
                if let Ok(inner) = self.parse_step(level) {
                    return Ok(self.ast.add_op(
                        Op {
                            op,
                            args: smallvec![inner],
                        },
                        loc,
                    ));
                }
            }
        }
        if rule.is_infix {
            todo!("infix");
        }
        if let Some(lit) = rule.lit {
            if let Ok(token) = self.tok(op) {}
        }
        self.parse_step(level + 1) // More specific
    }

    pub fn parse(&mut self) -> Result<(), ParseError> {
        // Dynamic recursive descent
        // TODO: Data stack for debugging.
        while !self.head.tokens.is_empty() {
            let root = self.parse_step(0)?;

            // Add all the valid trees that are not subtrees as roots.
            self.ast.roots.push(root);
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
            tokens: VecDeque::from_iter(tokens.into_iter().copied()),
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
