use std::path::Path;
use better_std::include_strs;
use chumsky::input::Stream;
use chumsky::prelude::*;
use chumsky::pratt::*;
pub mod tokens;
use tokens::{Symbol, Token, TokenType};
use crate::ast::string_interner::StrId;
use crate::{
    ast::{location::Location, nodes::Op, Ast, Call, Definition, NodeId},
    error::TError,
};
pub mod semantics;
pub mod error;
use chumsky::prelude::*;
use chumsky::pratt::*;
use chumsky::extra;

pub const KEYWORDS: &[&str] = include_strs!("keywords.txt");

// TODO: REMOVE
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum OpBinding {
    PostfixOp,
    PrefixOp,
    PrefixOrInfixBinOp,
    InfixOrPostfixBinOp,
    InfixBinOp,
    Open(Symbol),  // the associated Closer
    Close(Symbol), // the associated Opener
}

enum Expr {
    Add(Box<Self>, Box<Self>),
    Sub(Box<Self>, Box<Self>),
    Pow(Box<Self>, Box<Self>),
    Neg(Box<Self>),
    Factorial(Box<Self>),
    Deref(Box<Self>),
    Literal(i32),
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Literal(literal) => write!(f, "{literal}"),
            Self::Factorial(left) => write!(f, "({left}!)"),
            Self::Deref(left) => write!(f, "(*{left})"),
            Self::Neg(right) => write!(f, "(-{right})"),
            Self::Add(left, right) => write!(f, "({left} + {right})"),
            Self::Sub(left, right) => write!(f, "({left} - {right})"),
            Self::Pow(left, right) => write!(f, "({left} ^ {right})"),
        }
    }
}

fn language() -> () {
    use TokenType::*;

    // TODO: Compute once.
    // use lazy_static::lazy_static;
    // lazy_static! {
    // static ref RIGHT_ASSOCIATIVE: HashSet<Symbol> = hash_set!{

    // Tokens
    let op = any().filter(|op| matches!(op, Op(_)));
    let comma = just(Comma);
    let ident = just(Ident);
    let atom = just(Atom);
    let number = just(NumberLit);
    let color = just(ColorLit);
    let str_lit = just(StringLit);
    let fmt_str_start = just(FmtStringLitStart);
    let fmt_str_mid = just(FmtStringLitMid);
    let fmt_str_end = just(FmtStringLitEnd);

    // Expressions
    let expr = recursive(|expr| {
        let fmt_str_body = expr.separated_by(fmt_str_mid).repeated();
        let fmt_str = fmt_str_start.then(fmt_str_body).fmt_start_end;

    });

    let expr = atom.pratt((
        // We want factorial to happen before any negation, so we need its precedence to be higher than `Expr::Neg`.
        postfix(4, op('!'), |lhs, _, _| Expr::Factorial(Box::new(lhs))),
        // Just like in math, we want that if we write -x^2, our parser parses that as -(x^2), so we need it to have
        // exponents bind tighter than our prefix operators.
        infix(right(3), op('^'), |l, _, r, _| Expr::Pow(Box::new(l), Box::new(r))),
        // Notice the conflict with our `Expr::Sub`. This will still parse correctly. We want negation to happen before
        // `+` and `-`, so we set its precedence higher.
        prefix(2, op('-'), |_, rhs, _| Expr::Neg(Box::new(rhs))),
        prefix(2, op('*'), |_, rhs, _| Expr::Deref(Box::new(rhs))),
        // Our `-` and `+` bind the weakest, meaning that even if they occur first in an expression, they will be the
        // last executed.
        infix(left(1), op('+'), |l, _, r, _| Expr::Add(Box::new(l), Box::new(r))),
        infix(left(1), op('-'), |l, _, r, _| Expr::Sub(Box::new(l), Box::new(r))),
    ))
        .map(|x| x.to_string());

    let roots = expr.repeated();

    assert_eq!(
        expr.parse("*1 + -2! - -3^2").into_result(),
        Ok("(((*1) + (-(2!))) - (-(3 ^ 2)))".to_string()),
    );
}

fn token(s: String) -> TokenType {
    if let Ok(op) = Symbol::try_into(&*s) {
        TokenType::Op(s)
    }
    // TODO: Other types of token
    Unknown
}

pub fn parse(file: &Path, input: &str, tokens: &[Token]) -> Result<Ast, TError> {
    // TODO: Support for streams?
    let mut ast = Ast::new(file.to_path_buf());

    let res = parse_file(file, input, tokens, &mut ast)?;

    // TODO: Handle errors
    println!("Result: {:?}", res);
    // Done converting to ast
    ast.roots.push(res);
    Ok(ast)
}

// TODO: Recover tests from ./old_mod.rs
