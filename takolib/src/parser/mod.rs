use std::path::Path;
use better_std::include_strs;
use chumsky::extra::Full;
use chumsky::extra::SimpleState;
use chumsky::prelude::*;
use chumsky::pratt::*;
use semantics::Literal;
use smallvec::smallvec;
use smallvec::SmallVec;

use crate::ast::location::Location;
use crate::ast::NodeId;
use crate::ast::Op;
use crate::{
    ast::Ast,
    error::TError,
};

pub mod tokens;
pub mod semantics;
pub mod lexer;
pub mod error;

use tokens::{Symbol, Token, TokenType};

pub type Span = SimpleSpan;
pub type Spanned<T> = (T, Span);

pub const KEYWORDS: &[&str] = include_strs!("keywords.txt");

type ParserConfig<'src, 'ast> = Full<Rich<'src, Token>, SimpleState<Ast>, ()>;

use super::ast::nodes::FMT_STR_STANDARD_ITEM_NUM;

// TODO: Remove this indirection
type Par<'src, 'ast> = chumsky::Boxed<'src, 'ast, &'src[Token], NodeId, ParserConfig<'src, 'ast>>;

fn language<'src: 'ast, 'ast>() -> impl Parser<'src, &'src[Token], (), ParserConfig<'src, 'ast>> {
    use TokenType::*;

    // TODO: Compute once.
    // use lazy_static::lazy_static;
    // lazy_static! {
    // static ref RIGHT_ASSOCIATIVE: HashSet<Symbol> = hash_set!{

    // Tokens
    let simple_value: Par<'src, 'ast> = select_ref! {
        Token {kind: Ident, start, length } = extra => {
            let ast: &mut Ast = std::ops::DerefMut::deref_mut(extra.state());
            let t = ast.string_interner.register_str(&ast.contents);
            ast.add_identifier(t, Location { start: *start, length: *length })
        },
        Token {kind: NumberLit, start, length } = extra => {
            let ast: &mut Ast = std::ops::DerefMut::deref_mut(extra.state());
            let _ = ast.string_interner.register_str_by_loc(&ast.contents, *start);
            ast.add_literal(Literal::Numeric, Location { start: *start, length: *length })
        },
        Token {kind: ColorLit, start, length } = extra => {
            let ast: &mut Ast = std::ops::DerefMut::deref_mut(extra.state());
            let _ = ast.string_interner.register_str_by_loc(&ast.contents, *start);
            ast.add_literal(Literal::Color, Location { start: *start, length: *length })
        },
        Token {kind: StringLit, start, length } = extra => {
            let ast: &mut Ast = std::ops::DerefMut::deref_mut(extra.state());
            let _ = ast.string_interner.register_str_by_loc(&ast.contents, *start);
            ast.add_literal(Literal::String, Location { start: *start, length: *length })
        },
    }.boxed();

    // Special Tokens
    let fmt_str_start = select_ref! {
        Token {kind: FmtStringLitStart, start, length } = extra => {
            let ast: &mut Ast = std::ops::DerefMut::deref_mut(extra.state());
            ast.add_literal(Literal::String, Location { start: *start, length: *length })
        },
    };
    let fmt_str_mid = select_ref! {
        Token {kind: FmtStringLitMid, start, length } = extra => {
            let ast: &mut Ast = std::ops::DerefMut::deref_mut(extra.state());
            ast.add_literal(Literal::String, Location { start: *start, length: *length })
        },
    };
    let fmt_str_end = select_ref! {
        Token {kind: FmtStringLitEnd, start, length } = extra => {
            let ast: &mut Ast = std::ops::DerefMut::deref_mut(extra.state());
            ast.add_literal(Literal::String, Location { start: *start, length: *length })
        },
    }.boxed();

    let op = |op| select_ref! {
        Token {kind: OpType(op), start, length } => Location { start: *start, length: *length }
    };

    let comma = select_ref! {
        Token {kind: Comma, start, length } => ()
    };

    // Expressions
    let expr = recursive(|expr| {
        let fmt_str_body_head = fmt_str_start.then(expr.clone()).map(|(start, node)| {
            let mut nodes: SmallVec<NodeId, FMT_STR_STANDARD_ITEM_NUM> = SmallVec::new();
            nodes.push(start);
            nodes.push(node);
            nodes
        });
        let fmt_str_body = fmt_str_body_head.foldl(
            fmt_str_mid.then(expr.clone()).repeated(),
            |mut nodes: SmallVec<NodeId, FMT_STR_STANDARD_ITEM_NUM>, (fstr, node): (NodeId, NodeId)| {
                nodes.push(fstr);
                nodes.push(node);
                nodes
            }
        );
        let fmt_str = fmt_str_body.then(fmt_str_end).map_with(|(mut nodes, fstr), extra| {
            nodes.push(fstr);
            let ast: &mut Ast = std::ops::DerefMut::deref_mut(extra.state());
            let loc = ast[fstr].location;
            // TODO: Switch from op to call with built ins?
            ast.add_op(Op { op: Symbol::Group, args: nodes }, loc)
        });

        // Single value
        let parens = expr.clone().delimited_by(op(Symbol::OpenParen), op(Symbol::CloseParen));
        // TODO:
        let container = expr.clone().separated_by(comma).delimited_by(op(Symbol::OpenBracket), op(Symbol::CloseBracket));
        let unordered_container = expr.clone().separated_by(comma).delimited_by(op(Symbol::OpenCurly), op(Symbol::CloseCurly));

        let atom = parens.or(simple_value).or(fmt_str);

        let prefix_op = |prec, op_type: Symbol| {
            prefix(prec, op(op_type), move |loc, rhs, extra| {
                let ast: &mut Ast = std::ops::DerefMut::deref_mut(extra.state());
                ast.add_op(Op { op: op_type, args: smallvec![rhs] }, loc)
            })
        };

        let infix_op = |prec, op_type: Symbol| {
            infix(prec, op(op_type), move |lhs, loc, rhs, extra| {
                let ast: &mut Ast = std::ops::DerefMut::deref_mut(extra.state());
                ast.add_op(Op { op: op_type, args: smallvec![lhs, rhs] }, loc)
            })
        };

        atom.pratt((
            // Just like in math, we want that if we write -x^2, our parser parses that as -(x^2), so we need it to have
            // exponents bind tighter than our prefix operators.
            infix_op(right(3), Symbol::Exp),
            infix_op(right(3), Symbol::BitXor),
            // Notice the conflict with our `Expr::Sub`. This will still parse correctly. We want negation to happen before
            // `+` and `-`, so we set its precedence higher.
            //prefix(2, op('-'), |_, rhs, _| Expr::Neg(Box::new(rhs))),
            prefix_op(2, Symbol::Add),
            prefix_op(2, Symbol::Sub),
            //prefix(2, op('*'), |_, rhs, _| Expr::Deref(Box::new(rhs))),
            prefix_op(2, Symbol::LogicalNot),
            infix_op(left(2), Symbol::Mul),
            infix_op(left(2), Symbol::Div),
            // Our `-` and `+` bind the weakest, meaning that even if they occur first in an expression, they will be the
            // last executed.
            infix_op(left(1), Symbol::Add),
            infix_op(left(1), Symbol::Sub),
        ))
    });

    let roots = expr.repeated().collect().map_with(|roots: Vec<NodeId>, extra|{
        let ast: &mut Ast = std::ops::DerefMut::deref_mut(extra.state());
        for root in roots {
            ast.roots.push(root);
        }
        ()
    });
    Box::new(roots)
}

pub fn parse(file: &Path, input: &str, tokens: &[Token]) -> Result<Ast, TError> {
    let lang = language(); // TODO: Avoid recreating.

    let mut state = SimpleState(
        Ast::new(file.to_path_buf(), input.to_string()) // TODO: Avoid copy here?
    );

    // TODO: Support for streams?
    lang.parse_with_state(tokens, &mut state).unwrap(); // TODO: Handle errors
    Ok(state.0)
}

// TODO: Recover tests from ./old_mod.rs
