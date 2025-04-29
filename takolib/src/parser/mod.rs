use better_std::include_strs;
use chumsky::extra::Full;
use chumsky::extra::SimpleState;
use chumsky::pratt::*;
use chumsky::prelude::*;
use semantics::op_from_assign_op;
use semantics::Literal;
use smallvec::smallvec;
use smallvec::SmallVec;
use std::path::Path;

use crate::ast::location::Location;
use crate::ast::NodeId;
use crate::ast::Op;
use crate::{ast::Ast, error::TError};

pub mod error;
pub mod lexer;
pub mod semantics;
pub mod tokens;

use tokens::{Symbol, Token, TokenType};

pub type Span = SimpleSpan;
pub type Spanned<T> = (T, Span);

pub const KEYWORDS: &[&str] = include_strs!("keywords.txt");

type ParserConfig<'src> = Full<Rich<'src, Token>, SimpleState<Ast>, ()>;

use super::ast::nodes::FMT_STR_STANDARD_ITEM_NUM;

#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum OpKind {
    Infix,
    Prefix,
    Postfix,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct OpSetup {
    op: &'static [Symbol],
    kind: OpKind,
    contains: &'static [Symbol],
}

impl OpSetup {
    const fn new(op: &'static [Symbol], kind: OpKind, contains: &'static [Symbol]) -> Self {
        Self { op, kind, contains }
    }
}

// In precedence order least tightly binding to most.
const OPS: &[OpSetup] = {
    use OpKind::*;
    use Symbol::*;

    // TODO: Use
    const INSPO: &str = include_str!("../../../tree-sitter-tako/grammar.js");
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

    &[
        OpSetup::new(COMPARISONS, Infix, ANY_VALUE),
        OpSetup::new(ASSIGN, Infix, ANY_VALUE),
    ]
};

fn language<'src, 'ast>() -> impl Parser<'src, &'src [Token], (), ParserConfig<'src>> {
    use TokenType::*;

    // Tokens
    let simple_value = select_ref! {
        Token {kind: Ident, start, length } = extra => {
            let ast: &mut Ast = std::ops::DerefMut::deref_mut(extra.state());
            // TODO: Get the str from the Token
            let s = &ast.contents[(*start as usize)..(start+(*length as u16)) as usize];
            let t = ast.string_interner.register_str(s);
            ast.add_identifier(t, Location { start: *start, length: *length })
        },
        Token {kind: NumberLit, start, length } = extra => {
            let ast: &mut Ast = std::ops::DerefMut::deref_mut(extra.state());
            // TODO: Get the str from the Token
            let s = &ast.contents[(*start as usize)..(start+(*length as u16)) as usize];
            let _ = ast.string_interner.register_str_by_loc(s, *start);
            ast.add_literal(Literal::Numeric, Location { start: *start, length: *length })
        },
        Token {kind: ColorLit, start, length } = extra => {
            let ast: &mut Ast = std::ops::DerefMut::deref_mut(extra.state());
            // TODO: Get the str from the Token
            let s = &ast.contents[(*start as usize)..(start+(*length as u16)) as usize];
            let _ = ast.string_interner.register_str_by_loc(s, *start);
            ast.add_literal(Literal::Color, Location { start: *start, length: *length })
        },
        Token {kind: StringLit, start, length } = extra => {
            let ast: &mut Ast = std::ops::DerefMut::deref_mut(extra.state());
            // TODO: Get the str from the Token
            let s = &ast.contents[(*start as usize)..(start+(*length as u16)) as usize];
            let _ = ast.string_interner.register_str_by_loc(s, *start);
            ast.add_literal(Literal::String, Location { start: *start, length: *length })
        },
    }
    .labelled("an identifier or literal");

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
    };

    let op = |op| {
        select_ref! {
            Token {kind: OpType(top), start, length } if *top == op => Location { start: *start, length: *length }
        }.labelled(format!("{}", op))
    };

    let comma = select_ref! {
        Token {kind: Comma, start: _, length: _ } => ()
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
            |mut nodes: SmallVec<NodeId, FMT_STR_STANDARD_ITEM_NUM>,
             (fstr, node): (NodeId, NodeId)| {
                nodes.push(fstr);
                nodes.push(node);
                nodes
            },
        );
        let fmt_str = fmt_str_body
            .then(fmt_str_end)
            .map_with(|(mut nodes, fstr), extra| {
                nodes.push(fstr);
                let ast: &mut Ast = std::ops::DerefMut::deref_mut(extra.state());
                let loc = ast[fstr].location;
                // TODO: Switch from op to call with built ins?
                ast.add_op(
                    Op {
                        op: Symbol::Group,
                        args: nodes,
                    },
                    loc,
                )
            })
            .labelled("format string");

        // Single value
        let parens = expr
            .clone()
            .delimited_by(op(Symbol::OpenParen), op(Symbol::CloseParen))
            .labelled("parenthesized expression");
        // TODO:
        let many_head = expr.clone().map(|node| {
            let mut nodes: SmallVec<NodeId, FMT_STR_STANDARD_ITEM_NUM> = SmallVec::new();
            nodes.push(node);
            nodes
        });
        let many = many_head
            .foldl(
                expr.clone().separated_by(comma).allow_trailing(),
                |mut nodes, node| {
                    nodes.push(node);
                    nodes
                },
            )
            .labelled("a set of values");

        let container = op(Symbol::OpenBracket)
            .then(many.clone())
            .then(op(Symbol::CloseBracket))
            .map_with(|((open, nodes), _close), extra| {
                let ast: &mut Ast = std::ops::DerefMut::deref_mut(extra.state());
                let loc = open; // TODO: Merge with _close
                                // TODO: Switch from op to call with built ins?
                ast.add_op(
                    Op {
                        op: Symbol::Group,
                        args: nodes,
                    },
                    loc,
                )
            })
            .labelled("an ordered set of values");

        let unordered_container = op(Symbol::OpenCurly)
            .then(many.clone())
            .then(op(Symbol::CloseCurly))
            .map_with(|((open, nodes), _close), extra| {
                let ast: &mut Ast = std::ops::DerefMut::deref_mut(extra.state());
                let loc = open; // TODO: Merge with _close
                                // TODO: Switch from op to call with built ins?
                ast.add_op(
                    Op {
                        op: Symbol::Group,
                        args: nodes,
                    },
                    loc,
                )
            })
            .labelled("an unordered set of values array");

        let untyped_atom = parens
            .or(container)
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

        /*
        For reference, some interesting ideas about partial ordering of precedences:
            - https://www.foonathan.net/2017/07/operator-precedence/
        */

        use Symbol::*;

        // TODO: Do these manually?
        let field_expr = atom.pratt((
            // TODO: left(4), Calls // a.b(x, y) | a.b[x, y] | a.b {x: y} => (a.b)<args>, a : b() => a : (b())
            infix_op(left(3), HasType), // @a : c => (@a) : c, a.b : c => (a.b) : c, a:b.c => a : (b.c)
            prefix_op(2, GetAddress),   // @a.b => @(a.b), @a.b() => @(a())
            infix_op(left(1), Dot),     // a.b
        ));

        let bit_expr = field_expr.clone().pratt((
            // TODO: No chaining
            prefix_op(3, BitNot),
            infix_op(left(3), BitXor), // TODO: Check
            infix_op(left(2), Or),
            infix_op(left(1), And),
        ));

        let lshift_expr = field_expr.clone().pratt((
            // TODO: Not chaining.
            infix_op(left(1), LeftShift),
        ));
        let rshift_expr = field_expr.clone().pratt((
            // TODO: Not chaining.
            infix_op(left(1), RightShift),
        ));

        let boolean_expr = field_expr.clone().pratt((
            prefix_op(3, LogicalNot),
            infix_op(left(2), LogicalOr),
            infix_op(left(1), LogicalAnd),
        ));

        let math_expr = field_expr.clone().pratt((
            prefix_op(5, Add),
            prefix_op(5, Sub),
            infix_op(left(4), Modulo),
            infix_op(right(3), Exp),
            infix_op(left(2), Div),
            infix_op(left(2), Mul),
            infix_op(left(1), Sub),
            infix_op(left(1), Add),
        ));

        let comparison = field_expr.clone().pratt((
            // TODO: Not chaining.
            infix_op(left(1), Eqs),
            infix_op(left(1), NotEqs),
            infix_op(left(1), Lt),
            infix_op(left(1), LtEqs),
            infix_op(left(1), Gt),
            infix_op(left(1), GtEqs),
        ));

        let range_expr = field_expr.clone().pratt((
            // TODO: Not chaining.
            infix_op(left(1), Range),
        ));

        // infix_op(left(13), Spread),
        // postfix_op(11, Try),
        // infix_op(left(9), DoubleArrow),
        // infix_op(left(8), Arrow),
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

        let expr = math_expr
            .clone()
            .or(callable)
            .or(boolean_expr)
            .or(comparison)
            .or(lshift_expr)
            .or(rshift_expr)
            .or(bit_expr)
            .or(range_expr)
            .boxed();

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

        assignment.or(expr)
    });

    let roots =
        expr.separated_by(op(Symbol::Sequence))
            .collect()
            .map_with(|roots: Vec<NodeId>, extra| {
                let ast: &mut Ast = std::ops::DerefMut::deref_mut(extra.state());
                for root in roots {
                    ast.roots.push(root);
                }
            });
    Box::new(roots)
}

pub fn parse(file: &Path, input: &str, tokens: &[Token]) -> Result<Ast, TError> {
    let lang = language(); // TODO: Avoid recreating.

    let mut state = SimpleState(
        Ast::new(file.to_path_buf(), input.to_string()), // TODO: Avoid copy here?
    );

    // TODO: Support for streams?
    lang.parse_with_state(tokens, &mut state).unwrap(); // TODO: Handle errors
    Ok(state.0)
}

// TODO: Recover tests from ./old_mod.rs
