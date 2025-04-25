use std::path::Path;
use std::ops::Range;
use better_std::include_strs;
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

use semantics::{ ASSOCIATIVE, RIGHT_ASSOCIATIVE };

pub const KEYWORDS: &[&str] = include_strs!("keywords.txt");

impl Symbol {
    #[must_use]
    pub fn is_associative(&self) -> bool {
        ASSOCIATIVE.contains(self)
    }

    #[must_use]
    pub fn is_right_associative(&self) -> bool {
        RIGHT_ASSOCIATIVE.contains(self)
    }

    #[must_use]
    pub fn is_left_associative(&self) -> bool {
        !(self.is_associative() || self.is_right_associative())
    }

    #[must_use]
    pub fn binding_type(&self) -> OpBinding {
        match self {
            Self::Escape
            | Self::BitNot
            | Self::LogicalNot
            | Self::GetAddress
            | Self::Spread
            | Self::Lambda
            | Self::Sigma
            | Self::Forall
            | Self::Pi
            | Self::Exists => OpBinding::PrefixOp,
            Self::Try => OpBinding::PostfixOp,
            Self::Sub => OpBinding::PrefixOrInfixBinOp,
            Self::CloseCurly => OpBinding::Close(Self::OpenCurly),
            Self::CloseParen => OpBinding::Close(Self::OpenParen),
            Self::CloseBracket => OpBinding::Close(Self::CloseParen),
            Self::OpenCurly => OpBinding::Open(Self::CloseCurly),
            Self::OpenParen => OpBinding::Open(Self::CloseParen),
            Self::OpenBracket => OpBinding::Open(Self::CloseBracket),
            Self::Sequence => OpBinding::InfixOrPostfixBinOp,
            _ => OpBinding::InfixBinOp,
        }
    }

    #[must_use]
    pub fn is_looser(&self, other: Self) -> bool {
        if *self == other {
            return self.is_right_associative();
        }
        LOOSER_THAN.contains(&(*self, other))
    }
}

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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ParseError {
    UnexpectedEof, // TODO: Add context.
    UnexpectedTokenTypeExpectedOperator {
        got: TokenType,
        location: Location,
    },
    UnexpectedTokenTypeExpectedAssignment {
        got: TokenType,
        location: Location,
    },
    UnexpectedTokenType {
        got: TokenType,
        location: Location,
        expected: TokenType,
    },
    UnexpectedTokenTypeInExpression {
        got: TokenType,
        location: Location,
    },
    ParseIntError {
        message: String,
        location: Option<Location>,
    },
    AmbiguousExpression {
        left: Symbol,
        right: Symbol,
        location: Location,
    },
    UnexpectedExpressionInDefinitionArguments {
        arg: NodeId,
        arg_str: String,
        location: Location,
    },
    MissingLeftHandSideOfOperator {
        op: Symbol,
        bind_type: OpBinding,
        location: Location,
    },
    MissingRightHandSideOfOperator {
        op: Symbol,
        bind_type: OpBinding,
        location: Location,
    },
    UnparsedTokens {
        token: TokenType,
        location: Location,
    },
}

impl ParseError {
    pub fn location(&self) -> Option<&Location> {
        match self {
            Self::UnexpectedEof => None,
            Self::ParseIntError { location, .. } => location.as_ref(),
            Self::UnexpectedTokenTypeExpectedOperator { got: _, location }
            | Self::UnexpectedTokenTypeExpectedAssignment { got: _, location }
            | Self::UnexpectedTokenType {
                got: _,
                location,
                expected: _,
            }
            | Self::UnexpectedTokenTypeInExpression { got: _, location }
            | Self::AmbiguousExpression {
                left: _,
                right: _,
                location,
            }
            | Self::UnexpectedExpressionInDefinitionArguments { location, .. }
            | Self::MissingLeftHandSideOfOperator { location, .. } => Some(location),
            Self::MissingRightHandSideOfOperator { location, .. } => Some(location),
            Self::UnparsedTokens { location, .. } => Some(location),
        }
    }
}
impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

/*
fn token(s: String) -> TokenType {
    if let Ok(op) = Symbol::try_into(&*s) {
        TokenType::Op(s)
    }
    // TODO: Other types of token
    Unknown
}

// A parser that turns pythonic code with semantic whitespace into a token tree
fn lexer<'a>() -> impl Parser<'a, str, Vec<Spanned<Token>>> {
    let tt = recursive(|tt| {
        // Define some atomic tokens
        let int = text::int::<'a, &str, _>(10)
            .from_str()
            .unwrapped()
            .map(Token::Int);
        let ident = text::ascii::ident().map(|s: &str| Token::Ident(s.to_string()));

        let simple_op = one_of("=.:%,");
        let delims = one_of("{}()[]");
        let op = simple_op.or(delims)
            .repeated()
            .at_least(1)
            .collect()
            .map(token);

        let single_token = int.or(op).or(ident);
        single_token.map_with_span(|tt, span| (tt, span))
    });

    // Whitespace indentation creates code block token trees
    text::semantic_indentation(tt, |tts, span| (TokenTree::Tree(Delim::Block, tts), span))
}

/// Flatten a series of token trees into a single token stream, ready for feeding into the main parser
fn tts_to_stream(
    eoi: Span,
    token_trees: Vec<Spanned<TokenTree>>,
) -> BoxStream<'static, Token, Span> {
    use std::iter::once;

    BoxStream::from_nested(eoi, token_trees.into_iter(), |(tt, span)| match tt {
        // Single tokens remain unchanged
        TokenTree::Token(token) => Flat::Single((token, span)),
        // Nested token trees get flattened into their inner contents, surrounded by `Open` and `Close` tokens
        TokenTree::Tree(delim, tree) => Flat::Many(
            once((TokenTree::Token(Token::Open(delim)), span.clone()))
                .chain(tree.into_iter())
                .chain(once((TokenTree::Token(Token::Close(delim)), span))),
        ),
    })
}

fn parse_file() -> Result<(), TError> {
    // First, lex the code into some nested token trees
    let tts = lexer().parse(code).into_output().unwrap();

    println!("--- Token Trees ---\n{:#?}", tts);

    // Next, flatten
    let eoi = 0..code.chars().count();
    let mut token_stream = tts_to_stream(eoi, tts);

    // At this point, we have a token stream that can be fed into the main parser! Because this is just an example,
    // we're instead going to just collect the token stream into a vector and print it.

    let flattened_trees = token_stream.fetch_tokens().collect::<Vec<_>>();

    println!("--- Flattened Token Trees ---\n{:?}", flattened_trees);
}*/

pub fn parse(file: &Path, input: &str, tokens: &Vec<Token>) -> Result<Ast, TError> {
    let mut ast = Ast::new(file.to_path_buf());

    let res = parse_file(file, input, tokens, &mut ast)?;

    // TODO: Handle errors
    println!("Result: {:?}", res);
    // Done converting to ast
    ast.roots.push(res);
    Ok(ast)
}

// TODO: Recover tests from ./old_mod.rs
