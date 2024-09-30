use std::path::Path;
use log::error;

use tree_sitter::{Tree, Parser as TSParser};

use tokens::{Symbol, Token};

use crate::{ast::{location::Location, Ast, NodeId}, error::TError};

pub mod semantics;

pub const KEYWORDS: &[&str] = &[]; // TODO: Recover from tako.l

// TODO: REMOVE
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenType {
    Op(Symbol), // An operator (i.e. a known symbol used as a prefix or infix operator).
    Comma,      // A regular comma.
    Ident,      // A named value.
    Atom,       // A symbol starting with a '$', used differently to symbols which have values.
                // Literals (i.e. tokens representing values):
    NumberLit,
    ColorLit,
    // Short strings can be stored as symbols.
    StringLit,
    // Format string parts:
    FmtStringLitStart,
    FmtStringLitMid,
    FmtStringLitEnd,
    // If a symbol (normally strings) is too long, we will store it as multiple repeated tokens,
    // of the same kind preceeded by a 'Group' token.
    Group,
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

// TODO: REMOVE
pub mod tokens {
    use crate::error::TError;

    #[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
    pub enum Symbol {

        MultiCommentOpen,
        MultiCommentClose,
        Comment,

        OpenParen,
        CloseParen,
        OpenCurly,
        CloseCurly,
        OpenBracket,
        CloseBracket,

        Eqs,
        NotEqs,
        Lt,
        Gt,
        LtEqs,
        GtEqs,

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
        Assign,

        LogicalNot,
        Shebang,
        Hash,
        Modulo,
        And,
        LogicalAnd,
        Mul,
        Exp,
        Add,
        Sub,
        Arrow,
        Dot,
        Range,
        Spread,
        Div,
        HasType,
        Sequence,
        LeftShift,
        LeftPipe,
        DoubleArrow,
        RightShift,
        Try,
        GetAddress,
        BitXor,
        Or,
        RightPipe,
        LogicalOr,
        BitNot,

        Lambda,
        Pi,
        Sigma,
        Forall,
        Exists,
    }

    impl std::fmt::Display for Symbol {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(
                f,
                "{}",
                match self {
                    Self::Hash => "#",
                    Self::Shebang => "#!",
                    Self::Comment => "//",
                    Self::MultiCommentOpen => "/*",
                    Self::MultiCommentClose => "*/",
                    // Basics
                    Self::Add => "+",
                    Self::Sub => "-",
                    Self::Div => "/",
                    Self::Mul => "*",
                    Self::Exp => "**",
                    Self::LogicalNot => "!",
                    Self::BitNot => "~",
                    Self::And => "&",
                    Self::BitXor => "^",
                    Self::Or => "|",
                    Self::LogicalAnd => "&&",
                    Self::LogicalOr => "||",
                    Self::Modulo => "%",
                    Self::GetAddress => "@",
                    Self::HasType => ":",
                    Self::Try => "?",
                    Self::Dot => ".",
                    Self::Range => "..",
                    Self::Spread => "...",
                    Self::Sequence => ";",
                    Self::Arrow => "->",
                    Self::DoubleArrow => "=>",
                    Self::LeftShift => "<<",
                    Self::RightShift => ">>",
                    Self::LeftPipe => "<|",
                    Self::RightPipe => "|>",
                    // Assignment versions
                    Self::Assign => "=",
                    Self::AddAssign => "+=",
                    Self::SubAssign => "-=",
                    Self::DivAssign => "/=",
                    Self::MulAssign => "*=",
                    Self::AndAssign => "&=",
                    Self::OrAssign => "|=",
                    Self::BitXorAssign => "^=",
                    Self::LogicalAndAssign => "&&=",
                    Self::LogicalOrAssign => "||=",
                    Self::ModuloAssign => "%=",
                    // Quantification
                    Self::Lambda => "λ",
                    Self::Sigma => "Σ",
                    Self::Pi => "Π",
                    Self::Forall => "∀",
                    Self::Exists => "∃",
                    // Comparisons
                    Self::Eqs => "==",
                    Self::NotEqs => "!=",
                    Self::Lt => "<",
                    Self::Gt => ">",
                    Self::LtEqs => "<=",
                    Self::GtEqs => ">=",
                    Self::OpenParen => "(",
                    Self::CloseParen => ")",
                    Self::OpenCurly => "{",
                   Self::CloseCurly => "}",
                    Self::OpenBracket => "[",
                    Self::CloseBracket => "]",
                }
            )
        }
    }
    #[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
    pub enum Token {
    }
    impl std::fmt::Display for Token {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{self:?}")
        }
    }

    pub fn lex(_s: &str) -> Result<Vec<Token>, TError> {
        // TODO: Remove
        Ok(vec![])
    }
}

pub fn parse(file: &Path, input: &str, _tokens: &[Token]) -> Result<Ast, TError> {
    let mut ast = Ast::new(file.to_path_buf());

    // TODO: Put parser in a state to get caching
    // TODO: Set logger.
    let mut parser = TSParser::new();
    parser
        .set_language(&tree_sitter_tako::LANGUAGE.into())
        .expect("Error loading Tako parser");

    let old_tree: Option<&Tree> = None;
    let Some(res) = parser.parse(input.as_bytes(), old_tree) else {
        error!("Unknown parser error");
        panic!("Unknown parser error");
    };
    // TODO: Handle errors
    println!("Result: {:?}", res);
    // ast.roots.push(res);
    Ok(ast)
}

// TODO: Recover tests from ./old_mod.rs
