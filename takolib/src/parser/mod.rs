use std::path::Path;

use lrlex::lrlex_mod;
use lrpar::lrpar_mod;
use tokens::{Symbol, Token};
// pub use lrpar::ParseError as UnderlyingParserError;

use crate::{ast::{location::Location, Ast, NodeId}, error::TError};

pub mod semantics;

// Using `lrlex_mod!` brings the lexer for `calc.l` into scope. By default the
// module name will be `calc_l` (i.e. the file name, minus any extensions,
// with a suffix of `_l`).
lrlex_mod!("tako.l");
// Using `lrpar_mod!` brings the parser for `tako.y` into scope. By default the
// module name will be `tako` (i.e. the file name, minus any extensions,
// with a suffix of `_y`).
lrpar_mod!("tako.y");

pub const KEYWORDS: &[&str] = &[]; // TODO: Recover from tako.l

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
            write!(f, "{self:?}")
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
        todo!()
    }
}

pub fn parse(_file: &Path, _s: &str, _tokens: &[Token]) -> Result<Ast, TError> {
    todo!()
}

#[test]
fn main() {
    // Get the `LexerDef` for the `tako` language.
    let lexerdef = tako_l::lexerdef();
    let input = "2 + 3
2 + 3 * 4
(2 + 3) * 4
";
    for l in input.lines() {
        println!(">>> {l}");
        if l.trim().is_empty() {
            continue;
        }
        // Now we create a lexer with the `lexer` method with which
        // we can lex an input.
        let lexer = lexerdef.lexer(l);
        // Pass the lexer to the parser and lex and parse the input.
        let (res, errs) = tako_y::parse(&lexer);
        for e in errs {
            println!("{}", e.pp(&lexer, &tako_y::token_epp));
        }
        match res {
            Some(Ok(r)) => println!("Result: {:?}", r),
            Some(e) => eprintln!("Unable to evaluate expression.\n{e:?}"),
            None => eprintln!("Unable to evaluate expression."),
        }
    }
    assert_eq!("WTF", "");
}
