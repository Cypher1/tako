use crate::ast::location::{IndexIntoFile, Location, SymbolLength};
use crate::error::TError;
use crate::parser::semantics::BindingMode;
use better_std::{assert_eq, todo, *};
use log::{debug, trace};
use std::collections::{HashMap, HashSet};
use std::fmt;
use strum_macros::EnumIter;

use static_assertions::assert_eq_size;
assert_eq_size!(Symbol, [u8; 1]);
assert_eq_size!([Symbol; 2], [u8; 2]);
assert_eq_size!(TokenType, [u8; 1]);
assert_eq_size!(IndexIntoFile, [u8; 2]);
assert_eq_size!(SymbolLength, [u8; 1]);
assert_eq_size!(Token, [u8; 4]);
assert_eq_size!([Token; 2], [u8; 8]);

#[derive(Copy, Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum OpBinding {
    PostfixOp,
    PrefixOp,
    PrefixOrInfixBinOp,
    InfixOrPostfixBinOp,
    InfixBinOp,
    Open(Symbol),  // the associated Closer
    Close(Symbol), // the associated Opener
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash, EnumIter)]
pub enum Symbol {
    // Ignore symbols
    Shebang,
    Comment,
    Hash,
    MultiCommentOpen,
    MultiCommentClose,
    // Closes
    CloseBracket,
    CloseCurly,
    CloseParen,
    // Opens
    OpenParen,
    OpenCurly,
    OpenBracket,
    // Sequences/
    Sequence,
    // Assignments // TODO: Put ops separately if they shouldn't be in the AST.
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
    // Pipes...
    LeftPipe,
    RightPipe,

    // Functions,
    Sigma,
    Lambda, // For compatibility with other systems (ignored).
    Arrow,
    DoubleArrow, // In case value level and type level must be different.
    // Quantification (type level)
    Forall,
    // Sugar for forall.
    Pi,     // For compatibility with other systems.
    Exists, // Sigma

    // Comparisons
    Eqs,
    NotEqs,
    Lt,
    LtEqs,
    Gt,
    GtEqs,

    // Maths
    LeftShift,
    RightShift,
    Add,
    Sub,
    Exp,
    Div,
    Mul,
    // Logical
    And,
    Or,
    BitNot,
    BitXor,
    LogicalNot,
    LogicalAnd,
    LogicalOr,
    Modulo,
    // Special...
    GetAddress,
    HasType,
    Try,
    Dot,
    Range,
    Spread,
    Escape,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum CharacterType {
    AtomHead,                // '$' on it's own (invalid).
    Whitespace,              // A special discardable token representing whitespace.
    HexSym, // A subset of symbol characters that can be used in Hex strings (e.g. Colors).
    PartialToken(TokenType), // Already a valid token!
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
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
    // TODO: Group,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Op(sym) => write!(f, "a '{sym:?}' symbol"),
            Self::Comma => write!(f, "a comma"),
            Self::Ident => write!(f, "an identifier"),
            Self::Atom => write!(f, "an atom"),
            Self::NumberLit => write!(f, "a number"),
            Self::ColorLit => write!(f, "a color"),
            Self::StringLit => write!(f, "a string literal"),
            Self::FmtStringLitStart => write!(f, "the start of a format string literal"),
            Self::FmtStringLitMid => write!(f, "the middle of a format string literal"),
            Self::FmtStringLitEnd => write!(f, "the end of a format string literal"),
            // Self::Group => write!(f, "a long string literal"),
        }
    }
}

#[derive(Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Token {
    pub kind: TokenType,
    // These are byte indexes and byte lengths. They may need to be interpreted before being shown
    // to the user.
    pub start: IndexIntoFile,
    pub length: SymbolLength,
}

impl Token {
    #[must_use]
    pub fn location(self) -> Location {
        Location {
            start: self.start,
            length: self.length,
        }
    }

    #[must_use]
    pub fn get_src<'a>(&self, source: &'a str) -> &'a str {
        // Assuming the token is from the source file...
        &source[self.start as usize..self.start as usize + self.length as usize]
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO(usability): Look up the token to get the contents?
        write!(
            f,
            "{:?}@{}..{}",
            self.kind,
            self.start,
            self.start + u16::from(self.length)
        )
    }
}

const _COMMENT: &str = "//";
const _MULTI_COMMENT: &str = "/*";

#[inline]
fn classify_char(ch: char) -> CharacterType {
    use CharacterType::{AtomHead, HexSym, PartialToken, Whitespace};
    use TokenType::{Comma, Ident, NumberLit, Op, StringLit};
    PartialToken(match ch {
        '\n' | '\r' | '\t' | ' ' => return Whitespace,
        '$' => return AtomHead,
        'A'..='F' | 'a'..='f' => return HexSym,
        ',' => Comma,
        '#' => Op(Symbol::Hash),
        '~' => Op(Symbol::BitNot),
        '!' => Op(Symbol::LogicalNot),
        '@' => Op(Symbol::GetAddress),
        '%' => Op(Symbol::Modulo),
        '^' => Op(Symbol::BitXor),
        '&' => Op(Symbol::And),
        '*' => Op(Symbol::Mul),
        '-' => Op(Symbol::Sub),
        '+' => Op(Symbol::Add),
        '=' => Op(Symbol::Assign),
        '<' => Op(Symbol::Lt),
        '>' => Op(Symbol::Gt),
        '|' => Op(Symbol::Or),
        '/' => Op(Symbol::Div),
        '?' => Op(Symbol::Try),
        '.' => Op(Symbol::Dot),
        ':' => Op(Symbol::HasType),
        ';' => Op(Symbol::Sequence),
        '(' => Op(Symbol::OpenParen),
        ')' => Op(Symbol::CloseParen),
        '{' => Op(Symbol::OpenCurly),
        '}' => Op(Symbol::CloseCurly),
        '[' => Op(Symbol::OpenBracket),
        ']' => Op(Symbol::CloseBracket),
        '\\' => Op(Symbol::Escape), // Escape?
        'λ' => Op(Symbol::Lambda),
        'Π' => Op(Symbol::Pi),
        'Σ' => Op(Symbol::Sigma),
        '∀' => Op(Symbol::Forall),
        '∃' => Op(Symbol::Exists),
        '0'..='9' => NumberLit,
        'A'..='Z' | 'a'..='z' | '_' => Ident, // Overlapped by colors.
        '"' | '\'' => StringLit,
        _ => panic!("Unknown token character {ch}"),
    })
}

#[inline]
#[must_use]
pub const fn op_from_assign_op(s: Symbol) -> Option<Symbol> {
    // TODO(clarity): Move to a symbol module.
    Some(match s {
        Symbol::AddAssign => Symbol::Add,
        Symbol::SubAssign => Symbol::Sub,
        Symbol::DivAssign => Symbol::Div,
        Symbol::MulAssign => Symbol::Mul,
        Symbol::AndAssign => Symbol::And,
        Symbol::OrAssign => Symbol::Or,
        Symbol::BitXorAssign => Symbol::BitXor,
        Symbol::LogicalAndAssign => Symbol::LogicalAnd,
        Symbol::LogicalOrAssign => Symbol::LogicalOr,
        Symbol::ModuloAssign => Symbol::Modulo,
        _ => return None,
    })
}

#[inline]
#[must_use]
pub const fn binding_mode_from_op(s: Symbol) -> Option<BindingMode> {
    // TODO(clarity): Move to a symbol module.
    Some(match s {
        Symbol::Lambda => BindingMode::Given,
        Symbol::Pi => BindingMode::Forall,
        Symbol::Forall => BindingMode::Forall,
        Symbol::Exists => BindingMode::With,
        Symbol::Sigma => BindingMode::With,
        _ => return None,
    })
}

#[inline]
#[must_use]
pub const fn is_assign(s: Symbol) -> bool {
    // TODO(clarity): Move to a symbol module.
    matches!(s, Symbol::Assign) || op_from_assign_op(s).is_some()
}

#[inline]
fn is_whitespace(chr: char) -> bool {
    classify_char(chr) == CharacterType::Whitespace
}

impl FromStr for Symbol {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use Symbol::*;
        let t = match s {
            "#" => Hash,
            "#!" => Shebang,
            "//" => Comment,
            "/*" => MultiCommentOpen,
            "*/" => MultiCommentClose,
            // Basics
            "+" => Add,
            "-" => Sub,
            "/" => Div,
            "*" => Mul,
            "**" => Exp,
            "!" => LogicalNot,
            "~" => BitNot,
            "&" => And,
            "^" => BitXor,
            "|" => Or,
            "&&" => LogicalAnd,
            "||" => LogicalOr,
            "%" => Modulo,
            "@" => GetAddress,
            ":" => HasType,
            "?" => Try,
            "." => Dot,
            ".." => Range,
            "..." => Spread,
            ";" => Sequence,
            "->" => Arrow,
            "=>" => DoubleArrow,
            "<<" => LeftShift,
            ">>" => RightShift,
            "<|" => LeftPipe,
            "|>" => RightPipe,
            // Assignment versions
            "=" => Assign,
            "+=" => AddAssign,
            "-=" => SubAssign,
            "/=" => DivAssign,
            "*=" => MulAssign,
            "&=" => AndAssign,
            "|=" => OrAssign,
            "^=" => BitXorAssign,
            "&&=" => LogicalAndAssign,
            "||=" => LogicalOrAssign,
            "%=" => ModuloAssign,
            // Quantification
            "λ" => Lambda,
            "Σ" => Sigma,
            "Π" => Pi,
            "∀" => Forall,
            "∃" => Exists,
            // Comparisons
            "==" => Eqs,
            "!=" => NotEqs,
            "<" => Lt,
            ">" => Gt,
            "<=" => LtEqs,
            ">=" => GtEqs,
            "(" => OpenParen,
            ")" => CloseParen,
            "{" => OpenCurly,
            "}" => CloseCurly,
            "[" => OpenBracket,
            "]" => CloseBracket,
            _ => return Err(()),
        };
        Ok(t)
    }
}

impl TryFrom<&str> for Symbol {
    type Error = ();

    fn try_from(s: &str) -> Result<Symbol, Self::Error> {
        Symbol::from_str(s)
    }
}

impl Into<&str> for &Symbol {
    fn into(self) -> &'static str {
        use Symbol::*;
        match self {
            Hash => "#",
            Shebang => "#!",
            Comment => "//",
            MultiCommentOpen => "/*",
            MultiCommentClose => "*/",
            // Basics
            Add => "+",
            Sub => "-",
            Div => "/",
            Mul => "*",
            Exp => "**",
            LogicalNot => "!",
            BitNot => "~",
            And => "&",
            BitXor => "^",
            Or => "|",
            LogicalAnd => "&&",
            LogicalOr => "||",
            Modulo => "%",
            GetAddress => "@",
            HasType => ":",
            Try => "?",
            Dot => ".",
            Range => "..",
            Spread => "...",
            Sequence => ";",
            Arrow => "->",
            DoubleArrow => "=>",
            LeftShift => "<<",
            RightShift => ">>",
            LeftPipe => "<|",
            RightPipe => "|>",
            // Assignment versions
            Assign => "=",
            AddAssign => "+=",
            SubAssign => "-=",
            DivAssign => "/=",
            MulAssign => "*=",
            AndAssign => "&=",
            OrAssign => "|=",
            BitXorAssign => "^=",
            LogicalAndAssign => "&&=",
            LogicalOrAssign => "||=",
            ModuloAssign => "%=",
            // Quantification
            Lambda => "λ",
            Sigma => "Σ",
            Pi => "Π",
            Forall => "∀",
            Exists => "∃",
            // Comparisons
            Eqs => "==",
            NotEqs => "!=",
            Lt => "<",
            Gt => ">",
            LtEqs => "<=",
            GtEqs => ">=",
            OpenParen => "(",
            CloseParen => ")",
            OpenCurly => "{",
            CloseCurly => "}",
            OpenBracket => "[",
            CloseBracket => "]",
        }
    }
}
impl Into<&str> for Symbol {
    fn into(self) -> &'static str {
        (&self).into()
    }
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", Into::<&str>::into(self))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn round_trip_to_str_try_from() {
        for ch in Symbol::iter() {
            assert_eq!(Symbol::try_from(Into::<&str>::into(ch)), Ok(ch));
        }
    }
    // TODO: Test error path
}
