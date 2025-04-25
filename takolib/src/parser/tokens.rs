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
    // TODO: Remove for chumsky errors!
    PostfixOp,
    PrefixOp,
    PrefixOrInfixBinOp,
    InfixOrPostfixBinOp,
    InfixBinOp,
    Open(Symbol),  // the associated Closer
    Close(Symbol), // the associated Opener
}

/*
macro_rules! senum({ $s: expr } => {{
    const BYTES: &[u8] = ($s as &str).as_bytes();
    const CH_0: isize = BYTES[0] as isize;
    const CH_1: isize = BYTES[1] as isize;
    // TODO: Check bytes length
    const M: isize = CH_0 + 256 * CH_1;
    M
}});
*/

#[derive(Copy, Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash, EnumIter)]
pub enum Symbol {
    // Ignore symbols
    Shebang, // TODO: Consider using direct comparisons? = senum!("/*"),
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
    // If a symbol (normally strings) is too long, we will store it as multiple repeated tokens,
    // of the same kind preceeded by a 'Group' token.
    Group,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum CharacterType {
    Whitespace,              // A special discardable token representing whitespace.
    HexSym, // A subset of symbol characters that can be used in Hex strings (e.g. Colors).
    PartialToken(TokenType), // Already a valid token!
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum TokenType {
    OpType(Symbol), // An operator (i.e. a known symbol used as a prefix or infix operator).
    Comma,      // A regular comma.
    Ident,      // A named value.
    // Literals (i.e. tokens representing values):
    NumberLit,
    ColorLit,
    // Short strings can be stored as symbols.
    StringLit,
    // Format string parts:
    FmtStringLitStart,
    FmtStringLitMid,
    FmtStringLitEnd,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenType::OpType(sym) => write!(f, "a '{sym:?}' symbol"),
            TokenType::Comma => write!(f, "a comma"),
            TokenType::Ident => write!(f, "an identifier"),
            TokenType::NumberLit => write!(f, "a number"),
            TokenType::ColorLit => write!(f, "a color"),
            TokenType::StringLit => write!(f, "a string literal"),
            TokenType::FmtStringLitStart => write!(f, "the start of a format string literal"),
            TokenType::FmtStringLitMid => write!(f, "the middle of a format string literal"),
            TokenType::FmtStringLitEnd => write!(f, "the end of a format string literal"),
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
pub fn classify_char(ch: char) -> CharacterType {
    use CharacterType::{HexSym, PartialToken, Whitespace};
    use TokenType::{Comma, Ident, NumberLit, OpType, StringLit};
    use Symbol::*;
    PartialToken(match ch {
        '\n' | '\r' | '\t' | ' ' => return Whitespace,
        'A'..='F' | 'a'..='f' => return HexSym,
        ',' => Comma,
        '#' => OpType(Hash),
        '~' => OpType(BitNot),
        '!' => OpType(LogicalNot),
        '@' => OpType(GetAddress),
        '%' => OpType(Modulo),
        '^' => OpType(BitXor),
        '&' => OpType(And),
        '*' => OpType(Mul),
        '-' => OpType(Sub),
        '+' => OpType(Add),
        '=' => OpType(Assign),
        '<' => OpType(Lt),
        '>' => OpType(Gt),
        '|' => OpType(Or),
        '/' => OpType(Div),
        '?' => OpType(Try),
        '.' => OpType(Dot),
        ':' => OpType(HasType),
        ';' => OpType(Sequence),
        '(' => OpType(OpenParen),
        ')' => OpType(CloseParen),
        '{' => OpType(OpenCurly),
        '}' => OpType(CloseCurly),
        '[' => OpType(OpenBracket),
        ']' => OpType(CloseBracket),
        '\\' => OpType(Escape), // Escape?
        'λ' => OpType(Lambda),
        'Π' => OpType(Pi),
        'Σ' => OpType(Sigma),
        '∀' => OpType(Forall),
        '∃' => OpType(Exists),
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


impl std::str::FromStr for Symbol {
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
        use std::str::FromStr;
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
            Lambda => "λ",
            Sigma => "Σ",
            Pi => "Π",
            Forall => "∀",
            Exists => "∃",
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
            Escape => "\\",
            Group => " ", // Group is not a expressable character.
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
    use better_std::assert_eq;
    use strum::IntoEnumIterator;
    use CharacterType::*;
    use TokenType::*;
    use Symbol::*;

    #[test]
    fn round_trip_to_str_try_from() {
        for ch in Symbol::iter() {
            assert_eq!(Symbol::try_from(Into::<&str>::into(ch)), Ok(ch));
        }
        // TODO: Test error path
    }

    #[test]
    fn classify_whitespace() {
        assert_eq!(classify_char(' '), Whitespace);
        assert_eq!(classify_char('\n'), Whitespace);
        assert_eq!(classify_char('\r'), Whitespace);
    }

    #[test]
    fn classify_parens_and_brackets() {
        assert_eq!(classify_char('('), PartialToken(OpType(OpenParen)));
        assert_eq!(classify_char(')'), PartialToken(OpType(CloseParen)));
        assert_eq!(classify_char('['), PartialToken(OpType(OpenBracket)));
        assert_eq!(classify_char(']'), PartialToken(OpType(CloseBracket)));
        assert_eq!(classify_char('{'), PartialToken(OpType(OpenCurly)));
        assert_eq!(classify_char('}'), PartialToken(OpType(CloseCurly)));
    }

    #[test]
    fn classify_number() {
        assert_eq!(classify_char('0'), PartialToken(NumberLit));
        assert_eq!(classify_char('1'), PartialToken(NumberLit));
        assert_eq!(classify_char('2'), PartialToken(NumberLit));
    }

}
