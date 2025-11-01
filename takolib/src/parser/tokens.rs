use crate::ast::location::{IndexIntoFile, Location, SymbolLength};
use crate::parser::semantics::BindingMode;
use std::fmt;
use strum_macros::EnumIter;

use static_assertions::assert_eq_size;
assert_eq_size!(Symbol, [u8; 1]);
assert_eq_size!([Symbol; 2], [u8; 2]);
assert_eq_size!(IndexIntoFile, [u8; 2]);
assert_eq_size!(SymbolLength, [u8; 1]);
assert_eq_size!(Token, [u8; 4]);
assert_eq_size!([Token; 2], [u8; 8]);

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
#[repr(u8)]
pub enum Symbol {
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

    // Ignore symbols
    Group,
    Shebang, // TODO: Consider using direct comparisons? = senum!("/*"),
    Comment,
    Hash,
    MultiCommentOpen,
    MultiCommentClose,
    // TODO: ?
    Comma, // A regular comma.

    // Non operator Token types
    Ident, // A named value.
    // Literals (i.e. tokens representing values):
    NumberLit,
    ColorLit,
    // Short strings can be stored as symbols.
    StringLit,
    // Format string parts:
    FmtStringLitStart,
    FmtStringLitMid,
    FmtStringLitEnd,
    // TODO: Add semver.
    // TODO: Add headings: /====*[^='"]*====*\/,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum CharacterType {
    Whitespace,           // A special discardable token representing whitespace.
    HexSym, // A subset of symbol characters that can be used in Hex strings (e.g. Colors).
    PartialToken(Symbol), // Already a valid token!
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", Into::<&str>::into(self))
    }
}

#[derive(Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Token {
    pub kind: Symbol,
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
    use Symbol::*;
    use Symbol::{Ident, NumberLit, StringLit};
    PartialToken(match ch {
        '\n' | '\r' | '\t' | ' ' => return Whitespace,
        'A'..='F' | 'a'..='f' => return HexSym,
        ',' => Comma,
        '#' => Hash,
        '~' => BitNot,
        '!' => LogicalNot,
        '@' => GetAddress,
        '%' => Modulo,
        '^' => BitXor,
        '&' => And,
        '*' => Mul,
        '-' => Sub,
        '+' => Add,
        '=' => Assign,
        '<' => Lt,
        '>' => Gt,
        '|' => Or,
        '/' => Div,
        '?' => Try,
        '.' => Dot,
        ':' => HasType,
        ';' => Sequence,
        '(' => OpenParen,
        ')' => CloseParen,
        '{' => OpenCurly,
        '}' => CloseCurly,
        '[' => OpenBracket,
        ']' => CloseBracket,
        '\\' => Group, // Escape?
        'λ' => Lambda,
        'Π' => Pi,
        'Σ' => Sigma,
        '∀' => Forall,
        '∃' => Exists,
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
            "\\" => Group,
            "," => Comma,
            "an identifier" => Ident,
            "a number" => NumberLit,
            "a color" => ColorLit,
            "a string literal" => StringLit,
            "the start of a format string literal" => FmtStringLitStart,
            "the middle of a format string literal" => FmtStringLitMid,
            "the end of a format string literal" => FmtStringLitEnd,
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

impl From<&Symbol> for &str {
    fn from(o: &Symbol) -> &'static str {
        match o {
            Symbol::Ident => "an identifier",
            Symbol::NumberLit => "a number",
            Symbol::ColorLit => "a color",
            Symbol::StringLit => "a string literal",
            Symbol::FmtStringLitStart => "the start of a format string literal",
            Symbol::FmtStringLitMid => "the middle of a format string literal",
            Symbol::FmtStringLitEnd => "the end of a format string literal",
            Symbol::Comma => ",",
            Symbol::Hash => "#",
            Symbol::Shebang => "#!",
            Symbol::Comment => "//",
            Symbol::MultiCommentOpen => "/*",
            Symbol::MultiCommentClose => "*/",
            Symbol::Add => "+",
            Symbol::Sub => "-",
            Symbol::Div => "/",
            Symbol::Mul => "*",
            Symbol::Exp => "**",
            Symbol::LogicalNot => "!",
            Symbol::BitNot => "~",
            Symbol::And => "&",
            Symbol::BitXor => "^",
            Symbol::Or => "|",
            Symbol::LogicalAnd => "&&",
            Symbol::LogicalOr => "||",
            Symbol::Modulo => "%",
            Symbol::GetAddress => "@",
            Symbol::HasType => ":",
            Symbol::Try => "?",
            Symbol::Dot => ".",
            Symbol::Range => "..",
            Symbol::Spread => "...",
            Symbol::Sequence => ";",
            Symbol::Arrow => "->",
            Symbol::DoubleArrow => "=>",
            Symbol::LeftShift => "<<",
            Symbol::RightShift => ">>",
            Symbol::Assign => "=",
            Symbol::AddAssign => "+=",
            Symbol::SubAssign => "-=",
            Symbol::DivAssign => "/=",
            Symbol::MulAssign => "*=",
            Symbol::AndAssign => "&=",
            Symbol::OrAssign => "|=",
            Symbol::BitXorAssign => "^=",
            Symbol::LogicalAndAssign => "&&=",
            Symbol::LogicalOrAssign => "||=",
            Symbol::ModuloAssign => "%=",
            Symbol::Lambda => "λ",
            Symbol::Sigma => "Σ",
            Symbol::Pi => "Π",
            Symbol::Forall => "∀",
            Symbol::Exists => "∃",
            Symbol::Eqs => "==",
            Symbol::NotEqs => "!=",
            Symbol::Lt => "<",
            Symbol::Gt => ">",
            Symbol::LtEqs => "<=",
            Symbol::GtEqs => ">=",
            Symbol::OpenParen => "(",
            Symbol::CloseParen => ")",
            Symbol::OpenCurly => "{",
            Symbol::CloseCurly => "}",
            Symbol::OpenBracket => "[",
            Symbol::CloseBracket => "]",
            Symbol::Group => "\\",
        }
    }
}
impl From<Symbol> for &str {
    fn from(o: Symbol) -> &'static str {
        (&o).into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use better_std::assert_eq;
    use strum::IntoEnumIterator;
    use CharacterType::*;
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
        assert_eq!(classify_char('('), PartialToken(OpenParen));
        assert_eq!(classify_char(')'), PartialToken(CloseParen));
        assert_eq!(classify_char('['), PartialToken(OpenBracket));
        assert_eq!(classify_char(']'), PartialToken(CloseBracket));
        assert_eq!(classify_char('{'), PartialToken(OpenCurly));
        assert_eq!(classify_char('}'), PartialToken(CloseCurly));
    }

    #[test]
    fn classify_number() {
        assert_eq!(classify_char('0'), PartialToken(NumberLit));
        assert_eq!(classify_char('1'), PartialToken(NumberLit));
        assert_eq!(classify_char('2'), PartialToken(NumberLit));
    }
}
