use crate::error::TError;
use crate::location::{IndexIntoFile, Location, SymbolLength};
use crate::parser::semantics::BindingMode;
use log::debug;
use std::fmt;

#[cfg(test)]
use strum_macros::EnumIter;

use static_assertions::*;
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
    InfixBinOp,
    Open,
    Close,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[cfg_attr(test, derive(EnumIter))]
pub enum Symbol {
    // Closes
    CloseBracket,
    CloseCurly,
    CloseParen,
    // Opens
    OpenParen,
    OpenCurly,
    OpenBracket,
    // Sequences
    Sequence,
    Comma,
    // Assignments
    Assign,
    AddAssign,
    SubAssign,
    DivAssign,
    DivRoundingAssign,
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
    LeftShift,
    Gt,
    GtEqs,
    RightShift,

    // Maths
    Add,
    Sub,
    Exp,
    Div,
    DivRounding,
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

impl Symbol {
    pub fn binding(&self) -> OpBinding {
        match self {
            Symbol::Escape
            | Symbol::BitNot
            | Symbol::LogicalNot
            | Symbol::GetAddress
            | Symbol::Spread
            | Symbol::Lambda
            | Symbol::Sigma
            | Symbol::Forall
            | Symbol::Pi
            | Symbol::Exists => OpBinding::PrefixOp,
            Symbol::Try => OpBinding::PostfixOp,
            Symbol::CloseCurly | Symbol::CloseParen | Symbol::CloseBracket => OpBinding::Close,
            Symbol::OpenCurly | Symbol::OpenParen | Symbol::OpenBracket => OpBinding::Open,
            _ => OpBinding::InfixBinOp,
        }
    }

    // For const eval.
    // TODO: Store in a const map
    // TODO: Check there are no cycles.
    pub fn is_more_tight(&self, other: Symbol) -> bool {
        if *self == other {
            true
        } else if let Some(tighter) = other.next_more_tight() {
            self.is_more_tight(tighter)
        } else {
            false
        }
    }

    /*
    Source: https://www.foonathan.net/2017/07/operator-precedence/

    Inside the categories the relative precedence of the operators is as follows:

        logical operators: ! > &&,||, but not mixed && and || chains

        comparison operators: no chaining at all

        mathematical operators: unary +,- > *,/ > +,-, with the usual associativity

        bitwise operators: unary ~ before the binary operators, but again no mixed chaining of &, | and ^ and no chaining of the shift operators

        unary operators: just as usual
    */
    pub fn next_more_tight(&self) -> Option<Symbol> {
        let next = match self {
            Symbol::OpenParen => Symbol::OpenCurly,
            Symbol::OpenCurly => Symbol::OpenBracket,
            Symbol::OpenBracket => Symbol::Sequence,
            Symbol::Sequence => Symbol::Comma,
            Symbol::Comma => Symbol::Assign,
            Symbol::Assign => Symbol::AddAssign,
            Symbol::AddAssign => Symbol::SubAssign,
            Symbol::SubAssign => Symbol::DivAssign,
            Symbol::DivAssign => Symbol::DivRoundingAssign,
            Symbol::DivRoundingAssign => Symbol::MulAssign,
            Symbol::MulAssign => Symbol::AndAssign,
            Symbol::AndAssign => Symbol::OrAssign,
            Symbol::OrAssign => Symbol::BitXorAssign,
            Symbol::BitXorAssign => Symbol::LogicalAndAssign,
            Symbol::LogicalAndAssign => Symbol::LogicalOrAssign,
            Symbol::LogicalOrAssign => Symbol::ModuloAssign,
            Symbol::ModuloAssign => Symbol::HasType,
            Symbol::HasType => Symbol::LeftPipe,
            Symbol::LeftPipe => Symbol::RightPipe,
            Symbol::RightPipe => Symbol::Sigma,
            Symbol::Sigma => Symbol::Lambda,
            Symbol::Lambda => Symbol::Arrow,
            Symbol::Arrow => Symbol::DoubleArrow,
            Symbol::DoubleArrow => Symbol::Forall,
            Symbol::Forall => Symbol::Pi,
            Symbol::Pi => Symbol::Exists,
            Symbol::Exists => Symbol::Eqs,
            Symbol::Eqs => Symbol::NotEqs,
            Symbol::NotEqs => Symbol::Lt,
            Symbol::Lt => Symbol::LtEqs,
            Symbol::LtEqs => Symbol::LeftShift,
            Symbol::LeftShift => Symbol::Gt,
            Symbol::Gt => Symbol::GtEqs,
            Symbol::GtEqs => Symbol::RightShift,
            Symbol::RightShift => Symbol::Add,
            Symbol::Add => Symbol::Sub,
            Symbol::Sub => Symbol::Exp,
            Symbol::Exp => Symbol::Div,
            Symbol::Div => Symbol::DivRounding,
            Symbol::DivRounding => Symbol::Mul,
            Symbol::Mul => Symbol::And,
            Symbol::And => Symbol::Or,
            Symbol::Or => Symbol::BitNot,
            Symbol::BitNot => Symbol::BitXor,
            Symbol::BitXor => Symbol::LogicalNot,
            Symbol::LogicalNot => Symbol::LogicalAnd,
            Symbol::LogicalAnd => Symbol::LogicalOr,
            Symbol::LogicalOr => Symbol::Modulo,
            Symbol::Modulo => Symbol::GetAddress,
            Symbol::GetAddress => Symbol::Try,
            Symbol::Try => Symbol::Dot,
            Symbol::Dot => Symbol::Range,
            Symbol::Range => Symbol::Spread,
            Symbol::Spread => Symbol::Escape,
            // Done?
            Symbol::Escape | Symbol::CloseBracket | Symbol::CloseCurly | Symbol::CloseParen => {
                return None
            }
        };
        Some(next)
    }
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                // Basics
                Symbol::Add => "+",
                Symbol::Sub => "-",
                Symbol::Div => "/",
                Symbol::DivRounding => "//",
                Symbol::Escape => "\\",
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
                Symbol::Comma => ",",
                Symbol::Sequence => ";",
                Symbol::Arrow => "->",
                Symbol::DoubleArrow => "=>",
                Symbol::LeftShift => "<<",
                Symbol::RightShift => ">>",
                Symbol::LeftPipe => "<|",
                Symbol::RightPipe => "|>",
                // Assignment versions
                Symbol::Assign => "=",
                Symbol::AddAssign => "+=",
                Symbol::SubAssign => "-=",
                Symbol::DivAssign => "/=",
                Symbol::MulAssign => "*=",
                Symbol::DivRoundingAssign => "//=",
                Symbol::AndAssign => "&=",
                Symbol::OrAssign => "|=",
                Symbol::BitXorAssign => "^=",
                Symbol::LogicalAndAssign => "&&=",
                Symbol::LogicalOrAssign => "||=",
                Symbol::ModuloAssign => "%=",
                // Quantification
                Symbol::Lambda => "λ",
                Symbol::Sigma => "Σ",
                Symbol::Pi => "Π",
                Symbol::Forall => "∀",
                Symbol::Exists => "∃",
                // Comparisons
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
            }
        )
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum CharacterType {
    AtomHead,                // '$' on it's own (invalid).
    ColorLitHead,            // '#' on it's own (invalid).
    Whitespace,              // A special discardable token representing whitespace.
    HexSym, // A subset of symbol characters that can be used in Hex strings (e.g. Colors).
    PartialToken(TokenType), // Already a valid token!
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum TokenType {
    Op(Symbol), // An operator (i.e. a known symbol used as a prefix or infix operator).
    Ident,      // A named value.
    Atom,       // A symbol starting with a '$', used differently to symbols which have values.
    // Literals (i.e. tokens representing values):
    NumLit,
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

#[derive(Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Token {
    pub kind: TokenType,
    // These are byte indexes and byte lengths. They may need to be interpreted before being shown
    // to the user.
    pub start: IndexIntoFile,
    pub length: SymbolLength,
}

impl Token {
    pub fn location(self) -> Location {
        Location {
            start: self.start,
            length: self.length,
        }
    }

    pub fn to_prim(&self) -> i32 {
        todo!("Provide a 'to_prim' that also de escapes tokens.")
    }

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
            self.start + (self.length as IndexIntoFile)
        )
    }
}

const _COMMENT: &str = "//";
const _MULTI_COMMENT: &str = "/*";

#[inline]
fn classify_char(ch: char) -> CharacterType {
    use CharacterType::*;
    use TokenType::*;
    PartialToken(match ch {
        '\n' | '\r' | '\t' | ' ' => return Whitespace,
        '$' => return AtomHead,
        '#' => return ColorLitHead,
        'A'..='F' | 'a'..='f' => return HexSym,
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
        ',' => Op(Symbol::Comma),
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
        '0'..='9' => NumLit,
        'A'..='Z' | 'a'..='z' | '_' => Ident, // Overlapped by colors.
        '"' | '\'' => StringLit,
        _ => panic!("Unknown token character {ch}"),
    })
}

#[inline]
pub const fn assign_op(s: Symbol) -> Option<Symbol> {
    // TODO(clarity): Move to a symbol module.
    Some(match s {
        Symbol::AddAssign => Symbol::Add,
        Symbol::SubAssign => Symbol::Sub,
        Symbol::DivAssign => Symbol::Div,
        Symbol::DivRoundingAssign => Symbol::DivRounding,
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
pub const fn binding_mode_operation(s: Symbol) -> Option<BindingMode> {
    // TODO(clarity): Move to a symbol module.
    Some(match s {
        Symbol::Lambda => BindingMode::Lambda,
        Symbol::Pi => BindingMode::Pi,
        Symbol::Forall => BindingMode::Pi,
        Symbol::Exists => BindingMode::Sigma,
        Symbol::Sigma => BindingMode::Sigma,
        _ => return None,
    })
}

#[inline]
pub const fn is_assign(s: Symbol) -> bool {
    // TODO(clarity): Move to a symbol module.
    matches!(s, Symbol::Assign) || assign_op(s).is_some()
}

#[inline]
fn is_whitespace(chr: char) -> bool {
    classify_char(chr) == CharacterType::Whitespace
}

#[derive(Debug)]
pub struct Characters<'a> {
    it: std::iter::Peekable<std::str::Chars<'a>>,
    index: usize,
    start: usize,
    curr: Option<char>,
}

impl<'a> Characters<'a> {
    fn new(s: &'a str) -> Self {
        Self {
            it: s.chars().peekable(),
            index: 0,
            start: 0,
            curr: None,
        }
    }
    #[allow(unused)]
    fn curr(&self) -> Option<char> {
        self.curr
    }
    fn start(&self) -> usize {
        self.start
    }
    fn curr_char_bytes(&self) -> usize {
        self.curr.map(|c| c.len_utf8()).unwrap_or(0)
    }
    fn length(&self) -> usize {
        (self.index + self.curr_char_bytes())
            .checked_sub(self.start())
            .expect("Token should finish after it starts")
    }
    #[allow(unused)]
    fn index(&self) -> usize {
        self.index
    }
    fn set_start(&mut self) -> usize {
        self.start = self.index;
        self.start
    }
    fn next(&mut self) -> Option<char> {
        self.index += self.curr_char_bytes();
        self.curr = self.it.next();
        self.curr
    }
    fn peek(&mut self) -> Option<char> {
        self.it.peek().copied()
    }
}

// Reads all the tokens.
pub fn lex(contents: &str) -> Result<Vec<Token>, TError> {
    debug!("Lex {}", contents);
    let mut chars = Characters::new(contents);
    let mut tokens = Vec::with_capacity(1000); // TODO(perf): Bench mark & tune?
    while lex_head(&mut chars, &mut tokens) {}
    Ok(tokens)
}

// Consumes a single token.
pub fn lex_head(characters: &mut Characters, tokens: &mut Vec<Token>) -> bool {
    while let Some(chr) = characters.peek() {
        // skip whitespace.
        if !is_whitespace(chr) {
            // TODO(perf): use trim_start
            break;
        }
        characters.next();
    }
    // TODO(usability): Work out a better way of printing pretty spaces.
    /*
    // TODO(usability): Handle comments.
    // Token is finished, covers from `start` to `characters`.
    let comment = contents[start..end].starts_with(COMMENT);
    let multi_comment = contents[start..end].starts_with(MULTI_COMMENT);
    if !comment && !multi_comment {
    }
    // Track depth of mutli line comments
    let mut depth = 1;
    let mut last: Option<char> = None;
    loop {
        characters.next();
        // Add the character.
        match (last, characters.peek().map(|(_, chr)| *chr)) { // TODO(perf): use .find
            (Some('/'), Some('*')) => {
                depth += 1;
            }
            (Some('*'), Some('/')) => {
                depth -= 1;
                if multi_comment && depth == 0 {
                    characters.next();
                    return lex_head(contents, characters);
                }
            }
            (_, Some(chr)) => {
                if comment && (chr == '\n' || chr == '\r') {
                    characters.next();
                    return lex_head(contents, characters);
                }
                last = Some(chr);
            }
            (_, None) => return lex_head(characters),
        }
    }
    */
    use CharacterType::*;
    use TokenType::*;
    let chr = if let Some(chr) = characters.next() {
        chr
    } else {
        return false;
    };
    let mut kind = classify_char(chr);
    characters.set_start(); // Start the token!
    while let Some(chr) = characters.peek() {
        // TODO(perf): these could be bit strings and we could and them.
        kind = match (kind, classify_char(chr)) {
            (PartialToken(Op(first)), PartialToken(Op(second))) => {
                PartialToken(Op(match (first, second) {
                    // Continuation
                    (Symbol::Add, Symbol::Assign) => Symbol::AddAssign,
                    (Symbol::Sub, Symbol::Assign) => Symbol::SubAssign,
                    (Symbol::Sub, Symbol::Gt) => Symbol::Arrow,
                    (Symbol::Div, Symbol::Assign) => Symbol::DivAssign,
                    (Symbol::Div, Symbol::Div) => Symbol::DivRounding,
                    (Symbol::DivRounding, Symbol::Assign) => Symbol::DivRoundingAssign,
                    (Symbol::Mul, Symbol::Assign) => Symbol::MulAssign,
                    (Symbol::Mul, Symbol::Mul) => Symbol::Exp,
                    (Symbol::Modulo, Symbol::Assign) => Symbol::ModuloAssign,
                    (Symbol::LogicalOr, Symbol::Assign) => Symbol::LogicalOrAssign,
                    (Symbol::LogicalAnd, Symbol::Assign) => Symbol::LogicalAndAssign,
                    (Symbol::And, Symbol::Assign) => Symbol::AndAssign,
                    (Symbol::And, Symbol::And) => Symbol::LogicalAnd,
                    (Symbol::BitXor, Symbol::Assign) => Symbol::BitXorAssign,
                    (Symbol::Lt, Symbol::Or) => Symbol::LeftPipe,
                    (Symbol::Or, Symbol::Assign) => Symbol::OrAssign,
                    (Symbol::Or, Symbol::Or) => Symbol::LogicalOr,
                    (Symbol::Or, Symbol::Gt) => Symbol::RightPipe,
                    (Symbol::Lt, Symbol::Lt) => Symbol::LeftShift,
                    (Symbol::Gt, Symbol::Gt) => Symbol::RightShift,
                    (Symbol::Dot, Symbol::Dot) => Symbol::Range,
                    (Symbol::Range, Symbol::Dot) => Symbol::Spread,
                    (Symbol::Assign, Symbol::Assign) => Symbol::Eqs,
                    (Symbol::Assign, Symbol::Gt) => Symbol::DoubleArrow,
                    (Symbol::Gt, Symbol::Assign) => Symbol::GtEqs,
                    (Symbol::Lt, Symbol::Assign) => Symbol::LtEqs,
                    (Symbol::LogicalNot, Symbol::Assign) => Symbol::NotEqs,
                    (_, _) => break,
                }))
            }
            (ColorLitHead, HexSym | PartialToken(NumLit)) => PartialToken(ColorLit), // Color Literal.
            (PartialToken(ColorLit), HexSym | PartialToken(NumLit)) => PartialToken(ColorLit), // Color Literal.
            (AtomHead, HexSym | PartialToken(NumLit | Ident)) => PartialToken(Atom), // Atom.
            (PartialToken(Atom), HexSym | PartialToken(NumLit | Ident)) => PartialToken(Atom), // Atom.
            (HexSym | PartialToken(Ident), HexSym | PartialToken(NumLit | Ident)) => {
                PartialToken(Ident)
            } // Symbol.
            (PartialToken(NumLit), PartialToken(NumLit)) => PartialToken(NumLit), // Continuation
            (PartialToken(NumLit), PartialToken(Ident)) => PartialToken(NumLit), // Number with suffix.
            _ => break, // Token finished can't continue here.
        };
        characters.next(); // Continue past the character.
    }
    if kind == CharacterType::HexSym {
        kind = PartialToken(TokenType::Ident);
    }
    let kind = if let PartialToken(kind) = kind {
        kind
    } else {
        todo!("Invalid / unfinished token: {kind:?} {characters:?}");
    };
    if kind == StringLit {
        let quote = characters
            .curr()
            .expect("String literals should start with a quote");
        while let Some(chr) = characters.next() {
            // TODO(perf): use .find
            if chr == quote {
                break; // reached the end of the quote.
            }
            if chr == '\\' {
                characters.next(); // Skip escaped quotes.
            }
        }
    }
    let length = characters.length();
    if length > SymbolLength::MAX as usize {
        assert_eq!(kind, TokenType::StringLit); // TODO(usability): Error here.
        let mut number_of_tokens =
            (length + SymbolLength::MAX as usize - 1) / (SymbolLength::MAX as usize);
        if number_of_tokens >= (SymbolLength::MAX as usize) {
            todo!("Token was too long ({length:?}), implement a recursive group thing...");
        }
        tokens.push(Token {
            start: characters.start() as IndexIntoFile,
            length: number_of_tokens as SymbolLength,
            kind: TokenType::Group,
        });
        let mut length = length;
        while length > 0 {
            let curr_len = std::cmp::min(length, SymbolLength::MAX as usize);
            length -= curr_len;
            number_of_tokens -= 1;
            tokens.push(Token {
                start: characters.start() as IndexIntoFile,
                length: curr_len as SymbolLength,
                kind,
            });
        }
        assert_eq!(
            number_of_tokens, 0,
            "Should generate the calculated number of grouped tokens"
        );
    } else {
        tokens.push(Token {
            start: characters.start() as IndexIntoFile,
            length: length as SymbolLength,
            kind,
        })
    }
    true
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::{CharacterType::*, TokenType::*};
    use strum::IntoEnumIterator; // TODO(cleanup): Make these test only

    fn setup_many(contents: &str, n: usize) -> Vec<Token> {
        let mut chars = Characters::new(contents);
        let mut tokens = Vec::new();
        for _i in 0..n {
            lex_head(&mut chars, &mut tokens);
        }
        tokens
    }
    fn setup(contents: &str) -> Vec<Token> {
        setup_many(contents, 1)
    }

    #[test]
    fn classify_whitespace() {
        assert_eq!(classify_char(' '), Whitespace);
        assert_eq!(classify_char('\n'), Whitespace);
        assert_eq!(classify_char('\r'), Whitespace);
    }

    #[test]
    fn classify_parens_and_brackets() {
        assert_eq!(classify_char('('), PartialToken(Op(Symbol::OpenParen)));
        assert_eq!(classify_char(')'), PartialToken(Op(Symbol::CloseParen)));
        assert_eq!(classify_char('['), PartialToken(Op(Symbol::OpenBracket)));
        assert_eq!(classify_char(']'), PartialToken(Op(Symbol::CloseBracket)));
        assert_eq!(classify_char('{'), PartialToken(Op(Symbol::OpenCurly)));
        assert_eq!(classify_char('}'), PartialToken(Op(Symbol::CloseCurly)));
    }

    #[test]
    fn classify_number() {
        assert_eq!(classify_char('0'), PartialToken(NumLit));
        assert_eq!(classify_char('1'), PartialToken(NumLit));
        assert_eq!(classify_char('2'), PartialToken(NumLit));
    }

    #[test]
    fn lex_head_number() {
        let tokens = setup("123");
        assert_eq!(
            tokens,
            vec![Token {
                kind: NumLit,
                start: 0,
                length: 3
            }]
        );
    }

    #[test]
    #[should_panic]
    fn lex_head_color_invalid() {
        setup("#");
    }

    #[test]
    fn lex_head_color_hex() {
        let tokens = setup("#f9F");
        assert_eq!(
            tokens,
            vec![Token {
                kind: ColorLit,
                start: 0,
                length: 4
            }]
        );
    }

    #[test]
    fn lex_head_color_black() {
        let tokens = setup("#000000");
        assert_eq!(
            tokens,
            vec![Token {
                kind: ColorLit,
                start: 0,
                length: 7
            }]
        );
    }

    #[test]
    fn lex_head_atom() {
        let tokens = setup("$a1_2_3");
        assert_eq!(
            tokens,
            vec![Token {
                kind: Atom,
                start: 0,
                length: 7
            }]
        );
    }

    #[test]
    fn lex_head_symbol_with_underscores() {
        let tokens = setup("a1_2_3");
        assert_eq!(
            tokens,
            vec![Token {
                kind: Ident,
                start: 0,
                length: 6
            }]
        );
    }

    #[test]
    fn lex_head_symbol() {
        let tokens = setup("a123");
        assert_eq!(
            tokens,
            vec![Token {
                kind: Ident,
                start: 0,
                length: 4
            }]
        );
    }

    #[test]
    fn lex_head_operator() {
        let tokens = setup("-a123");
        assert_eq!(
            tokens,
            vec![Token {
                kind: Op(Symbol::Sub),
                start: 0,
                length: 1
            }]
        );
    }

    #[test]
    fn lex_head_num_and_newline_linux() {
        let tokens = setup("\n12");
        assert_eq!(
            tokens,
            vec![Token {
                kind: NumLit,
                start: 1,
                length: 2
            }]
        );
    }

    #[test]
    fn lex_head_num_and_newline_windows() {
        let tokens = setup("\r\n12");
        assert_eq!(
            tokens,
            vec![Token {
                kind: NumLit,
                start: 2,
                length: 2
            }]
        );
    }

    #[test]
    fn lex_head_num_and_newline_old_mac() {
        // For mac systems before OSX
        let tokens = setup("\r12");
        assert_eq!(
            tokens,
            vec![Token {
                kind: NumLit,
                start: 1,
                length: 2
            }]
        );
    }

    #[test]
    fn lex_head_escaped_characters_in_string() {
        let contents = "'\\n\\t2\\r\\\'\"'";
        let tokens = setup(contents);
        assert_eq!(
            tokens,
            vec![Token {
                kind: StringLit,
                start: 0,
                length: 12
            }]
        );
        assert_str_eq!(tokens[0].get_src(contents), "\'\\n\\t2\\r\\'\"\'");
    }

    #[test]
    fn lex_head_call() {
        let contents = "x()";
        let tokens = setup_many(contents, 3);
        assert_eq!(
            tokens,
            vec![
                Token {
                    kind: Ident,
                    start: 0,
                    length: 1
                },
                Token {
                    kind: Op(Symbol::OpenParen),
                    start: 1,
                    length: 1
                },
                Token {
                    kind: Op(Symbol::CloseParen),
                    start: 2,
                    length: 1
                }
            ]
        );
        assert_str_eq!(tokens[0].get_src(contents), "x");
        assert_str_eq!(tokens[1].get_src(contents), "(");
        assert_str_eq!(tokens[2].get_src(contents), ")");
    }

    #[test]
    fn lex_head_strings_with_operators() {
        let contents = "!\"hello world\"\n7";
        let tokens = setup_many(contents, 3);
        assert_eq!(
            tokens,
            vec![
                Token {
                    kind: Op(Symbol::LogicalNot),
                    start: 0,
                    length: 1
                },
                Token {
                    kind: StringLit,
                    start: 1,
                    length: 13
                },
                Token {
                    kind: NumLit,
                    start: 15,
                    length: 1
                },
            ]
        );
        assert_str_eq!(tokens[0].get_src(contents), "!");
        // The token-izer is not responsible for un-escaping...
        assert_str_eq!(tokens[1].get_src(contents), "\"hello world\"");
        assert_str_eq!(tokens[2].get_src(contents), "7");
    }

    #[test]
    fn lex_parentheses() {
        let contents = "(\"hello world\"\n)";
        let tokens = setup_many(contents, 3);
        let expected = vec![Op(Symbol::OpenParen), StringLit, Op(Symbol::CloseParen)];
        assert_eq!(
            tokens
                .iter()
                .map(|tok| tok.kind)
                .collect::<Vec<TokenType>>(),
            expected
        );
        let expected_strs = vec!["(", "\"hello world\"", ")"];
        assert_eq!(
            tokens
                .iter()
                .map(|tok| tok.get_src(contents))
                .collect::<Vec<&str>>(),
            expected_strs
        );
    }

    #[test]
    fn lex_curlies() {
        let contents = "{\"hello world\"\n}";
        let tokens = setup_many(contents, 3);
        let expected = vec![Op(Symbol::OpenCurly), StringLit, Op(Symbol::CloseCurly)];
        assert_eq!(
            tokens
                .iter()
                .map(|tok| tok.kind)
                .collect::<Vec<TokenType>>(),
            expected
        );
        let expected_strs = vec!["{", "\"hello world\"", "}"];
        assert_eq!(
            tokens
                .iter()
                .map(|tok| tok.get_src(contents))
                .collect::<Vec<&str>>(),
            expected_strs
        );
    }

    #[test]
    fn lex_brackets() {
        let contents = "[\"hello world\"\n]";
        let tokens = setup_many(contents, 3);
        let expected = vec![Op(Symbol::OpenBracket), StringLit, Op(Symbol::CloseBracket)];
        assert_eq!(
            tokens
                .iter()
                .map(|tok| tok.kind)
                .collect::<Vec<TokenType>>(),
            expected
        );
        let expected_strs = vec!["[", "\"hello world\"", "]"];
        assert_eq!(
            tokens
                .iter()
                .map(|tok| tok.get_src(contents))
                .collect::<Vec<&str>>(),
            expected_strs
        );
    }

    #[test]
    fn lex_strings_with_operators() {
        let contents = "!\"hello world\"\n7";
        let tokens = setup_many(contents, 3);
        let expected = vec![Op(Symbol::LogicalNot), StringLit, NumLit];
        assert_eq!(
            tokens
                .iter()
                .map(|tok| tok.kind)
                .collect::<Vec<TokenType>>(),
            expected
        );
        let expected_strs = vec!["!", "\"hello world\"", "7"];
        assert_eq!(
            tokens
                .iter()
                .map(|tok| tok.get_src(contents))
                .collect::<Vec<&str>>(),
            expected_strs
        );
    }

    #[test]
    fn can_tokenize_operators() {
        for symbol in Symbol::iter() {
            let symbol_str = format!("{}", &symbol);
            let contents = format!("{}123", &symbol);
            let tokens = setup_many(&contents, 2);
            let length = symbol_str.len();
            assert_eq!(
                tokens,
                vec![
                    Token {
                        kind: Op(symbol),
                        start: 0,
                        length: length as SymbolLength,
                    },
                    Token {
                        kind: NumLit,
                        start: length as IndexIntoFile,
                        length: 3,
                    },
                ],
                "Failed with operator {symbol}"
            );
            assert_str_eq!(tokens[0].get_src(&contents), symbol_str);
            assert_str_eq!(tokens[1].get_src(&contents), "123");
        }
    }
}
