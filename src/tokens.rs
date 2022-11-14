use crate::error::TError;
use crate::location::{IndexIntoFile, Location, SymbolLength};
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
#[cfg_attr(test, derive(EnumIter))]
pub enum Symbol {
    // Basics
    Assign,
    Add,
    AddAssign,
    Sub,
    SubAssign,
    Div,
    DivAssign,
    DivRounding,
    DivRoundingAssign,
    Escape,
    Mul,
    MulAssign,
    Exp,
    BitNot,
    BitAnd,
    BitAndAssign,
    BitXor,
    BitXorAssign,
    BitOr,
    BitOrAssign,
    LeftPipe,
    LogicalNot,
    LogicalAnd,
    LogicalAndAssign,
    LogicalOr,
    LogicalOrAssign,
    Modulo,
    ModuloAssign,
    PtrTo,
    HasType,
    FunctionType,
    Try,
    Dot,
    Range,
    Spread,
    Comma,
    Sequence,
    // Comparisons
    Eqs,
    NotEqs,
    Lt,
    LtEqs,
    LeftShift,
    RightPipe,
    Gt,
    GtEqs,
    RightShift,
    // The rest?
    // Named(StrId),
}

impl<'a> std::fmt::Display for Symbol {
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
                Symbol::BitAnd => "&",
                Symbol::BitXor => "^",
                Symbol::BitOr => "|",
                Symbol::LogicalAnd => "&&",
                Symbol::LogicalOr => "||",
                Symbol::Modulo => "%",
                Symbol::PtrTo => "@",
                Symbol::HasType => ":", // TODO: Work out a better way of printing pretty spaces.
                Symbol::Try => "?",
                Symbol::Dot => ".",
                Symbol::Range => "..",
                Symbol::Spread => "...",
                Symbol::Comma => ",",
                Symbol::Sequence => ";",
                Symbol::FunctionType => "->",
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
                Symbol::BitAndAssign => "&=",
                Symbol::BitXorAssign => "^=",
                Symbol::BitOrAssign => "|=",
                Symbol::LogicalAndAssign => "&&=",
                Symbol::LogicalOrAssign => "||=",
                Symbol::ModuloAssign => "%=",
                // Comparisons
                Symbol::Eqs => "==",
                Symbol::NotEqs => "!=",
                Symbol::Lt => "<",
                Symbol::Gt => ">",
                Symbol::LtEqs => "<=",
                Symbol::GtEqs => ">=",
            }
        )
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum TokenType {
    Op(Symbol),
    OpenCurly,
    CloseCurly,
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    Sym,
    Unknown,
    Whitespace,
    Eof,
    NumLit,
    // Short strings can be stored as symbols.
    StringLit,
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

    pub fn eof(start: IndexIntoFile) -> Self {
        Self {
            start,
            kind: TokenType::Eof,
            length: 0, // zero characters == empty str.
        }
    }

    #[allow(dead_code)] // TODO:: REMOVE!
    fn get_str<'a>(&self, source: &'a str) -> &'a str {
        // Assuming the token is from the source file...
        &source[self.start as usize..self.start as usize + self.length as usize]
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO: Look up the token to get the contents?
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
fn classify_char(ch: char) -> TokenType {
    // TODO: replace this with an array with a value for each character.
    use TokenType::*;
    match ch {
        '\n' | '\r' | '\t' | ' ' => Whitespace,
        '~' => Op(Symbol::BitNot),
        '!' => Op(Symbol::LogicalNot),
        '@' => Op(Symbol::PtrTo),
        '%' => Op(Symbol::Modulo),
        '^' => Op(Symbol::BitXor),
        '&' => Op(Symbol::BitAnd),
        '*' => Op(Symbol::Mul),
        '-' => Op(Symbol::Sub),
        '+' => Op(Symbol::Add),
        '=' => Op(Symbol::Assign),
        '<' => Op(Symbol::Lt),
        '>' => Op(Symbol::Gt),
        '|' => Op(Symbol::BitOr),
        '#' => todo!(),
        '$' => todo!(),
        '\\' => Op(Symbol::Escape), // Escape?
        '/' => Op(Symbol::Div),
        '?' => Op(Symbol::Try),
        '.' => Op(Symbol::Dot),
        ',' => Op(Symbol::Comma),
        ':' => Op(Symbol::HasType),
        ';' => Op(Symbol::Sequence),
        '(' => OpenParen,
        ')' => CloseParen,
        '{' => OpenCurly,
        '}' => CloseCurly,
        '[' => OpenBracket,
        ']' => CloseBracket,
        '0'..='9' => NumLit,
        '"' | '\'' => StringLit,
        'A'..='Z' | 'a'..='z' | '_' => Sym,
        _ => panic!("Unknown character {}", ch),
    }
}

#[inline]
fn is_whitespace(chr: char) -> bool {
    classify_char(chr) == TokenType::Whitespace
}

pub struct Characters<'a> {
    it: std::iter::Peekable<std::str::Chars<'a>>,
    index: usize,
    start: usize,
    prev: Option<char>,
}

impl<'a> Characters<'a> {
    fn new(s: &'a str) -> Self {
        Self {
            it: s.chars().peekable(),
            index: 0,
            start: 0,
            prev: None,
        }
    }
    fn prev(&self) -> Option<char> {
        self.prev
    }
    fn start(&self) -> usize {
        self.start
    }
    fn index(&self) -> usize {
        self.index
    }
    fn set_start(&mut self) -> usize {
        self.start = self.index;
        self.start
    }
    fn next(&mut self) -> Option<char> {
        self.prev = self.peek();
        self.index += 1;
        self.it.next()
    }
    fn peek(&self) -> Option<char> {
        let mut it = self.it.clone();
        it.next()
    }
}

// Reads all the tokens.
pub fn lex(contents: &str) -> Result<Vec<Token>, TError> {
    debug!("Lex {}", contents);
    let mut chars = Characters::new(contents);
    let mut tokens = Vec::with_capacity(1000); // TODO: Bench mark & tune?
    while lex_head(&mut chars, &mut tokens) {}
    Ok(tokens)
}

// Consumes a single token.
pub fn lex_head(characters: &mut Characters, tokens: &mut Vec<Token>) -> bool {
    while let Some(chr) = characters.peek() {
        // skip whitespace.
        if !is_whitespace(chr) {
            // TODO: use trim_start
            break;
        }
        characters.next();
    }
    /*
    // TODO: Handle comments.
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
        match (last, characters.peek().map(|(_, chr)| *chr)) { // TODO: use .find
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
    if characters.peek().is_none() {
        return false;
    }
    characters.set_start();
    // TODO: This should be simplified (make tight loops).
    use TokenType::*;
    let mut kind: TokenType = Unknown;
    while let Some(chr) = characters.peek() {
        // TODO: these could be bit strings and we could and them.
        kind = match (kind, classify_char(chr)) {
            (_, Whitespace) => break,                // Token finished whitespace.
            (Unknown, new_tok_type) => new_tok_type, // Start token.
            (Op(first), Op(second)) => match (first, second) {
                // Continuation
                (Symbol::Add, Symbol::Assign) => Op(Symbol::AddAssign),
                (Symbol::Sub, Symbol::Assign) => Op(Symbol::SubAssign),
                (Symbol::Sub, Symbol::Gt) => Op(Symbol::FunctionType),
                (Symbol::Div, Symbol::Assign) => Op(Symbol::DivAssign),
                (Symbol::Div, Symbol::Div) => Op(Symbol::DivRounding),
                (Symbol::DivRounding, Symbol::Assign) => Op(Symbol::DivRoundingAssign),
                (Symbol::Mul, Symbol::Assign) => Op(Symbol::MulAssign),
                (Symbol::Mul, Symbol::Mul) => Op(Symbol::Exp),
                (Symbol::Modulo, Symbol::Assign) => Op(Symbol::ModuloAssign),
                (Symbol::LogicalOr, Symbol::Assign) => Op(Symbol::LogicalOrAssign),
                (Symbol::LogicalAnd, Symbol::Assign) => Op(Symbol::LogicalAndAssign),
                (Symbol::BitAnd, Symbol::Assign) => Op(Symbol::BitAndAssign),
                (Symbol::BitAnd, Symbol::BitAnd) => Op(Symbol::LogicalAnd),
                (Symbol::BitXor, Symbol::Assign) => Op(Symbol::BitXorAssign),
                (Symbol::Lt, Symbol::BitOr) => Op(Symbol::LeftPipe),
                (Symbol::BitOr, Symbol::Assign) => Op(Symbol::BitOrAssign),
                (Symbol::BitOr, Symbol::BitOr) => Op(Symbol::LogicalOr),
                (Symbol::BitOr, Symbol::Gt) => Op(Symbol::RightPipe),
                (Symbol::Lt, Symbol::Lt) => Op(Symbol::LeftShift),
                (Symbol::Gt, Symbol::Gt) => Op(Symbol::RightShift),
                (Symbol::Dot, Symbol::Dot) => Op(Symbol::Range),
                (Symbol::Range, Symbol::Dot) => Op(Symbol::Spread),
                (Symbol::Assign, Symbol::Assign) => Op(Symbol::Eqs),
                (Symbol::Gt, Symbol::Assign) => Op(Symbol::GtEqs),
                (Symbol::Lt, Symbol::Assign) => Op(Symbol::LtEqs),
                (Symbol::LogicalNot, Symbol::Assign) => Op(Symbol::NotEqs),
                (_, _) => break,
            },
            (NumLit, NumLit) => NumLit, // Continuation
            (NumLit, Sym) => NumLit,    // Number with suffix.
            (Sym, NumLit | Sym) => Sym, // Symbol.
            _ => break,                 // Token finished can't continue here.
        };
        characters.next(); // Continue past the character.
    }
    if kind == StringLit {
        let quote = characters
            .prev()
            .expect("String literals should start with a quote");
        while let Some(chr) = characters.next() {
            // TODO: use .find
            if chr == quote {
                break; // reached the end of the quote.
            }
            if chr == '\\' {
                characters.next(); // Skip escaped quotes.
            }
        }
    };
    let length = characters
        .index()
        .checked_sub(characters.start())
        .expect("Token should finish after it starts");

    if length > SymbolLength::MAX as usize {
        assert_eq!(kind, TokenType::StringLit); // TODO: Error here.
        let mut number_of_tokens =
            (length + SymbolLength::MAX as usize - 1) / (SymbolLength::MAX as usize);
        if number_of_tokens >= (u8::MAX as usize) {
            todo!("Token was too long, implement a recursive group thing...");
        }
        tokens.push(Token {
            start: characters.start() as IndexIntoFile,
            length: number_of_tokens as u8,
            kind: TokenType::Group,
        });
        let mut length = length;
        while length > 0 {
            let curr_len = std::cmp::min(length, u8::MAX as usize);
            length -= curr_len;
            number_of_tokens -= 1;
            tokens.push(Token {
                start: characters.start() as IndexIntoFile,
                length: curr_len as u8,
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
    use super::TokenType::*;
    use super::*;
    use strum::IntoEnumIterator; // TODO: Make these test only

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
        assert_eq!(classify_char('('), OpenParen);
        assert_eq!(classify_char(')'), CloseParen);
        assert_eq!(classify_char('['), OpenBracket);
        assert_eq!(classify_char(']'), CloseBracket);
        assert_eq!(classify_char('{'), OpenCurly);
        assert_eq!(classify_char('}'), CloseCurly);
    }

    #[test]
    fn classify_number() {
        assert_eq!(classify_char('0'), NumLit);
        assert_eq!(classify_char('1'), NumLit);
        assert_eq!(classify_char('2'), NumLit);
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
    fn lex_head_symbol() {
        let tokens = setup("a123");
        assert_eq!(
            tokens,
            vec![Token {
                kind: Sym,
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
        // TODO: De escape them.
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
        assert_str_eq!(tokens[0].get_str(contents), "\'\\n\\t2\\r\\'\"\'");
    }

    #[test]
    fn lex_head_call() {
        let contents = "x()";
        let tokens = setup_many(contents, 3);
        assert_eq!(
            tokens,
            vec![
                Token {
                    kind: Sym,
                    start: 0,
                    length: 1
                },
                Token {
                    kind: OpenBracket,
                    start: 1,
                    length: 1
                },
                Token {
                    kind: CloseBracket,
                    start: 2,
                    length: 1
                }
            ]
        );
        assert_str_eq!(tokens[0].get_str(&contents), "x");
        assert_str_eq!(tokens[1].get_str(&contents), "(");
        assert_str_eq!(tokens[2].get_str(&contents), ")");
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
        assert_str_eq!(tokens[0].get_str(&contents), "!");
        // The token-izer is not responsible for un-escaping...
        assert_str_eq!(tokens[1].get_str(&contents), "\"hello world\"");
        assert_str_eq!(tokens[2].get_str(&contents), "7");
    }

    #[test]
    fn lex_parentheses() {
        let contents = "(\"hello world\"\n)";
        let tokens = setup_many(contents, 3);
        let expected = vec![OpenParen, StringLit, CloseParen];
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
                .map(|tok| tok.get_str(&contents))
                .collect::<Vec<&str>>(),
            expected_strs
        );
    }

    #[test]
    fn lex_curlies() {
        let contents = "{\"hello world\"\n}";
        let tokens = setup_many(contents, 3);
        let expected = vec![OpenCurly, StringLit, CloseCurly];
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
                .map(|tok| tok.get_str(&contents))
                .collect::<Vec<&str>>(),
            expected_strs
        );
    }

    #[test]
    fn lex_brackets() {
        let contents = "[\"hello world\"\n]";
        let tokens = setup_many(contents, 3);
        let expected = vec![OpenBracket, StringLit, CloseBracket];
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
                .map(|tok| tok.get_str(&contents))
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
                .map(|tok| tok.get_str(&contents))
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
            assert_eq!(
                tokens,
                vec![
                    Token {
                        kind: Op(symbol),
                        start: 0,
                        length: symbol_str.len() as SymbolLength,
                    },
                    Token {
                        kind: NumLit,
                        start: symbol_str.len() as IndexIntoFile,
                        length: 3,
                    },
                ],
                "Failed with operator {}", symbol
            );
            assert_str_eq!(tokens[0].get_str(&contents), symbol_str);
            assert_str_eq!(tokens[1].get_str(&contents), "123");
        }
    }
}
