use std::collections::VecDeque;
use std::fmt;

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum TokenType {
    Op,
    Bracket,
    NumLit,
    // Local,
    Sym,
    Unknown,
    Whitespace,
    Error,
}

#[derive(Clone)]
pub struct Token {
    pub tok_type: TokenType,
    pub value: String,
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} {:?}", self.tok_type, self.value)
    }
}

pub const ERR: &str = "#err";
const OPERATORS: &str = "~!@#$%^&*-+=<>|/?.,";
const BRACKETS: &str = "([{}])";
const NUMBERS: &str = "0123456789";
const SYMBOLS: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_";
const WHITESPACE: &str = "\n\r\t ";

fn classify_char(ch: char) -> TokenType {
    if WHITESPACE.contains(ch) {
        return TokenType::Whitespace;
    }
    if OPERATORS.contains(ch) {
        return TokenType::Op;
    }
    if BRACKETS.contains(ch) {
        return TokenType::Bracket;
    }
    if NUMBERS.contains(ch) {
        return TokenType::NumLit;
    }
    if SYMBOLS.contains(ch) {
        return TokenType::Sym;
    }
    return TokenType::Unknown;
}

pub fn lex_head(mut contents: VecDeque<char>) -> (Token, VecDeque<char>) {
    let mut head: VecDeque<char> = VecDeque::new();

    let mut tok_type: TokenType = TokenType::Unknown;

    loop {
        match contents.front() {
            Some(chr) => {
                let chr_type = classify_char(chr.clone());
                tok_type = match (tok_type.clone(), chr_type.clone()) {
                    (TokenType::Unknown, TokenType::Whitespace) => TokenType::Unknown, // Ignore
                    (TokenType::Unknown, new_tok_type) => new_tok_type,
                    (_, TokenType::Whitespace) => break, // Token finished.

                    (TokenType::Op, TokenType::Op) => TokenType::Op,
                    (TokenType::Op, _) => break, // Token finished.

                    (TokenType::NumLit, TokenType::NumLit) => TokenType::NumLit,
                    (TokenType::NumLit, TokenType::Sym) => TokenType::Sym, // Promotion
                    (TokenType::NumLit, _) => break,                       // Token finished.

                    (TokenType::Sym, TokenType::Sym) => TokenType::Sym,
                    (TokenType::Sym, TokenType::NumLit) => TokenType::Sym,
                    (TokenType::Sym, _) => break, // Token finished.

                    (TokenType::Bracket, _) => break, // Token finished.
                    _ => TokenType::Error,            // Can't mix other tokentypes
                };
                if chr_type != TokenType::Whitespace {
                    // Add the character.
                    head.push_back(chr.clone());
                }
                // Continue past the character.
                contents.pop_front();
            }
            None => break,
        }
    }
    let value = head.into_iter().collect();
    return (Token { value, tok_type }, contents);
}
