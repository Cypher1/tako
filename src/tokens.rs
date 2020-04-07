use super::location::*;
use std::collections::VecDeque;
use std::fmt;

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum TokenType {
    Op,
    OpenBracket,
    CloseBracket,
    NumLit,
    StringLit,
    Sym,
    Unknown,
    Whitespace,
}

#[derive(Clone)]
pub struct Token {
    pub tok_type: TokenType,
    pub value: String,
    pub pos: Loc,
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} {:?}", self.tok_type, self.value)
    }
}

const OPERATORS: &str = "~!@#$%^&*-+=<>|\\/?.,:;";
const OPENBRACKETS: &str = "([{";
const CLOSEBRACKETS: &str = ")]}";
const NUMBERS: &str = "0123456789";
const SYMBOLS: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_";
const WHITESPACE: &str = "\n\r\t ";
const QUOTES: &str = "'\"`";
const COMMENT: &str = "//";
const MULTI_COMMENT: &str = "/*";

fn classify_char(ch: char) -> TokenType {
    if WHITESPACE.contains(ch) {
        return TokenType::Whitespace;
    }
    if SYMBOLS.contains(ch) {
        return TokenType::Sym;
    }
    if OPERATORS.contains(ch) {
        return TokenType::Op;
    }
    if OPENBRACKETS.contains(ch) {
        return TokenType::OpenBracket;
    }
    if CLOSEBRACKETS.contains(ch) {
        return TokenType::CloseBracket;
    }
    if NUMBERS.contains(ch) {
        return TokenType::NumLit;
    }
    if QUOTES.contains(ch) {
        return TokenType::StringLit;
    }
    TokenType::Unknown
}

// Consumes a single token from a Deque of characters.
pub fn lex_head(mut contents: VecDeque<char>, pos: &mut Loc) -> (Token, VecDeque<char>) {
    let mut head: VecDeque<char> = VecDeque::new();

    let mut tok_type: TokenType = TokenType::Unknown;
    let mut quote: Option<char> = None;
    let start = pos.clone();

    // TODO: This should be simplified (make tight loops).
    while let Some(chr) = contents.front() {
        let chr_type = classify_char(*chr);
        tok_type = match (tok_type.clone(), chr_type.clone()) {
            (TokenType::Unknown, TokenType::Whitespace) => TokenType::Unknown, // Ignore
            (TokenType::Unknown, TokenType::StringLit) => {
                quote = Some(*chr);
                TokenType::StringLit
            }
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

            (TokenType::OpenBracket, _) => break, // Token finished.
            (TokenType::CloseBracket, _) => break, // Token finished.
            _unexpected => {
                unimplemented!() // Can't mix other tokentypes
            }
        };
        if chr_type == TokenType::StringLit {
            break;
        }
        if chr_type != TokenType::Whitespace {
            // Add the character.
            head.push_back(chr.clone());
        }
        // Continue past the character.
        pos.next(contents.front());
        contents.pop_front();
    }
    if tok_type == TokenType::StringLit {
        // We hit a quote.
        loop {
            pos.next(contents.front());
            contents.pop_front();
            // Add the character.
            match contents.front() {
                Some(chr) => {
                    if Some(*chr) == quote {
                        break;
                    }
                    head.push_back(chr.clone());
                }
                _ => break,
            }
        }
        // Drop the quote
        pos.next(contents.front());
        contents.pop_front();
    }
    let value = head.into_iter().collect();
    let comment = value == COMMENT;
    let multi_comment = value == MULTI_COMMENT;
    if !comment && !multi_comment {
        return (
            Token {
                value,
                tok_type,
                pos: start,
            },
            contents,
        );
    }
    // Track depth of mutli line comments
    let mut depth = 1;
    let mut last: Option<char> = None;
    loop {
        pos.next(contents.front());
        contents.pop_front();
        // Add the character.
        match (last, &mut contents.front()) {
            (Some('/'), Some('*')) => {
                depth += 1;
            }
            (Some('*'), Some('/')) => {
                depth -= 1;
                if multi_comment && depth == 0 {
                    pos.next(contents.front());
                    contents.pop_front();
                    return lex_head(contents, pos);
                }
            }
            (_, Some(chr)) => {
                if comment && **chr == '\n' {
                    pos.next(contents.front());
                    contents.pop_front();
                    return lex_head(contents, pos);
                }
                last = Some(**chr);
            }
            (_, None) => return lex_head(contents, pos),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::location::*;
    use super::classify_char;
    use super::lex_head;
    use super::TokenType;
    use std::collections::VecDeque;
    use std::iter::FromIterator;

    #[test]
    fn classify_whitespace() {
        assert_eq!(classify_char(' '), TokenType::Whitespace);
        assert_eq!(classify_char('\n'), TokenType::Whitespace);
        assert_eq!(classify_char('\r'), TokenType::Whitespace);
    }

    #[test]
    fn classify_brackets() {
        assert_eq!(classify_char('('), TokenType::OpenBracket);
        assert_eq!(classify_char(')'), TokenType::CloseBracket);
        assert_eq!(classify_char('['), TokenType::OpenBracket);
        assert_eq!(classify_char(']'), TokenType::CloseBracket);
        assert_eq!(classify_char('{'), TokenType::OpenBracket);
        assert_eq!(classify_char('}'), TokenType::CloseBracket);
    }

    #[test]
    fn classify_number() {
        assert_eq!(classify_char('0'), TokenType::NumLit);
        assert_eq!(classify_char('1'), TokenType::NumLit);
        assert_eq!(classify_char('2'), TokenType::NumLit);
    }

    #[test]
    fn lex_number() {
        let chars = VecDeque::from_iter("123".chars());
        let mut pos = Loc::default();
        let (tok, _) = lex_head(chars, &mut pos);
        assert_eq!(tok.tok_type, TokenType::NumLit);
    }

    #[test]
    fn lex_symbol() {
        let chars = VecDeque::from_iter("a123".chars());
        let mut pos = Loc::default();
        let (tok, _) = lex_head(chars, &mut pos);
        assert_eq!(tok.tok_type, TokenType::Sym);
    }

    #[test]
    fn lex_operator() {
        let chars = VecDeque::from_iter("-a123".chars());
        let mut pos = Loc::default();
        let (tok, _) = lex_head(chars, &mut pos);
        assert_eq!(tok.tok_type, TokenType::Op);
    }
}
