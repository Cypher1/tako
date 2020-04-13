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
    // TODO: Use enum types to convert tokens to literals and symbols.
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
const WHITESPACE: &str = "\n\r\t ";
const QUOTES: &str = "'\"`";
const COMMENT: &str = "//";
const MULTI_COMMENT: &str = "/*";

fn classify_char(ch: char) -> TokenType {
    // TODO: replace this with an array with a value for each character.
    if WHITESPACE.contains(ch) {
        return TokenType::Whitespace;
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
    TokenType::Sym
}

// Consumes a single token from a Deque of characters.
pub fn lex_head<'a>(
    mut contents: std::iter::Peekable<std::str::Chars<'a>>,
    pos: &mut Loc,
) -> (Token, std::iter::Peekable<std::str::Chars<'a>>) {
    let mut head: VecDeque<char> = VecDeque::new();

    let mut tok_type: TokenType = TokenType::Unknown;
    let mut quote: Option<char> = None;
    let start = pos.clone();

    // TODO: This should be simplified (make tight loops).
    while let Some(chr) = contents.peek() {
        let chr_type = classify_char(*chr);
        tok_type = match (&tok_type, &chr_type) {
            (TokenType::Unknown, TokenType::Whitespace) => TokenType::Unknown, // Ignore
            (TokenType::Unknown, TokenType::StringLit) => {
                quote = Some(*chr);
                TokenType::StringLit
            }
            (TokenType::Unknown, new_tok_type) => new_tok_type.clone(),
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
        pos.next(&mut contents);
    }
    if tok_type == TokenType::StringLit {
        // We hit a quote.
        loop {
            pos.next(&mut contents);
            // Add the character.
            if let Some(chr) = contents.peek() {
                if Some(*chr) == quote {
                    break;
                }
                let nxt = if chr == &'\\' {
                    contents.next(); // Escape.
                    let escape = contents.peek().expect("Escaped character");
                    match escape {
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        '0' => '\0',
                        ch => *ch,
                    }
                } else {
                    *contents.peek().expect("Escaped character")
                };
                head.push_back(nxt);
            }
        }
        // Drop the quote
        pos.next(&mut contents);
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
        pos.next(&mut contents);
        // Add the character.
        match (last, &mut contents.peek()) {
            (Some('/'), Some('*')) => {
                depth += 1;
            }
            (Some('*'), Some('/')) => {
                depth -= 1;
                if multi_comment && depth == 0 {
                    pos.next(&mut contents);
                    return lex_head(contents, pos);
                }
            }
            (_, Some(chr)) => {
                if comment && **chr == '\n' {
                    pos.next(&mut contents);
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
        let chars = "123".chars().peekable();
        let mut pos = Loc::default();
        let (tok, _) = lex_head(chars, &mut pos);
        assert_eq!(tok.tok_type, TokenType::NumLit);
    }

    #[test]
    fn lex_symbol() {
        let chars = "a123".chars().peekable();
        let mut pos = Loc::default();
        let (tok, _) = lex_head(chars, &mut pos);
        assert_eq!(tok.tok_type, TokenType::Sym);
    }

    #[test]
    fn lex_operator() {
        let chars = "-a123".chars().peekable();
        let mut pos = Loc::default();
        let (tok, _) = lex_head(chars, &mut pos);
        assert_eq!(tok.tok_type, TokenType::Op);
    }

    #[test]
    fn lex_num_and_newline_linux() {
        let chars = "\n12".chars().peekable();
        let mut pos = Loc::default();
        let (tok, _) = lex_head(chars, &mut pos);
        assert_eq!(tok.tok_type, TokenType::NumLit);
        assert_eq!(
            pos,
            Loc {
                filename: None,
                line: 2,
                col: 3
            }
        );
    }

    #[test]
    fn lex_num_and_newline_windows() {
        let chars = "\r\n12".chars().peekable();
        let mut pos = Loc::default();
        let (tok, _) = lex_head(chars, &mut pos);
        assert_eq!(tok.tok_type, TokenType::NumLit);
        assert_eq!(
            pos,
            Loc {
                filename: None,
                line: 2,
                col: 3
            }
        );
    }

    #[test]
    fn lex_num_and_newline_old_mac() {
        // For mac systems before OSX
        let chars = "\r12".chars().peekable();
        let mut pos = Loc::default();
        let (tok, _) = lex_head(chars, &mut pos);
        assert_eq!(tok.tok_type, TokenType::NumLit);
        assert_eq!(
            pos,
            Loc {
                filename: None,
                line: 2,
                col: 3
            }
        );
    }

    #[test]
    fn lex_escaped_characters_in_string() {
        let chars = "'\\n\\t2\\r\\\'\"'".chars().peekable();
        let mut pos = Loc::default();
        let (tok, _) = lex_head(chars, &mut pos);
        assert_eq!(tok.tok_type, TokenType::StringLit);
        assert_eq!(tok.value, "\n\t2\r\'\"");
    }
}
