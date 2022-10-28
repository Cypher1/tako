use std::collections::VecDeque;
use std::fmt;

#[derive(Copy, Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
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

#[derive(Copy, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
struct Token<'a> {
    pub tok_type: TokenType,
    // this stores both the location and length, in the file and gives a way to get the contents.
    pub value: &'a str, // TODO: Avoid string comparisons.
}

impl<'a> fmt::Debug for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}({:?})", self.tok_type, self.value)
    }
}

const COMMENT: &str = "//";
const MULTI_COMMENT: &str = "/*";

fn classify_char(ch: char) -> TokenType {
    // TODO: replace this with an array with a value for each character.
    use TokenType::*;
    match ch {
        '\n' | '\r' | '\t' | ' ' => Whitespace,
        '~' | '!' | '@' | '#' | '$' | '%' | '^' | '&' | '*' | '-' | '+' | '=' | '<' | '>' | '|' | '\\' | '/' | '?' | '.' | ',' | ':' | ';' => Op,
        '(' | '[' | '{' => OpenBracket,
        ')' | ']' | '}' => CloseBracket,
        '0'..'9' => NumLit,
        '"' | '\'' => StringLit,
        '0'..'9' => NumLit,
        'A'..'Z' | 'a'..'z' | '_' => Sym,
        _ => panic!("Unknown character {}", ch)
    }
}

type Characters<'a> = std::iter::Peekable<std::str::Chars<'a>>;

// Consumes a single token from a Deque of characters.
pub fn lex_head<'a>(
    mut contents: Characters<'a>,
) -> (Token, Characters<'a>) {
    contents = contents.skip_while(|chr| matches!('\n' | '\r' | '\t' | ' ', chr)); // Continue past the character.
    let mut start = contents.as_str();
    // TODO: This should be simplified (make tight loops).
    let mut tok_type: TokenType = TokenType::Unknown;
    while let Some(chr) = contents.peek() {
        tok_type = match (&tok_type, classify_char(*chr)) {
            (TokenType::Unknown, TokenType::Whitespace) => TokenType::Unknown, // Ignore
            (TokenType::Unknown, new_tok_type) => new_tok_type, // Start token.
            (TokenType::Op, TokenType::Op) => TokenType::Op, // Continuation
            (TokenType::NumLit, TokenType::NumLit) => TokenType::NumLit, // Continuation
            (TokenType::NumLit, TokenType::Sym) => TokenType::NumLit, // Number with suffix.
            (TokenType::Sym, TokenType::NumLit | TokenType::Sym) => TokenType::Sym, // Symbol.
            (_, TokenType::Whitespace) => break, // Token finished whitespace.
            _ => break, // Token finished can't continue here.
        };
        contents.next(); // Continue past the character.
    }
    if tok_type == TokenType::StringLit {
        let quote = chr; // We hit a quote.
        contents.next();
        start = contents.as_str(); // start inside the string.
        while let Some(chr) = contents.peek() {
            if *chr == quote { // reached the end of the quote.
                break;
            }
            contents.next(); // Add the character.
            if chr == &'\\' {
                contents.next(); // Read the escaped character.
            };
        }
        // Drop the quote
        contents.next();
    }
    // Token is finished, covers from `start` to `contents`.
    let value = start[0..contents-start];
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
        contents.next();
        // Add the character.
        match (last, &mut contents.peek()) {
            (Some('/'), Some('*')) => {
                depth += 1;
            }
            (Some('*'), Some('/')) => {
                depth -= 1;
                if multi_comment && depth == 0 {
                    contents.next();
                    return lex_head(contents, pos);
                }
            }
            (_, Some(chr)) => {
                if comment && (**chr == '\n' || **chr == '\r') {
                    contents.next();
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
    use super::super::location::{Loc, Pos};
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
        let (tok, _) = lex_head(chars);
        assert_eq!(tok.tok_type, TokenType::NumLit);
    }

    #[test]
    fn lex_symbol() {
        let chars = "a123".chars().peekable();
        let (tok, _) = lex_head(chars);
        assert_eq!(tok.tok_type, TokenType::Sym);
    }

    #[test]
    fn lex_operator() {
        let chars = "-a123".chars().peekable();
        let (tok, _) = lex_head(chars);
        assert_eq!(tok.tok_type, TokenType::Op);
    }

    #[test]
    fn lex_num_and_newline_linux() {
        let chars = "\n12".chars().peekable();
        let (tok, _) = lex_head(chars);
        assert_eq!(tok.tok_type, TokenType::NumLit);
        assert_eq!(
            pos,
            Loc {
                filename: None,
                pos: Pos { line: 2, col: 3 }
            }
        );
    }

    #[test]
    fn lex_num_and_newline_windows() {
        let chars = "\r\n12".chars().peekable();
        let (tok, _) = lex_head(chars);
        assert_eq!(tok.tok_type, TokenType::NumLit);
        assert_eq!(
            pos,
            Loc {
                filename: None,
                pos: Pos { line: 2, col: 3 }
            }
        );
    }

    #[test]
    fn lex_num_and_newline_old_mac() {
        // For mac systems before OSX
        let chars = "\r12".chars().peekable();
        let (tok, _) = lex_head(chars);
        assert_eq!(tok.tok_type, TokenType::NumLit);
        assert_eq!(
            pos,
            Loc {
                filename: None,
                pos: Pos { line: 2, col: 3 }
            }
        );
    }

    #[test]
    fn lex_escaped_characters_in_string() {
        let chars = "'\\n\\t2\\r\\\'\"'".chars().peekable();
        let (tok, _) = lex_head(chars);
        assert_eq!(tok.tok_type, TokenType::StringLit);
        assert_eq!(tok.value, "\n\t2\r\'\"");
    }

    #[test]
    fn lex_call() {
        let chars = "x()".chars().peekable();
        let (tok, chars2) = lex_head(chars);
        assert_eq!(tok.tok_type, TokenType::Sym);
        assert_eq!(tok.value, "x");
        let (tok, chars3) = lex_head(chars2);
        assert_eq!(tok.tok_type, TokenType::OpenBracket);
        assert_eq!(tok.value, "(");
        let (tok, _) = lex_head(chars3);
        assert_eq!(tok.tok_type, TokenType::CloseBracket);
        assert_eq!(tok.value, ")");
    }

    #[test]
    fn lex_strings_with_operators() {
        let chars = "!\"hello world\"\n7".chars().peekable();
        let (tok, chars2) = lex_head(chars);
        assert_eq!(tok.tok_type, TokenType::Op);
        assert_eq!(tok.value, "!");
        let (tok, chars3) = lex_head(chars2);
        assert_eq!(tok.tok_type, TokenType::StringLit);
        assert_eq!(tok.value, "hello world");
        let (tok, _) = lex_head(chars3);
        assert_eq!(tok.tok_type, TokenType::NumLit);
        assert_eq!(tok.value, "7");
    }
}
