use std::fmt;
use crate::string_interner::StrId;
use crate::concepts::File;

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
    Eof,
}

#[derive(Copy, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Token {
    pub start: u32,
    pub tok_type: TokenType,
    pub str_id: StrId,
}

impl Token {
    fn eof() -> Self {
        Self {
            start: 0,
            tok_type: TokenType::Eof,
            str_id: StrId::new(0), // TODO: Validate that 0 is always EOF.
        }
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO: Look up the token to get the contents?
        write!(f, "{:?}({:?} @ {})", self.tok_type, self.str_id, self.start)
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
        '0'..='9' => NumLit,
        '"' | '\'' => StringLit,
        '0'..='9' => NumLit,
        'A'..='Z' | 'a'..='z' | '_' => Sym,
        _ => panic!("Unknown character {}", ch)
    }
}

type Characters<'a> = std::iter::Peekable<
    std::iter::SkipWhile<
        std::iter::Enumerate<std::str::Chars<'a>>,
        impl FnMut(&(usize, char)) -> bool
    >
>;

impl File {
    pub fn start<'a>(&'a self) -> Characters<'a> {
        self.contents
            .chars()
            .enumerate()
            .skip_while(|(_, chr)|
                matches!(chr, '\n'..='\n' | '\r'..='\r' | '\t'..='\t' | ' '..=' ')
            ) // Continue past the character.
            .peekable()
    }

    // Consumes a single token from a Deque of characters.
    pub fn lex_head<'a>(
        &self,
        mut characters: Characters<'a>,
    ) -> (Token, Characters<'a>) {
        let mut start = if let Some((index, _chr)) = characters.peek() {
            *index
        } else {
            return (Token::eof(), characters)
        };

        // TODO: This should be simplified (make tight loops).
        use TokenType::*;
        let mut tok_type: TokenType = Unknown;
        while let Some((_index, chr)) = characters.peek() {
            tok_type = match (&tok_type, classify_char(*chr)) {
                (Unknown, Whitespace) => Unknown, // Ignore
                (Unknown, new_tok_type) => new_tok_type, // Start token.
                (Op, Op) => Op, // Continuation
                (NumLit, NumLit) => NumLit, // Continuation
                (NumLit, Sym) => NumLit, // Number with suffix.
                (Sym, NumLit | Sym) => Sym, // Symbol.
                (_, Whitespace) => break, // Token finished whitespace.
                _ => break, // Token finished can't continue here.
            };
            characters.next(); // Continue past the character.
        }
        if tok_type == StringLit {
            let (index, quote) = characters.peek().expect("String literals should starat with a quote");
            start = *index+1; // skip the quote.
            characters.next();
            while let Some((_index, chr)) = characters.peek() {
                if chr == quote { // reached the end of the quote.
                    break;
                }
                characters.next(); // Add the character.
                if chr == &'\\' {
                    characters.next(); // Read the escaped character.
                };
            }
            // Drop the quote
            characters.next();
        }
        // Token is finished, covers from `start` to `characters`.
        let end = characters.peek().map(|(end, _)| *end).unwrap_or(self.contents.len());
        let comment = self.contents[start..end].starts_with(COMMENT);
        let multi_comment = self.contents[start..end].starts_with(MULTI_COMMENT);
        if !comment && !multi_comment {
            let span = &self.contents[start..end];
            let str_id = self.string_interner.get_or_intern(span);
            return (
                Token {
                    start: start as u32,
                    tok_type,
                    str_id,
                },
                characters,
            );
        }
        // Track depth of mutli line comments
        let mut depth = 1;
        let mut last: Option<char> = None;
        loop {
            characters.next();
            // Add the character.
            match (last, characters.peek().map(|(_, chr)| *chr)) {
                (Some('/'), Some('*')) => {
                    depth += 1;
                }
                (Some('*'), Some('/')) => {
                    depth -= 1;
                    if multi_comment && depth == 0 {
                        characters.next();
                        return self.lex_head(characters);
                    }
                }
                (_, Some(chr)) => {
                    if comment && (chr == '\n' || chr == '\r') {
                        characters.next();
                        return self.lex_head(characters);
                    }
                    last = Some(chr);
                }
                (_, None) => return self.lex_head(characters),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::location::{Loc, Pos};
    use super::classify_char;
    use super::lex_head;
    use super::TokenType::*;

    #[test]
    fn classify_whitespace() {
        assert_eq!(classify_char(' '), Whitespace);
        assert_eq!(classify_char('\n'), Whitespace);
        assert_eq!(classify_char('\r'), Whitespace);
    }

    #[test]
    fn classify_brackets() {
        assert_eq!(classify_char('('), OpenBracket);
        assert_eq!(classify_char(')'), CloseBracket);
        assert_eq!(classify_char('['), OpenBracket);
        assert_eq!(classify_char(']'), CloseBracket);
        assert_eq!(classify_char('{'), OpenBracket);
        assert_eq!(classify_char('}'), CloseBracket);
    }

    #[test]
    fn classify_number() {
        assert_eq!(classify_char('0'), NumLit);
        assert_eq!(classify_char('1'), NumLit);
        assert_eq!(classify_char('2'), NumLit);
    }

    #[test]
    fn lex_number() {
        let chars = "123".chars().peekable();
        let (tok, _) = lex_head(chars);
        assert_eq!(tok.tok_type, NumLit);
    }

    #[test]
    fn lex_symbol() {
        let chars = "a123".chars().peekable();
        let (tok, _) = lex_head(chars);
        assert_eq!(tok.tok_type, Sym);
    }

    #[test]
    fn lex_operator() {
        let chars = "-a123".chars().peekable();
        let (tok, _) = lex_head(chars);
        assert_eq!(tok.tok_type, Op);
    }

    #[test]
    fn lex_num_and_newline_linux() {
        let chars = "\n12".chars().peekable();
        let (tok, _) = lex_head(chars);
        assert_eq!(tok.tok_type, NumLit);
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
        assert_eq!(tok.tok_type, NumLit);
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
        assert_eq!(tok.tok_type, NumLit);
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
        assert_eq!(tok.tok_type, StringLit);
        assert_eq!(tok.value, "\n\t2\r\'\"");
    }

    #[test]
    fn lex_call() {
        let chars = "x()".chars().peekable();
        let (tok, chars2) = lex_head(chars);
        assert_eq!(tok.tok_type, Sym);
        assert_eq!(tok.value, "x");
        let (tok, chars3) = lex_head(chars2);
        assert_eq!(tok.tok_type, OpenBracket);
        assert_eq!(tok.value, "(");
        let (tok, _) = lex_head(chars3);
        assert_eq!(tok.tok_type, CloseBracket);
        assert_eq!(tok.value, ")");
    }

    #[test]
    fn lex_strings_with_operators() {
        let chars = "!\"hello world\"\n7".chars().peekable();
        let (tok, chars2) = lex_head(chars);
        assert_eq!(tok.tok_type, Op);
        assert_eq!(tok.value, "!");
        let (tok, chars3) = lex_head(chars2);
        assert_eq!(tok.tok_type, StringLit);
        assert_eq!(tok.value, "hello world");
        let (tok, _) = lex_head(chars3);
        assert_eq!(tok.tok_type, NumLit);
        assert_eq!(tok.value, "7");
    }
}
