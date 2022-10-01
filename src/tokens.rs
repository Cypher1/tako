use crate::string_interner::{StrId, StrInterner};
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

#[inline]
fn classify_char(ch: char) -> TokenType {
    // TODO: replace this with an array with a value for each character.
    use TokenType::*;
    match ch {
        '\n' | '\r' | '\t' | ' ' => Whitespace,
        '~' | '!' | '@' | '#' | '$' | '%' | '^' | '&' | '*' | '-' | '+' | '=' | '<' | '>' | '|'
        | '\\' | '/' | '?' | '.' | ',' | ':' | ';' => Op,
        '(' | '[' | '{' => OpenBracket,
        ')' | ']' | '}' => CloseBracket,
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

pub fn get_str<'a>(string_interner: &'a StrInterner, tok: &Token) -> &'a str {
    string_interner
        .resolve(tok.str_id)
        .expect("Token created using different interner")
}

type ChPred = fn(&'_ (usize, char)) -> bool;

pub struct Characters<'a> {
    s: &'a str,
    it: std::iter::Peekable<std::str::Chars<'a>>,
    index: usize,
    start: usize,
    prev: Option<char>,
}

impl<'a> Characters<'a> {
    fn new(s: &'a str) -> Self {
        Self {
            it: s.chars().peekable(),
            s,
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
    fn set_start(&mut self) -> usize {
        self.start = self.index;
        self.start
    }
    fn index(&self) -> usize {
        self.index
    }
    fn as_str(&self) -> &'a str {
        &self.s[self.start..self.index]
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

// Consumes a single token from a Deque of characters.
pub fn lex_head<'a>(
    _contents: &str,
    string_interner: &mut StrInterner,
    mut characters: Characters<'a>,
) -> (Token, Characters<'a>) {
    while let Some(chr) = characters.peek() {
        // skip whitespace.
        if !is_whitespace(chr) {
            break;
        }
        characters.next();
    }
    /*
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
        match (last, characters.peek().map(|(_, chr)| *chr)) {
            (Some('/'), Some('*')) => {
                depth += 1;
            }
            (Some('*'), Some('/')) => {
                depth -= 1;
                if multi_comment && depth == 0 {
                    characters.next();
                    return lex_head(contents, string_interner, characters);
                }
            }
            (_, Some(chr)) => {
                if comment && (chr == '\n' || chr == '\r') {
                    characters.next();
                    return lex_head(contents, string_interner, characters);
                }
                last = Some(chr);
            }
            (_, None) => return lex_head(contents, string_interner, characters),
        }
    }
    */
    if characters.peek().is_none() {
        return (Token::eof(), characters);
    }
    characters.set_start();
    // TODO: This should be simplified (make tight loops).
    use TokenType::*;
    let mut tok_type: TokenType = Unknown;
    while let Some(chr) = characters.peek() {
        tok_type = match (tok_type, classify_char(chr)) {
            (Unknown, Whitespace) => Unknown,        // Ignore
            (_, Whitespace) => break,                // Token finished whitespace.
            (Unknown, new_tok_type) => new_tok_type, // Start token.
            (Op, Op) => Op,                          // Continuation
            (NumLit, NumLit) => NumLit,              // Continuation
            (NumLit, Sym) => NumLit,                 // Number with suffix.
            (Sym, NumLit | Sym) => Sym,              // Symbol.
            _ => break,                              // Token finished can't continue here.
        };
        characters.next(); // Continue past the character.
    }
    let str_id = if tok_type == StringLit {
        let mut strlit = "".to_string();
        let quote = characters
            .prev()
            .expect("String literals should starat with a quote");
        while let Some(chr) = characters.next() {
            if chr == quote {
                // reached the end of the quote.
                break;
            }
            strlit.push(match chr {
                '\\' => match characters.next() {
                    Some('\\') => '\\',
                    Some('\'') => '\'',
                    Some('\"') => '"',
                    Some('r') => '\r',
                    Some('n') => '\n',
                    Some('t') => '\t',
                    Some('0') => '\0',
                    ch => todo!("escaping for {:?}", ch),
                },
                _ => chr,
            });
        }
        // Drop the quote
        characters.next();
        string_interner.get_or_intern(strlit)
    } else {
        let span = characters.as_str();
        string_interner.get_or_intern(span)
    };
    // TODO: Handle comments.
    (
        Token {
            start: characters.start() as u32,
            tok_type,
            str_id,
        },
        characters,
    )
}

#[cfg(test)]
mod tests {
    use super::TokenType::*;
    use super::*;
    use crate::concepts::File;

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
        let mut file = File::dummy_for_test("123");
        let chars = Characters::new(&file.contents);
        let (tok, _) = lex_head(&file.contents, &mut file.string_interner, chars);
        assert_eq!(tok.tok_type, NumLit);
    }

    #[test]
    fn lex_symbol() {
        let mut file = File::dummy_for_test("a123");
        let chars = Characters::new(&file.contents);
        let (tok, _) = lex_head(&file.contents, &mut file.string_interner, chars);
        assert_eq!(tok.tok_type, Sym);
    }

    #[test]
    fn lex_operator() {
        let mut file = File::dummy_for_test("-a123");
        let chars = Characters::new(&file.contents);
        let (tok, _) = lex_head(&file.contents, &mut file.string_interner, chars);
        assert_eq!(tok.tok_type, Op);
    }

    #[test]
    fn lex_num_and_newline_linux() {
        let mut file = File::dummy_for_test("\n12");
        let chars = Characters::new(&file.contents);
        let (tok, _) = lex_head(&file.contents, &mut file.string_interner, chars);
        assert_eq!(tok.tok_type, NumLit);
        assert_eq!(tok.start, 1);
    }

    #[test]
    fn lex_num_and_newline_windows() {
        let mut file = File::dummy_for_test("\r\n12");
        let chars = Characters::new(&file.contents);
        let (tok, _) = lex_head(&file.contents, &mut file.string_interner, chars);
        assert_eq!(tok.tok_type, NumLit);
        assert_eq!(tok.start, 2);
    }

    #[test]
    fn lex_num_and_newline_old_mac() {
        // For mac systems before OSX
        let mut file = File::dummy_for_test("\r12");
        let chars = Characters::new(&file.contents);
        let (tok, _) = lex_head(&file.contents, &mut file.string_interner, chars);
        assert_eq!(tok.tok_type, NumLit);
        assert_eq!(tok.start, 1);
    }

    #[test]
    fn lex_escaped_characters_in_string() {
        // TODO: De escape them.
        let mut file = File::dummy_for_test("'\\n\\t2\\r\\\'\"'");
        let chars = Characters::new(&file.contents);
        let (tok, _) = lex_head(&file.contents, &mut file.string_interner, chars);
        assert_eq!(tok.tok_type, StringLit);
        assert_str_eq!(get_str(&file.string_interner, &tok), "\n\t2\r\'\"");
    }

    #[test]
    fn lex_call() {
        let mut file = File::dummy_for_test("x()");
        let chars = Characters::new(&file.contents);
        let (tok, chars2) = lex_head(&file.contents, &mut file.string_interner, chars);
        assert_eq!(tok.tok_type, Sym);
        assert_str_eq!(get_str(&file.string_interner, &tok), "x");
        let (tok, chars3) = lex_head(&file.contents, &mut file.string_interner, chars2);
        assert_eq!(tok.tok_type, OpenBracket);
        assert_str_eq!(get_str(&file.string_interner, &tok), "(");
        let (tok, _) = lex_head(&file.contents, &mut file.string_interner, chars3);
        assert_eq!(tok.tok_type, CloseBracket);
        assert_str_eq!(get_str(&file.string_interner, &tok), ")");
    }

    #[test]
    fn lex_strings_with_operators() {
        let mut file = File::dummy_for_test("!\"hello world\"\n7");
        let chars = Characters::new(&file.contents);
        let (tok, chars2) = lex_head(&file.contents, &mut file.string_interner, chars);
        assert_eq!(tok.tok_type, Op);
        assert_str_eq!(get_str(&file.string_interner, &tok), "!");
        let (tok, chars3) = lex_head(&file.contents, &mut file.string_interner, chars2);
        assert_eq!(tok.tok_type, StringLit);
        assert_str_eq!(get_str(&file.string_interner, &tok), "hello world");
        let (tok, _) = lex_head(&file.contents, &mut file.string_interner, chars3);
        assert_eq!(tok.tok_type, NumLit);
        assert_str_eq!(get_str(&file.string_interner, &tok), "7");
    }
}
