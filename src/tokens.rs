use std::fmt;
use crate::error::TError;
use crate::string_interner::{get_new_interner, StrId, StrInterner};

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
    pub kind: TokenType,
    pub str_id: StrId,
}

impl Token {
    fn eof() -> Self {
        Self {
            start: 0,
            kind: TokenType::Eof,
            // TODO: Validate that 0 is always EOF.
            str_id: get_new_interner()
                .get("")
                .expect("Eof/Empty string must be safely resolved"),
        }
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO: Look up the token to get the contents?
        write!(f, "{:?}({:?} @ {})", self.kind, self.str_id, self.start)
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

// Reads all the tokens.
pub fn lex<'a>(contents: &str, string_interner: &mut StrInterner) -> Result<Vec<Token>, TError> {
    let mut chars = Characters::new(contents);
    let mut tokens = Vec::new();
    loop {
        let (tok, new_chars) = lex_head(contents, string_interner, chars);
        if tok.kind == TokenType::Eof {
            break;
        }
        chars = new_chars;
        tokens.push(tok);
    }
    Ok(tokens)
}

// Consumes a single token.
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
    let mut kind: TokenType = Unknown;
    while let Some(chr) = characters.peek() {
        kind = match (kind, classify_char(chr)) {
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
    let str_id = if kind == StringLit {
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
            kind,
            str_id,
        },
        characters,
    )
}

#[cfg(test)]
mod tests {
    use super::TokenType::*;
    use super::*;

    fn setup(contents: &str) -> (String, StrInterner) {
        (
            contents.to_string(),
            string_interner: get_new_interner(),
        )
    }

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
    fn lex_head_number() {
        let (mut contents, mut string_interner) = setup("123");
        let chars = Characters::new(contents);
        let (tok, _) = lex_head(contents, &mut string_interner, chars);
        assert_eq!(tok.kind, NumLit);
    }

    #[test]
    fn lex_head_symbol() {
        let (mut contents, mut string_interner) = setup("a123");
        let chars = Characters::new(contents);
        let (tok, _) = lex_head(contents, &mut string_interner, chars);
        assert_eq!(tok.kind, Sym);
    }

    #[test]
    fn lex_head_operator() {
        let (mut contents, mut string_interner) = setup("-a123");
        let chars = Characters::new(contents);
        let (tok, _) = lex_head(contents, &mut string_interner, chars);
        assert_eq!(tok.kind, Op);
    }

    #[test]
    fn lex_head_num_and_newline_linux() {
        let (mut contents, mut string_interner) = setup("\n12");
        let chars = Characters::new(contents);
        let (tok, _) = lex_head(contents, &mut string_interner, chars);
        assert_eq!(tok.kind, NumLit);
        assert_eq!(tok.start, 1);
    }

    #[test]
    fn lex_head_num_and_newline_windows() {
        let (mut contents, mut string_interner) = setup("\r\n12");
        let chars = Characters::new(contents);
        let (tok, _) = lex_head(contents, &mut string_interner, chars);
        assert_eq!(tok.kind, NumLit);
        assert_eq!(tok.start, 2);
    }

    #[test]
    fn lex_head_num_and_newline_old_mac() {
        // For mac systems before OSX
        let (mut contents, mut string_interner) = setup("\r12");
        let chars = Characters::new(contents);
        let (tok, _) = lex_head(contents, &mut string_interner, chars);
        assert_eq!(tok.kind, NumLit);
        assert_eq!(tok.start, 1);
    }

    #[test]
    fn lex_head_escaped_characters_in_string() {
        // TODO: De escape them.
        let (mut contents, mut string_interner) = setup("'\\n\\t2\\r\\\'\"'");
        let chars = Characters::new(contents);
        let (tok, _) = lex_head(contents, &mut string_interner, chars);
        assert_eq!(tok.kind, StringLit);
        assert_str_eq!(get_str(&string_interner, &tok), "\n\t2\r\'\"");
    }

    #[test]
    fn lex_head_call() {
        let (mut contents, mut string_interner) = setup("x()");
        let chars = Characters::new(contents);
        let (tok, chars2) = lex_head(contents, &mut string_interner, chars);
        assert_eq!(tok.kind, Sym);
        assert_str_eq!(get_str(&string_interner, &tok), "x");
        let (tok, chars3) = lex_head(contents, &mut string_interner, chars2);
        assert_eq!(tok.kind, OpenBracket);
        assert_str_eq!(get_str(&string_interner, &tok), "(");
        let (tok, _) = lex_head(contents, &mut string_interner, chars3);
        assert_eq!(tok.kind, CloseBracket);
        assert_str_eq!(get_str(&string_interner, &tok), ")");
    }

    #[test]
    fn lex_head_strings_with_operators() {
        let (mut contents, mut string_interner) = setup("!\"hello world\"\n7");
        let chars = Characters::new(contents);
        let (tok, chars2) = lex_head(contents, &mut string_interner, chars);
        assert_eq!(tok.kind, Op);
        assert_str_eq!(get_str(&string_interner, &tok), "!");
        let (tok, chars3) = lex_head(contents, &mut string_interner, chars2);
        assert_eq!(tok.kind, StringLit);
        assert_str_eq!(get_str(&string_interner, &tok), "hello world");
        let (tok, _) = lex_head(contents, &mut string_interner, chars3);
        assert_eq!(tok.kind, NumLit);
        assert_str_eq!(get_str(&string_interner, &tok), "7");
    }

    #[test]
    fn lex_parentheses() {
        let (mut contents, mut string_interner) = setup("(\"hello world\"\n)");
        let tokens = lex(&mut contents, &mut string_interner).expect("Couldn't lex");

        let interner = &string_interner;
        let expected = vec![OpenBracket, StringLit, CloseBracket];
        assert_eq!(
            tokens
                .iter()
                .map(|tok| tok.kind)
                .collect::<Vec<TokenType>>(),
            expected
        );
        let expected_strs = vec!["(", "hello world", ")"];
        assert_eq!(
            tokens
                .iter()
                .map(|tok| get_str(&interner, tok))
                .collect::<Vec<&str>>(),
            expected_strs
        );
    }

    #[test]
    fn lex_strings_with_operators() {
        let (mut contents, mut string_interner) = setup("!\"hello world\"\n7");
        let tokens = lex(&mut contents, &mut string_interner).expect("Couldn't lex");

        let interner = &string_interner;
        let expected = vec![Op, StringLit, NumLit];
        assert_eq!(
            tokens
                .iter()
                .map(|tok| tok.kind)
                .collect::<Vec<TokenType>>(),
            expected
        );
        let expected_strs = vec!["!", "hello world", "7"];
        assert_eq!(
            tokens
                .iter()
                .map(|tok| get_str(&interner, tok))
                .collect::<Vec<&str>>(),
            expected_strs
        );
    }
}
