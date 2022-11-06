use crate::error::TError;
use crate::location::{IndexIntoFile, SymbolLength};
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

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum Source {
    // Most of the time we do the fast, non-owning thing.
    Symbol(SymbolLength),
    Lit(String), // Sometimes we have to hold on to a new value.
}

#[derive(Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Token {
    pub start: IndexIntoFile,
    pub kind: TokenType,
    pub source: Source,
}

impl Token {
    fn eof() -> Self {
        Self {
            start: 0,
            kind: TokenType::Eof,
            source: Source::Symbol(0), // zero characters == empty str.
        }
    }

    #[allow(dead_code)] // TODO:: REMOVE!
    fn get_str<'a>(&'a self, source: &'a str) -> &'a str {
        // Assuming the token is from the source file...
        match &self.source {
            Source::Symbol(length) => {
                &source[self.start as usize..self.start as usize + *length as usize]
            }
            Source::Lit(string) => string,
        }
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO: Look up the token to get the contents?
        write!(f, "{:?}({:?} @ {})", self.kind, self.source, self.start)
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
    let mut chars = Characters::new(contents);
    let mut tokens = Vec::new();
    loop {
        let (tok, new_chars) = lex_head(chars);
        if tok.kind == TokenType::Eof {
            break;
        }
        chars = new_chars;
        tokens.push(tok);
    }
    Ok(tokens)
}

// Consumes a single token.
pub fn lex_head(
    mut characters: Characters,
) -> (Token, Characters) {
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
    let source = if kind == StringLit {
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
        Source::Lit(strlit)
    } else {
        // This should find the offset into the source
        Source::Symbol(
            characters
                .index()
                .checked_sub(characters.start())
                .expect("Token should finish after it starts") as SymbolLength,
        )
    };
    // TODO: Handle comments.
    (
        Token {
            start: characters.start() as u32,
            kind,
            source,
        },
        characters,
    )
}

#[cfg(test)]
mod tests {
    use super::TokenType::*;
    use super::*;

    fn setup(contents: &str) -> String {
        contents.to_string()
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
        let contents = setup("123");
        let chars = Characters::new(&contents);
        let (tok, _) = lex_head(chars);
        assert_eq!(tok.kind, NumLit);
    }

    #[test]
    fn lex_head_symbol() {
        let contents = setup("a123");
        let chars = Characters::new(&contents);
        let (tok, _) = lex_head(chars);
        assert_eq!(tok.kind, Sym);
    }

    #[test]
    fn lex_head_operator() {
        let contents = setup("-a123");
        let chars = Characters::new(&contents);
        let (tok, _) = lex_head(chars);
        assert_eq!(tok.kind, Op);
    }

    #[test]
    fn lex_head_num_and_newline_linux() {
        let contents = setup("\n12");
        let chars = Characters::new(&contents);
        let (tok, _) = lex_head(chars);
        assert_eq!(tok.kind, NumLit);
        assert_eq!(tok.start, 1);
    }

    #[test]
    fn lex_head_num_and_newline_windows() {
        let contents = setup("\r\n12");
        let chars = Characters::new(&contents);
        let (tok, _) = lex_head(chars);
        assert_eq!(tok.kind, NumLit);
        assert_eq!(tok.start, 2);
    }

    #[test]
    fn lex_head_num_and_newline_old_mac() {
        // For mac systems before OSX
        let contents = setup("\r12");
        let chars = Characters::new(&contents);
        let (tok, _) = lex_head(chars);
        assert_eq!(tok.kind, NumLit);
        assert_eq!(tok.start, 1);
    }

    #[test]
    fn lex_head_escaped_characters_in_string() {
        // TODO: De escape them.
        let contents = setup("'\\n\\t2\\r\\\'\"'");
        let chars = Characters::new(&contents);
        let (tok, _) = lex_head(chars);
        assert_eq!(tok.kind, StringLit);
        assert_str_eq!(tok.get_str(&contents), "\n\t2\r\'\"");
    }

    #[test]
    fn lex_head_call() {
        let contents = setup("x()");
        let chars = Characters::new(&contents);
        let (tok, chars2) = lex_head(chars);
        assert_eq!(tok.kind, Sym);
        assert_str_eq!(tok.get_str(&contents), "x");
        let (tok, chars3) = lex_head(chars2);
        assert_eq!(tok.kind, OpenBracket);
        assert_str_eq!(tok.get_str(&contents), "(");
        let (tok, _) = lex_head(chars3);
        assert_eq!(tok.kind, CloseBracket);
        assert_str_eq!(tok.get_str(&contents), ")");
    }

    #[test]
    fn lex_head_strings_with_operators() {
        let contents = setup("!\"hello world\"\n7");
        let chars = Characters::new(&contents);
        let (tok, chars2) = lex_head(chars);
        assert_eq!(tok.kind, Op);
        assert_str_eq!(tok.get_str(&contents), "!");
        let (tok, chars3) = lex_head(chars2);
        assert_eq!(tok.kind, StringLit);
        assert_str_eq!(tok.get_str(&contents), "hello world");
        let (tok, _) = lex_head(chars3);
        assert_eq!(tok.kind, NumLit);
        assert_str_eq!(tok.get_str(&contents), "7");
    }

    #[test]
    fn lex_parentheses() {
        let contents = setup("(\"hello world\"\n)");
        let tokens = lex(&contents).expect("Couldn't lex");

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
                .map(|tok| tok.get_str(&contents))
                .collect::<Vec<&str>>(),
            expected_strs
        );
    }

    #[test]
    fn lex_strings_with_operators() {
        let contents = setup("!\"hello world\"\n7");
        let tokens = lex(&contents).expect("Couldn't lex");

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
                .map(|tok| tok.get_str(&contents))
                .collect::<Vec<&str>>(),
            expected_strs
        );
    }
}
