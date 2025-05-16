use crate::ast::location::{IndexIntoFile, SymbolLength};
use crate::error::TError;
use crate::parser::tokens::{CharacterType, Symbol};
use crate::parser::TokenType;
use better_std::{assert_eq, todo};
use log::debug;

use super::tokens::{classify_char, Token};

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
        self.curr.map_or(0, char::len_utf8)
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
    let mut tokens = Vec::with_capacity(1024); // TODO(perf): Bench mark & tune?
    while lex_head(&mut chars, &mut tokens) {}
    Ok(tokens)
}

// Consumes a single token.
pub fn lex_head(characters: &mut Characters<'_>, tokens: &mut Vec<Token>) -> bool {
    while let Some(chr) = characters.peek() {
        // skip whitespace.
        if !is_whitespace(chr) {
            // TODO(perf): use trim_start
            break;
        }
        characters.next();
    }
    // TODO(usability): Work out a better way of printing pretty spaces.
    use CharacterType::PartialToken;
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
            (
                PartialToken(Symbol::Hash),
                CharacterType::HexSym | PartialToken(Symbol::NumberLit),
            ) => PartialToken(Symbol::ColorLit), // Color Literal.
            (
                PartialToken(Symbol::ColorLit),
                CharacterType::HexSym | PartialToken(Symbol::NumberLit),
            ) => PartialToken(Symbol::ColorLit), // Color Literal.
            (
                CharacterType::HexSym | PartialToken(Symbol::Ident),
                CharacterType::HexSym | PartialToken(Symbol::NumberLit | Symbol::Ident),
            ) => PartialToken(Symbol::Ident), // Symbol.
            (PartialToken(Symbol::NumberLit), PartialToken(Symbol::NumberLit)) => {
                PartialToken(Symbol::NumberLit)
            } // Continuation
            (PartialToken(Symbol::NumberLit), PartialToken(Symbol::Ident)) => {
                PartialToken(Symbol::NumberLit)
            } // Number with suffix.
            (PartialToken(first), PartialToken(second)) => {
                PartialToken(match (first, second) {
                    // Continuation
                    (Symbol::Add, Symbol::Assign) => Symbol::AddAssign,
                    (Symbol::Sub, Symbol::Assign) => Symbol::SubAssign,
                    (Symbol::Sub, Symbol::Gt) => Symbol::Arrow,
                    (Symbol::Div, Symbol::Assign) => Symbol::DivAssign,
                    (Symbol::Div, Symbol::Div) => Symbol::Comment,
                    (Symbol::Div, Symbol::Mul) => Symbol::MultiCommentOpen,
                    (Symbol::Mul, Symbol::Div) => Symbol::MultiCommentClose,
                    (Symbol::Mul, Symbol::Assign) => Symbol::MulAssign,
                    (Symbol::Mul, Symbol::Mul) => Symbol::Exp,
                    (Symbol::Modulo, Symbol::Assign) => Symbol::ModuloAssign,
                    (Symbol::Hash, Symbol::LogicalNot) => Symbol::Shebang,
                    (Symbol::LogicalOr, Symbol::Assign) => Symbol::LogicalOrAssign,
                    (Symbol::LogicalAnd, Symbol::Assign) => Symbol::LogicalAndAssign,
                    (Symbol::And, Symbol::Assign) => Symbol::AndAssign,
                    (Symbol::And, Symbol::And) => Symbol::LogicalAnd,
                    (Symbol::BitXor, Symbol::Assign) => Symbol::BitXorAssign,
                    (Symbol::Or, Symbol::Assign) => Symbol::OrAssign,
                    (Symbol::Or, Symbol::Or) => Symbol::LogicalOr,
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
                })
            }
            _ => break, // Token finished can't continue here.
        };
        characters.next(); // Continue past the character.
    }
    if kind == CharacterType::HexSym {
        kind = PartialToken(Symbol::Ident); // TODO: Re-understand this.
    }
    let kind = if let PartialToken(kind) = kind {
        kind
    } else {
        todo!("Invalid / unfinished token: {kind:?} {characters:?}");
    };
    if kind == Symbol::StringLit {
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
    } else if kind == Symbol::Shebang || kind == Symbol::Comment {
        while let Some(chr) = characters.next() {
            // TODO(perf): use .find
            if chr == '\n' {
                break;
            }
        }
        return lex_head(characters, tokens);
    } else if kind == (Symbol::MultiCommentClose) {
        todo!("Recover from this?");
    } else if kind == (Symbol::MultiCommentOpen) {
        // Track depth of mutli line comments
        let mut depth = 1;
        let mut last: char = ' ';
        while let Some(mut char) = characters.peek() {
            match (last, char) {
                ('/', '*') => {
                    depth += 1;
                }
                ('*', '/') => {
                    depth -= 1;
                    // Drop the '/' as it could be read as a "/*" when its part of "*/*/"
                    char = ' ';
                    if depth == 0 {
                        break;
                    }
                }
                _ => {}
            }
            last = char;
            characters.next();
        }
        characters.next();
        return lex_head(characters, tokens);
    }
    let length = characters.length();
    if length > SymbolLength::MAX as usize {
        assert_eq!(kind, TokenType::StringLit); // TODO(usability): Error here.
        let mut number_of_tokens = length.div_ceil(SymbolLength::MAX as usize);
        if number_of_tokens >= (SymbolLength::MAX as usize) {
            todo!("Token was too long ({length:?}), implement a recursive group thing...");
        }
        tokens.push(Token {
            start: characters.start() as IndexIntoFile,
            length: number_of_tokens as SymbolLength,
            kind: (Symbol::Group),
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

#[inline]
fn is_whitespace(chr: char) -> bool {
    classify_char(chr) == CharacterType::Whitespace
}

#[cfg(test)]
mod tests {
    use super::Symbol::{ColorLit, Ident, NumberLit, StringLit};
    use super::*;
    use better_std::{assert_eq, assert_str_eq};
    use strum::IntoEnumIterator;

    fn setup(contents: &str) -> Vec<Token> {
        lex(contents).expect("Failed parse")
    }

    #[test]
    fn lex_head_number() {
        let tokens = setup("123");
        assert_eq!(
            tokens,
            vec![Token {
                kind: NumberLit,
                start: 0,
                length: 3
            }]
        );
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
            vec![
                Token {
                    kind: (Symbol::Sub),
                    start: 0,
                    length: 1
                },
                Token {
                    kind: Ident,
                    start: 1,
                    length: 4
                }
            ]
        );
    }

    #[test]
    fn lex_head_num_and_newline_linux() {
        let tokens = setup("\n12");
        assert_eq!(
            tokens,
            vec![Token {
                kind: NumberLit,
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
                kind: NumberLit,
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
                kind: NumberLit,
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
        let tokens = setup(contents);
        assert_eq!(
            tokens,
            vec![
                Token {
                    kind: Ident,
                    start: 0,
                    length: 1
                },
                Token {
                    kind: (Symbol::OpenParen),
                    start: 1,
                    length: 1
                },
                Token {
                    kind: (Symbol::CloseParen),
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
        let tokens = setup(contents);
        assert_eq!(
            tokens,
            vec![
                Token {
                    kind: (Symbol::LogicalNot),
                    start: 0,
                    length: 1
                },
                Token {
                    kind: StringLit,
                    start: 1,
                    length: 13
                },
                Token {
                    kind: NumberLit,
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
        let tokens = setup(contents);
        let expected = vec![(Symbol::OpenParen), StringLit, (Symbol::CloseParen)];
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
        let tokens = setup(contents);
        let expected = vec![(Symbol::OpenCurly), StringLit, (Symbol::CloseCurly)];
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
        let tokens = setup(contents);
        let expected = vec![(Symbol::OpenBracket), StringLit, (Symbol::CloseBracket)];
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
        let tokens = setup(contents);
        let expected = vec![Symbol::LogicalNot, StringLit, NumberLit];
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
    fn can_tokenize_comments() {
        // TODO: Multiline comments
        let symbol = Symbol::Comment;
        let contents = format!("{}123", &symbol);
        let tokens = setup(&contents);
        assert_eq!(tokens, vec![], "Should ignore comments and shebangs");
    }

    #[test]
    fn can_tokenize_shebang() {
        let symbol = Symbol::Shebang;
        let contents = format!("{}123", &symbol);
        let tokens = setup(&contents);
        assert_eq!(tokens, vec![], "Should ignore comments and shebangs");
    }

    #[test]
    fn can_tokenize_multicomment() {
        let op = Symbol::MultiCommentOpen;
        let cl = Symbol::MultiCommentClose;
        let contents = format!("{}678{}123", &op, &cl);
        let tokens = setup(&contents);
        let comment_str = format!("{}678{}", &op, &cl);
        let length = comment_str.len();
        assert_eq!(
            tokens,
            vec![Token {
                kind: NumberLit,
                start: length as IndexIntoFile,
                length: 3,
            },]
        );
        assert_str_eq!(tokens[0].get_src(&contents), "123");
    }

    #[test]
    fn can_tokenize_nested_multicomment() {
        let op = Symbol::MultiCommentOpen;
        let cl = Symbol::MultiCommentClose;
        let contents = format!("{op}{op}678{cl}{cl}123");
        let tokens = setup(&contents);
        let comment_str = format!("{op}{op}678{cl}{cl}");
        let length = comment_str.len();
        assert_eq!(
            tokens,
            vec![Token {
                kind: NumberLit,
                start: length as IndexIntoFile,
                length: 3,
            },]
        );
        assert_str_eq!(tokens[0].get_src(&contents), "123");
    }

    #[test]
    fn can_tokenize_color_lits() {
        let symbol = Symbol::Hash;
        let symbol_str = format!("{}", &symbol);
        let contents = format!("{}123", &symbol);
        let tokens = setup(&contents);
        let length = symbol_str.len();

        assert_eq!(
            tokens,
            vec![Token {
                kind: ColorLit,
                start: 0,
                length: (length + 3) as SymbolLength,
            },],
            "Should read #<num> as color"
        );
        assert_str_eq!(tokens[0].get_src(&contents), "#123");
    }

    #[test]
    fn can_tokenize_operators() {
        for kind in Symbol::iter() {
            if matches!(
                kind,
                Symbol::Shebang
                    | Symbol::Comment
                    | Symbol::Hash
                    | Symbol::MultiCommentOpen
                    | Symbol::MultiCommentClose
                    | Symbol::Group
                    | Symbol::Escape
            ) {
                // Special case
                continue;
            }
            let symbol_str = format!("{}", &kind);
            let contents = format!("{}123", &kind);
            let tokens = setup(&contents);
            let length = symbol_str.len();

            assert_eq!(
                tokens,
                vec![
                    Token {
                        kind,
                        start: 0,
                        length: length as SymbolLength,
                    },
                    Token {
                        kind: NumberLit,
                        start: length as IndexIntoFile,
                        length: 3,
                    },
                ],
                "Failed with operator {kind}"
            );
            assert_str_eq!(tokens[0].get_src(&contents), symbol_str);
            assert_str_eq!(tokens[1].get_src(&contents), "123");
        }
    }
}
