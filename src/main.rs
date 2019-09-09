use std::io::prelude::*;
use std::fmt;
use std::env;
use std::fs::File;
use std::collections::VecDeque;
use std::iter::FromIterator;

fn main() -> std::io::Result<()> {
    let all_args: Vec<String> = env::args().collect();
    let args: Vec<String> = all_args[1..].to_vec();

    let mut filename: String = "in.bt".to_string();
    for f in args {
        filename = f;
        break;
    }
    let mut file = File::open(filename)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    println!("Content: '\n{}'", contents);

    let ast = parse(contents);

    println!("R: {:?}", ast);

    Ok(())
}

#[derive(Clone)]
struct Tree<T> {
    value: T,
    children: Vec<Tree<T>>
}

impl<T: fmt::Debug> fmt::Debug for Tree<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({:?}, {:?})", self.value, self.children)
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
enum TokenType {
    Op,
    Bracket,
    NumLit,
    Sym,
    Unknown,
    Whitespace,
    Error
}

#[derive(Clone, Debug)]
struct Token {
    tok_type: TokenType,
    value: String
}

const ERR: &str = "#err";
const OPERATORS: &str = "~!@#$%^&*-+=<>|/?.,";
const BRACKETS: &str = "([{}])";
const NUMBERS: &str = "0123456789";
const SYMBOLS: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_";
const WHITESPACE: &str = "\n\r\t ";

fn classify_char(ch: char) -> TokenType {
    if WHITESPACE.contains(ch) {
        return TokenType::Whitespace
    }
    if OPERATORS.contains(ch) {
        return TokenType::Op
    }
    if BRACKETS.contains(ch) {
        return TokenType::Bracket
    }
    if NUMBERS.contains(ch) {
        return TokenType::NumLit
    }
    if SYMBOLS.contains(ch) {
        return TokenType::Sym
    }
    return TokenType::Unknown
}

fn lex_head(mut contents: VecDeque<char>) -> (Token, VecDeque<char>) {
    let mut head: VecDeque<char> = VecDeque::new();

    let mut tok_type: TokenType = TokenType::Unknown;

    loop {
        match contents.front() {
            Some(chr) => {
                tok_type = match (tok_type.clone(), classify_char(chr.clone())) {
                    (_, TokenType::Whitespace) => break,
                    (TokenType::Unknown, new_tok_type) => new_tok_type,

                    (TokenType::Op, TokenType::Op) => TokenType::Op,
                    (TokenType::Op, _) => break,

                    (TokenType::NumLit, TokenType::NumLit) => TokenType::NumLit,
                    (TokenType::NumLit, TokenType::Sym) => TokenType::Sym, // Promotion
                    (TokenType::NumLit, _) => break,

                    (TokenType::Sym, TokenType::Sym) => TokenType::Sym,
                    (TokenType::Sym, TokenType::NumLit) => TokenType::Sym,
                    (TokenType::Sym, _) => break,

                    (TokenType::Bracket, _) => TokenType::Error,
                    _ => TokenType::Error // Can't mix other tokentypes
                };
                head.push_back(chr.clone()); // Commit to removing the character.
                contents.pop_front();
            },
            None => break
        }
    }
    let value = head.into_iter().collect();
    return (Token{value, tok_type}, contents);
}

fn bind_infix(tok: Token) -> i32 {
    match tok.tok_type {
        TokenType::Op => {
            match tok.value.as_str() {
                "+" => 40,
                "-" => 40,
                "*" => 30,
                "/" => 30,
                "^" => 20,
                _ => 1000,
            }
        },
        TokenType::NumLit => 0,
        _bracket => 0 // TODO impossible
    }
}

fn nud(mut toks: VecDeque<Token>, rbp: i32) -> (Tree<Token>, VecDeque<Token>) {
    match toks.pop_front() {
        None => (Tree{value: Token{value: ERR.to_string(), tok_type: TokenType::Error}, children: [].to_vec()}, toks),
        Some(head) => match head.tok_type {
            TokenType::NumLit => (Tree{value: head, children: [].to_vec()}, toks),
            TokenType::Op => {
                let (right_branch, new_toks) = expr(toks, rbp);
                return (Tree{value: head, children: [right_branch].to_vec()}, new_toks);
            },
            _ => unimplemented!()
        }
    }
}

fn left(mut toks: VecDeque<Token>, left_branch: Tree<Token>, rbp: i32) -> (Tree<Token>, VecDeque<Token>) {
    match toks.pop_front() {
        None => (Tree{value: Token{value: ERR.to_string(), tok_type: TokenType::Error}, children: [].to_vec()}, toks),
        Some(head) => match head.tok_type {
            TokenType::NumLit => (Tree{value: head, children: [].to_vec()}, toks),
            TokenType::Op => {
                let (right_branch, new_toks) = expr(toks, rbp);
                return (Tree{value: head, children: [left_branch, right_branch].to_vec()}, new_toks);
            },
            _ => unimplemented!()
        }
    }
}

fn expr(init_toks: VecDeque<Token>, rbp: i32) -> (Tree<Token>, VecDeque<Token>) {
    // TODO: Name updates fields, this is confusing (0 is tree, 1 is toks)
    let mut update = nud(init_toks, rbp);
    while update.1.len() > 0 {
        update = left(update.1, update.0, rbp);
    }
    return update;
}

fn parse(contents: String) -> Tree<Token> {
    let mut toks: VecDeque<Token> = VecDeque::new();

    let mut chars = VecDeque::from_iter(contents.chars());
    loop {
        let (next, new_chars) = lex_head(chars);

        println!("LEXING {:?}", next);

        if next.tok_type == TokenType::Unknown {
            break // TODO done / skip?
        }

        if next.tok_type == TokenType::Error {
            break // TODO Error
        }

        // If valid, take the token and move on.
        toks.push_back(next);
        chars = new_chars;
    }

    println!("Toks: {:?}", toks);

    let (root, _left_over) = expr(toks, 0);
    // TODO: require left_over is empty
    return root;
}
