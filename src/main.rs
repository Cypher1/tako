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

#[derive(Eq, PartialEq, Debug)]
enum TokenType {
    Op,
    Bracket,
    NumLit,
    Sym,
    Unknown,
    Error
}

#[derive(Debug)]
struct Token {
    tok_type: TokenType,
    value: String
}

const OPERATORS: &str = "~!@#$%^&*-+=<>|/?.,";
const BRACKETS: &str = "([{}])";
const NUMBERS: &str = "0123456789";
const SYMBOLS: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_";

fn classify_char(ch: char) -> TokenType {
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
                tok_type = match (tok_type, classify_char(chr.clone())) {
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

/*fn nud(toks: VecDeque<Token>, rbp: i32) -> (Tree<Token>, VecDeque<Token>) {

}

fn left(toks: VecDeque<Token>, left_branch: Tree<Token>, rbp: i32) -> (Tree<Token>, VecDeque<Token>) {

}*/

fn expr(toks: VecDeque<Token>, rbp: i32) -> (Tree<Token>, VecDeque<Token>) {
    let children: Vec<Token> = Vec::new();
    let mut root = nud(toks, rbp);
    while toks {
        toks = left(toks, root, rbp);
    }
}

fn parse(contents: String) -> Tree<Token> {
    let mut toks: VecDeque<Token> = VecDeque::new();

    let mut chars = VecDeque::from_iter(contents.chars());
    loop {
        let (next, new_chars) = lex_head(chars);

        if next.tok_type == TokenType::Error {
            break // TODO Error
        }

        // If valid, take the token and move on.
        toks.push_back(next);
        chars = new_chars;
    }

    println!("Toks: {:?}", toks);

    let root = expr(toks, 0);

    // let left = Tree{value: "1".to_string(), children: [].to_vec()};
    // let right = Tree{value: "2".to_string(), children: [].to_vec()};
    // let root = Tree{value: "+".to_string(), children: [left, right].to_vec()};
    return root;
}
