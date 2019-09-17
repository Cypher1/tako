use std::io::prelude::*;
use std::env;
use std::fs::File;
use std::collections::VecDeque;
use std::iter::FromIterator;

mod tokens;
use tokens::TokenType;
use tokens::Token;

mod tree;
use tree::Tree;

const ERR: &str = "#err";

fn main() -> std::io::Result<()> {
  let all_args: Vec<String> = env::args().collect();
  let args: Vec<String> = all_args[1..].to_vec();

  for f in args {
    work(f)?
  }
  Ok(())
}

fn work(filename: String) -> std::io::Result<()> {
  let mut file = File::open(filename)?;
  let mut contents = String::new();
  file.read_to_string(&mut contents)?;
  // println!("Content: '\n{}'", contents);

  let ast = parse(contents);

  println!("R: {:?}", ast);

  let res = evali32(&ast);
  println!("{}", res);
  // TODO: require left_over is empty
  Ok(())
}

fn bind_infix(tok: &Token) -> i32 {
  match &tok.tok_type {
    TokenType::Op => {
      match tok.value.as_str() {
        "+" => 30,
        "-" => 30,
        "*" => 40,
        "/" => 40,
        "^" => 50,
        _ => 1000,
      }
    },
    TokenType::NumLit => 0,
    _bracket => 0 // TODO impossible
  }
}

fn nud(mut toks: VecDeque<Token>) -> (Tree<Token>, VecDeque<Token>) {
  match toks.pop_front() {
    None => (Tree{value: Token{value: ERR.to_string(), tok_type: TokenType::Error}, children: [].to_vec()}, toks),
    Some(head) => match head.tok_type {
      TokenType::NumLit => (Tree{value: head, children: [].to_vec()}, toks),
      TokenType::Op => {
        let lbp = bind_infix(&head);
        let (right, new_toks) = expr(toks, lbp);
        return (Tree{value: head, children: [right].to_vec()}, new_toks);
      },
      TokenType::Bracket => {
        let (inner, mut new_toks) = expr(toks, 0);
        // TODO require close bracket.
        new_toks.pop_front();
        return (inner, new_toks);
      },
      _ => unimplemented!()
    }
  }
}

fn led(mut toks: VecDeque<Token>, left_branch: Tree<Token>) -> (Tree<Token>, VecDeque<Token>) {
  match toks.pop_front() {
    None => (Tree{value: Token{value: ERR.to_string(), tok_type: TokenType::Error}, children: [].to_vec()}, toks),
    Some(head) => match head.tok_type {
      TokenType::NumLit => (Tree{value: head, children: [].to_vec()}, toks),
      TokenType::Op => {
        let lbp = bind_infix(&head);
        let (right, new_toks) = expr(toks, lbp);
        return (Tree{value: head, children: [left_branch, right].to_vec()}, new_toks);
      },
      _ => unimplemented!()
    }
  }
}

fn expr(init_toks: VecDeque<Token>, init_lbp: i32) -> (Tree<Token>, VecDeque<Token>) {
  // TODO: Name updates fields, this is confusing (0 is tree, 1 is toks)
  let init_update = nud(init_toks);
  let mut left: Tree<Token> = init_update.0;
  let mut toks: VecDeque<Token> = init_update.1;
  loop {
    match toks.front() {
      None => break,
      Some(token) => if init_lbp >= bind_infix(token) {
        break;
      }
    }
    let update = led(toks, left);
    left = update.0;
    toks = update.1;
  }

  return (left, toks);
}

fn evali32(expr: &Tree<Token>) -> i32 {
  match expr.value.tok_type {
    TokenType::Error => {
      return -1; // "#err illegal err".to_string();
    },
    TokenType::Whitespace => {
      return -2; // "#err illegal whitespace".to_string();
    },
    TokenType::Unknown => {
      return -3; // "#err illegal unknown".to_string();
    },
    TokenType::Sym => {
      return -4; // "#err unknown symbol".to_string();
    },
    TokenType::Bracket => {
      // TODO: Require a single child.
      return evali32(&expr.children[1]);
    },
    TokenType::Op => {
      // TODO: require 2 children
      match expr.value.value.as_str() {
        "*" => {
          return expr.children.iter().fold(1, |acc, x| acc * evali32(x))
        },
        "+" => {
          return expr.children.iter().fold(0, |acc, x| acc + evali32(x))
        }
        "/" => {
          // TODO: require divisibility
          return evali32(&expr.children[0]) / evali32(&expr.children[1]);
        }
        "-" => {
          return evali32(&expr.children[0]) - evali32(&expr.children[1]);
        }
        "^" => {
          // TODO: require pos pow
          return i32::pow(evali32(&expr.children[0]), evali32(&expr.children[1]) as u32);
        }
        _x => {
          return -6; // x.to_string()+"?"
        }
      }
    },
    TokenType::NumLit => {
      return expr.value.value.parse().unwrap();
    }
  }
}

fn parse(contents: String) -> Tree<Token> {
  let mut toks: VecDeque<Token> = VecDeque::new();

  let mut chars = VecDeque::from_iter(contents.chars());
  loop {
    let (next, new_chars) = tokens::lex_head(chars);

    // println!("LEXING {:?}", next);

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

  // println!("Toks: {:?}", toks);

  let (root, left_over) = expr(toks, 0);

  if left_over.len() != 0 {
    println!("Oh no");
  }

  return root;
}
