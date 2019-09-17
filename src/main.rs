use std::io::prelude::*;
use std::env;
use std::fs::File;

mod tokens;
use tokens::TokenType;
use tokens::Token;

mod tree;
use tree::Tree;

mod parser;

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

  let ast = parser::parse(contents);

  println!("R: {:?}", ast);

  let res = evali32(&ast);
  println!("{}", res);
  // TODO: require left_over is empty
  Ok(())
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

