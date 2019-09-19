use super::tree::Tree;
use super::tokens::*;

pub fn compi32(expr: &Tree<Token>) -> Vec<String> {
  match expr.value.tok_type {
    TokenType::Error => {
      return vec!["#Err".to_string()];
    },
    TokenType::Whitespace => {
      return vec!["#Err whitespace".to_string()];
    },
    TokenType::Unknown => {
      return vec!["#Err unknown".to_string()];
    },
    TokenType::Sym => {
      return vec!["#Err sym".to_string()];
    },
    TokenType::Bracket => {
      // TODO: Require a single child.
      return compi32(&expr.children[1]);
    },
    TokenType::Op => {
      let mut res = Vec::new();
      res.append(&mut compi32(&expr.children[0]));
      res.append(&mut compi32(&expr.children[1]));
      // TODO: require 2 children
      match expr.value.value.as_str() {
        "*" => {
          res.push("mul".to_string());
          return res;
        },
        "+" => {
          res.push("add".to_string());
          return res;
        }
        "/" => {
          // TODO: require divisibility
          res.push("div ".to_string());
          return res;
        }
        "-" => {
          res.push("sub".to_string());
          return res;
        }
        "^" => {
          // TODO: require pos pow
          res.push("pow".to_string());
          return res;
        }
        _x => {
          return vec![]; // x.to_string()+"?"
        }
      }
    },
    TokenType::NumLit => {
      return vec!["push ".to_string()+&expr.value.value];
    }
  }
}

