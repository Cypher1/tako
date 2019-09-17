use std::collections::VecDeque;
use std::iter::FromIterator;

use super::tree::Tree;

use super::tokens::*;

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

pub fn parse(contents: String) -> Tree<Token> {
  let mut toks: VecDeque<Token> = VecDeque::new();

  let mut chars = VecDeque::from_iter(contents.chars());
  loop {
    let (next, new_chars) = lex_head(chars);

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

