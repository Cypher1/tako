use std::io::prelude::*;
use std::env;
use std::fs::File;

mod tokens;
mod tree;
mod parser;
mod evali32;

use evali32::evali32;

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
