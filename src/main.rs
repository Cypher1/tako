use std::env;
use std::fs::File;
use std::io::prelude::*;

mod compi32;
mod evali32;
mod parser;
mod tokens;
mod tree;

use compi32::comp_tree;
use evali32::evali32;

fn main() -> std::io::Result<()> {
    let all_args: Vec<String> = env::args().collect();
    let args: Vec<String> = all_args[1..].to_vec();
    let mut files: Vec<String> = vec![];
    let mut interactive = false;
    for f in args {
        match f.as_str() {
            "-i" => interactive = true,
            _ => files.push(f),
        }
    }
    for f in files {
        work(f, interactive)?
    }
    Ok(())
}

fn work(filename: String, interactive: bool) -> std::io::Result<()> {
    let mut file = File::open(filename)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    // println!("Content: '\n{}'", contents);

    let ast = parser::parse(contents);

    if interactive {
        println!("R: {:?}", ast);

        let res = evali32(&ast);
        println!("{}", res);
        return Ok(());
    }
    let prog = comp_tree(&ast);
    println!("{}", prog);
    // TODO: require left_over is empty
    Ok(())
}
