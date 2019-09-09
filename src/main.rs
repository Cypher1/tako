extern crate rand;

use std::io;
use rand::Rng;
use std::env;
use std::fs::File;
use std::io::prelude::*;

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

    guess();

    Ok(())
}

fn guess() {
    let mut rng = rand::thread_rng();

    println!("Guess the number!");
    let mut guess = 0;
    let target = rng.gen_range(1, 1000);
    let mut count = 0;

    loop {
        println!("Please input your guess.");
        let mut guess_str = String::new();
        io::stdin().read_line(&mut guess_str)
            .expect("Failed to read your guess");

        match guess_str.trim().parse::<i32>() {
            Ok(n) => {
                guess = n;
                println!("You guessed: {}", guess);
            },
            _e => println!("Failed to read your guess (not a number) '{}'", guess_str)
        }
        if guess > target {
            println!("Lower!");
        }
        else if guess < target {
            println!("Higher!");
        } else {
            break;
        }
        count+=1;
    }
    println!("Correct! {} tries", count);
}
