extern crate rand;

use std::io;
use rand::Rng;

fn main() {

    let mut rng = rand::thread_rng();

    println!("Guess the number!");
    let mut guess = 0;
    let target = rng.gen();
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
