use super::tokens::*;
use super::tree::Tree;

pub fn evali32(expr: &Tree<Token>) -> i32 {
    match expr.value.tok_type {
        TokenType::Error => {
            return -1; // "#err illegal err".to_string();
        }
        TokenType::Whitespace => {
            return -2; // "#err illegal whitespace".to_string();
        }
        TokenType::Unknown => {
            return -3; // "#err illegal unknown".to_string();
        }
        TokenType::Sym => {
            return -4; // "#err unknown symbol".to_string();
        }
        TokenType::Bracket => {
            // TODO: Require a single child.
            return evali32(&expr.children[1]);
        }
        TokenType::Op => {
            // TODO: require 2 children
            match expr.value.value.as_str() {
                "*" => return expr.children.iter().fold(1, |acc, x| acc * evali32(x)),
                "+" => return expr.children.iter().fold(0, |acc, x| acc + evali32(x)),
                "/" => {
                    // TODO: require divisibility
                    return evali32(&expr.children[0]) / evali32(&expr.children[1]);
                }
                "-" => {
                    return evali32(&expr.children[0]) - evali32(&expr.children[1]);
                }
                "^" => {
                    // TODO: require pos pow
                    return i32::pow(
                        evali32(&expr.children[0]),
                        evali32(&expr.children[1]) as u32,
                    );
                }
                _x => {
                    return -6; // x.to_string()+"?"
                }
            }
        }
        TokenType::NumLit => {
            return expr.value.value.parse().unwrap();
        }
    }
}
