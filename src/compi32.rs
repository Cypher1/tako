use super::tokens::*;
use super::tree::Tree;

pub fn compi32(expr: &Tree<Token>) -> Vec<String> {
    match expr.value.tok_type {
        TokenType::Error => {
            return vec!["#Err".to_string()];
        }
        TokenType::Whitespace => {
            return vec!["#Err whitespace".to_string()];
        }
        TokenType::Unknown => {
            return vec!["#Err unknown".to_string()];
        }
        TokenType::Sym => {
            return vec!["#Err sym".to_string()];
        }
        TokenType::Bracket => {
            return vec!["#Err bracket".to_string()];
        }
        TokenType::Op => {
            let mut res = Vec::new();
            res.append(&mut compi32(&expr.children[0]));
            res.append(&mut compi32(&expr.children[1]));
            // TODO: require 2 children
            match expr.value.value.as_str() {
                "*" => {
                    res.push("mul".to_string());
                }
                "+" => {
                    res.push("add".to_string());
                }
                "/" => {
                    // TODO: require divisibility
                    res.push("idiv".to_string());
                }
                "-" => {
                    res.push("sub".to_string());
                }
                "^" => {
                    // TODO: require pos pow
                    res.push("pow".to_string());
                }
                unknown => {
                    res.push("?op ".to_string() + &unknown);
                }
            }
            return res;
        }
        TokenType::NumLit => {
            return vec!["push ".to_string() + &expr.value.value];
        }
    }
}
