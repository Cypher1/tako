use super::tokens::*;
use super::tree::Tree;

/*
(module
  (func (export "addTwo") (param i32 i32) (result i32)
    local.get 0                                             local.get 1
    i32.const 3
    i32.mul
    i32.add))
*/

pub fn comp_tree(expr: &Tree<Token>) -> Tree<String> {
    let name = Tree {
        value: "\"addTwo\"".to_string(),
        children: vec![]
    };
    let def = Tree {
        value: "export".to_string(),
        children: vec![name.clone()]
    };
    let node_i32 = Tree {
        value: "i32".to_string(),
        children: vec![]
    };
    let param = Tree {
        value: "param".to_string(),
        children: vec![node_i32.clone(), node_i32.clone()]
    };
    let result = Tree {
        value: "result".to_string(),
        children: vec![node_i32.clone()]
    };
    let mut children = vec![def, param, result];
    let mut body = comp_treei32(&expr);
    children.append(&mut body);
    let func = Tree {
        value: "func".to_string(),
        children: children,
    };
    return Tree {
        value: "module".to_string(),
        children: vec![func],
    }
}

pub fn comp_treei32(expr: &Tree<Token>) -> Vec<Tree<String>> {
    let mut output = Vec::new();
    output.extend(compi32(expr).iter().map(|expr| {
        Tree {
            value: expr.clone(),
            children: vec![],
        }
    }));
    return output;
}

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
        //TokenType::Local => {
            // return vec!["locals.get ".to_string() + &expr.value.value];
        // }
        TokenType::Op => {
            let mut res = Vec::new();
            res.append(&mut compi32(&expr.children[0]));
            res.append(&mut compi32(&expr.children[1]));
            // TODO: require 2 children
            match expr.value.value.as_str() {
                "*" => {
                    res.push("i32.mul".to_string());
                }
                "+" => {
                    res.push("i32.add".to_string());
                }
                "/" => {
                    // TODO: require divisibility
                    res.push("i32.div_s".to_string());
                }
                "-" => {
                    res.push("i32.sub".to_string());
                }
                "^" => {
                    // TODO: require pos pow
                    res.push("i32.pow".to_string());
                }
                unknown => {
                    res.push("?op ".to_string() + &unknown);
                }
            }
            return res;
        }
        TokenType::NumLit => {
            return vec!["i32.const ".to_string() + &expr.value.value];
        }
    }
}
