use std::collections::VecDeque;
use std::iter::FromIterator;

use super::ast::*;
use super::tokens::*;

fn bind_infix(tok: &Token) -> i32 {
    match &tok.tok_type {
        TokenType::Op => match tok.value.as_str() {
            "+" => 30,
            "-" => 30,
            "*" => 40,
            "/" => 40,
            "^" => 50,
            ";" => 2000,
            _ => 1000,
        },
        TokenType::NumLit => 0,
        _ => 0, // TODO impossible
    }
}

fn nud(mut toks: VecDeque<Token>) -> (Node, VecDeque<Token>) {
    match toks.pop_front() {
        None => (Node::Error("Unexpected eof, expected expr".to_string()), toks),
        Some(head) => match head.tok_type {
            TokenType::NumLit => (Node::Num(head.value.parse().unwrap()), toks),
            TokenType::StringLit => (Node::Str(head.value), toks),
            TokenType::Op => {
                let lbp = bind_infix(&head);
                let (right, new_toks) = expr(toks, lbp);
                return (
                    Node::UnOp(UnOpNode {
                        name: head.value,
                        inner: Box::new(right),
                    }),
                    new_toks,
                );
            }
            TokenType::Bracket => {
                let (inner, mut new_toks) = expr(toks, 0);
                // TODO require close bracket.
                new_toks.pop_front();
                return (inner, new_toks);
            }
            TokenType::Sym => {
                return (
                    Node::Call(CallNode {
                        name: head.value,
                    }),
                    toks,
                );
            }
            _ => unimplemented!(),
        },
    }
}

fn led(mut toks: VecDeque<Token>, left_branch: Node) -> (Node, VecDeque<Token>) {
    match toks.pop_front() {
        None => (Node::Error("Unexpected eof, expected expr tail".to_string()), toks),
        Some(head) => match head.tok_type {
            TokenType::NumLit => (Node::Num(head.value.parse().unwrap()), toks),
            TokenType::StringLit => (Node::Str(head.value), toks),
            TokenType::Op => {
                let lbp = bind_infix(&head);
                let (right, new_toks) = expr(toks, lbp);
                return (
                    Node::BinOp(BinOpNode {
                        name: head.value,
                        left: Box::new(left_branch),
                        right: Box::new(right),
                    }),
                    new_toks,
                );
            }
            _ => unimplemented!(),
        },
    }
}

fn expr(init_toks: VecDeque<Token>, init_lbp: i32) -> (Node, VecDeque<Token>) {
    // TODO: Name updates fields, this is confusing (0 is tree, 1 is toks)
    let init_update = nud(init_toks);
    let mut left: Node = init_update.0;
    let mut toks: VecDeque<Token> = init_update.1;
    loop {
        match toks.front() {
            None => break,
            Some(token) => {
                if init_lbp >= bind_infix(token) {
                    break;
                }
            }
        }
        let update = led(toks, left);
        left = update.0;
        toks = update.1;
    }

    return (left, toks);
}

pub fn parse(contents: String) -> Node {
    let mut toks: VecDeque<Token> = VecDeque::new();

    let mut chars = VecDeque::from_iter(contents.chars());
    loop {
        let (next, new_chars) = lex_head(chars);

        // println!("LEXING {:?}", next);

        if next.tok_type == TokenType::Unknown {
            break; // TODO done / skip?
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

#[cfg(test)]
mod tests {
    use super::parse;
    use super::super::ast::*;

    fn num_lit(x: i32) -> Box<Node> {
        Box::new(Node::Num(x))
    }

    fn str_lit(x: String) -> Box<Node> {
        Box::new(Node::Str(x))
    }

    #[test]
    fn parse_num() {
        assert_eq!(parse("12".to_string()), Node::Num(12));
    }

    #[test]
    fn parse_str() {
        assert_eq!(parse("\"hello world\"".to_string()),
            Node::Str("hello world".to_string()));
    }

    #[test]
    fn parse_un_op() {
        assert_eq!(parse("-12".to_string()), Node::UnOp(UnOpNode {name: "-".to_string(),
       inner: Box::new(Node::Num(12))}));
    }

    #[test]
    fn parse_min_op() {
        assert_eq!(parse("14-12".to_string()),
        Node::BinOp(BinOpNode {
            name: "-".to_string(),
            left: num_lit(14),
            right: num_lit(12)
        }));
    }

    #[test]
    fn parse_mul_op() {
        assert_eq!(parse("14*12".to_string()),
        Node::BinOp(BinOpNode {
            name: "*".to_string(),
            left: num_lit(14),
            right: num_lit(12)
        }));
    }

    #[test]
    fn parse_add_str() {
        assert_eq!(parse("\"hello\"+\" world\"".to_string()),
            Node::BinOp(BinOpNode {
               name: "+".to_string(),
                left: str_lit("hello".to_string()),
                right: str_lit(" world".to_string())
            }));
    }

}
