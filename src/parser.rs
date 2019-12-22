use std::collections::VecDeque;
use std::iter::FromIterator;

use super::ast::*;
use super::tokens::*;

fn binding_power(tok: &Token) -> (i32, bool) {
    let bind = match &tok.tok_type {
        TokenType::Op => match tok.value.as_str() {
            ";" => 20,
            "," => 30,
            "=" => 40,
            "<" => 50,
            "<=" => 50,
            ">" => 50,
            ">=" => 50,
            "!=" => 50,
            "==" => 50,
            "&&" => 60,
            "||" => 60,
            "^" => 70,
            "+" => 80,
            "-" => 80,
            "*" => 90,
            "/" => 90,
            "%" => 90,
            "." => 100,
            "[" => 110,
            "(" => 110,
            "{" => 110,
            _ => panic!("Unknown operator"),
        },
        TokenType::NumLit => 0,
        _ => 0, // TODO impossible
    };
    let assoc_right = match &tok.tok_type {
        TokenType::Op => match tok.value.as_str() {
            "^" => true,
            "=" => true,
            _ => false,
        },
        _ => false
    };
    return (bind, assoc_right);
}

fn nud(mut toks: VecDeque<Token>) -> (Node, VecDeque<Token>) {
    match toks.pop_front() {
        None => (Node::Error("Unexpected eof, expected expr".to_string()), toks),
        Some(head) => match head.tok_type {
            TokenType::NumLit => (Node::Prim(PrimValue::I32(head.value.parse().unwrap())), toks),
            TokenType::StringLit => (Node::Prim(PrimValue::Str(head.value)), toks),
            TokenType::Op => {
                let (lbp, _) = binding_power(&head);
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
                // Handle args.
                return (
                    Node::Call(CallNode {
                            name: head.value,
                            args: vec![],
                    }),
                    toks,
                );
            },
            TokenType::Unknown | TokenType::Whitespace => panic!("Lexer should not produce unknown or whitespace"),
        },
    }
}

fn led(mut toks: VecDeque<Token>, left: Node) -> (Node, VecDeque<Token>) {
    use Node::*;
    match toks.pop_front() {
        None => (Error("Unexpected eof, expected expr tail".to_string()), toks),
        Some(head) => match head.tok_type {
            TokenType::NumLit => (Prim(PrimValue::I32(head.value.parse().unwrap())), toks),
            TokenType::StringLit => (Prim(PrimValue::Str(head.value)), toks),
            TokenType::Op => {
                let (lbp, assoc_right) = binding_power(&head);
                let (right, new_toks) = expr(toks, lbp - if assoc_right {1} else {0});
                if head.value == "=".to_string() {
                     match left {
                        Call(n) => return (Let(LetNode {
                            call: n,
                            value: Some(Box::new(right)),
                        }), new_toks),
                        _ => panic!(format!("Expected a definition, found {:?}", left)),
                    }
                }
                return (
                    BinOp(BinOpNode {
                        name: head.value,
                        left: Box::new(left),
                        right: Box::new(right),
                    }),
                    new_toks,
                );
            },
            TokenType::Bracket => (Error("Array style indexing not currently supported".to_string()), toks),
            TokenType::Sym => (Error("Infix symbols not currently supported".to_string()), toks),
            TokenType::Unknown | TokenType::Whitespace => panic!("Lexer should not produce unknown or whitespace"),
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
                let (lbp, _) = binding_power(token);
                if init_lbp >= lbp {
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
        panic!("Oh no: Left over tokens {:?}", left_over);
    }

    return root;
}

#[cfg(test)]
mod tests {
    use super::parse;
    use super::super::ast::*;
    use Node::*;
    use PrimValue::*;

    fn num_lit(x: i32) -> Box<Node> {
        Box::new(Prim(I32(x)))
    }

    fn str_lit(x: String) -> Box<Node> {
        Box::new(Prim(Str(x)))
    }

    #[test]
    fn parse_num() {
        assert_eq!(parse("12".to_string()), Prim(I32(12)));
    }

    #[test]
    fn parse_str() {
        assert_eq!(parse("\"hello world\"".to_string()),
            Prim(Str("hello world".to_string())));
    }

    #[test]
    fn parse_un_op() {
        assert_eq!(parse("-12".to_string()), UnOp(UnOpNode {name: "-".to_string(),
       inner: Box::new(Prim(I32(12)))}));
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
    fn parse_add_mul_precedence() {
        assert_eq!(parse("3+2*4".to_string()),
        Node::BinOp(BinOpNode {
            name: "+".to_string(),
            left: num_lit(3),
            right: Box::new(
                Node::BinOp(BinOpNode {
                    name: "*".to_string(),
                    left: num_lit(2),
                    right: num_lit(4)
                })
            )
        }));
    }

    #[test]
    fn parse_mul_add_precedence() {
        assert_eq!(parse("3*2+4".to_string()),
        Node::BinOp(BinOpNode {
            name: "+".to_string(),
            left: Box::new(
                Node::BinOp(BinOpNode {
                    name: "*".to_string(),
                    left: num_lit(3),
                    right: num_lit(2)
                })
            ),
            right: num_lit(4),
        }));
    }

    #[test]
    fn parse_mul_add_parens() {
        assert_eq!(parse("3*(2+4)".to_string()),
        Node::BinOp(BinOpNode {
            name: "*".to_string(),
            left: num_lit(3),
            right: Box::new(
                Node::BinOp(BinOpNode {
                    name: "+".to_string(),
                    left: num_lit(2),
                    right: num_lit(4)
                })
            )
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
