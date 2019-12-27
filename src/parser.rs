use std::collections::VecDeque;
use std::iter::FromIterator;

use super::ast::*;
use super::tokens::*;

fn binding_power(tok: &Token) -> (i32, bool) {
    let bind = match &tok.tok_type {
        TokenType::Op => match tok.value.as_str() {
            "]" => 0,
            ")" => 0,
            "}" => 0,
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
        TokenType::NumLit => 10,
        _ => 10, // TODO impossible
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

fn getDefs(root: Node) -> Vec<LetNode> {
    let mut args = vec![];

    match root {
        Node::Let(n) => args.push(n),
        Node::BinOp(BinOpNode{name, left, right}) => {
            if name == "," {
                args.append(&mut getDefs(*left));
                args.append(&mut getDefs(*right));
            } else {
                args.push(LetNode{name: "it".to_string(), value: Some(Box::new(Node::BinOp(BinOpNode{name, left, right})))});
            }
        },
        n => args.push(LetNode{name: "it".to_string(), value: Some(Box::new(n))}),
    }

    return args;
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
            TokenType::CloseBracket => {
                panic!("Unexpected close bracket {}", head.value);
            }
            TokenType::OpenBracket => {
                let (inner, mut new_toks) = expr(toks, 0);
                // TODO require close bracket.
                let close = new_toks.front();
                match (head.value.as_str(), close) {
                    (open, Some(Token{value: close, tok_type: TokenType::CloseBracket})) => {
                        match (open, close.as_str()) {
                            ("(", ")") => {},
                            ("[", "]") => {},
                            ("{", "}") => {},
                            (open, chr) => {
                                panic!(format!("Unexpected closing bracket for {}, found {}.", open, chr));
                            },
                        };
                    },
                    (open, chr) => {
                        panic!("Unclosed bracket {} found {:?}", open, chr);
                    }
                }
                new_toks.pop_front();
                return (inner, new_toks);
            }
            TokenType::Sym => {
                // Handle args.
                return (
                    Node::Sym(head.value),
                    toks,
                );
            },
            TokenType::Unknown | TokenType::Whitespace => panic!("Lexer should not produce unknown or whitespace"),
        },
    }
}

fn led(mut toks: VecDeque<Token>, left: Node) -> (Node, VecDeque<Token>) {
    // println!("here {:?} {:?}", toks, left);
    use Node::*;
    match toks.front() {
        Some(Token{tok_type: TokenType::CloseBracket, value: _}) => {return (Error("Close bracket".to_string()), toks);}
        _ => {}
    }

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
                        Sym(s) => {
                            return (Let(LetNode {
                                name: s,
                                value: Some(Box::new(right)),
                            }), new_toks);
                        },
                        _ => panic!(format!("Cannot assign to {:?}", left))
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
            TokenType::CloseBracket => {
                panic!("Unexpected close bracket");
            }
            TokenType::OpenBracket => {
                let (inner, mut new_toks) = expr(toks, 0);
                // TODO require close bracket.
                let close = new_toks.front();
                match (head.value.as_str(), close) {
                    (open, Some(Token{value: close, tok_type: TokenType::CloseBracket})) => {
                        match (open, close.as_str()) {
                            ("(", ")") => {},
                            ("[", "]") => {},
                            ("{", "}") => {},
                            (open, chr) => {
                                panic!(format!("Unexpected closing bracket for {}, found {}.", open, chr));
                            },
                        };
                    },
                    (open, chr) => {
                        panic!("Unclosed bracket {}, found {:?}", open, chr);
                    }
                }
                new_toks.pop_front();
                // Introduce arguments
                let args = getDefs(inner);
                return (Apply(ApplyNode{inner: Box::new(left), args}), new_toks);
            },
            TokenType::Sym => {
                panic!("Infix symbols not currently supported".to_string());
            }
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
        let update = led(toks, left.clone());
        use Node::*;
        match update {
            (Error(_), new_toks) => { return (left, new_toks); }
            _ => {}
        }
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
