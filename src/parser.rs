use std::collections::VecDeque;
use std::iter::FromIterator;

use super::ast::*;
use super::tokens::*;
use super::location::*;

fn binding_power(tok: &Token) -> (i32, bool) {
    let bind = match &tok.tok_type {
        TokenType::Op => match tok.value.as_str() {
            ";" => 20,
            "," => 30,
            "=" => 40,
            ":" => 42,
            "?" => 45,
            "-|" => 47,
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
            "!" => 80,
            "*" => 90,
            "/" => 90,
            "%" => 90,
            "." => 100,
            "(" => 110,
            ")" => 110,
            "[" => 110,
            "]" => 110,
            "{" => 110,
            "}" => 110,
            op => panic!(format!("Unknown operator {}", op)),
        },
        TokenType::NumLit => 1000,
        _ => 1000, // TODO impossible
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

fn get_defs(root: Node) -> Vec<Let> {
    use Node::*;
    let mut args = vec![];

    match root {
        LetNode(n) => args.push(n),
        BinOpNode(BinOp{name, left, right, info}) => {
            if name == "," {
                args.append(&mut get_defs(*left));
                args.append(&mut get_defs(*right));
            } else {
                args.push(Let{name: "it".to_string(),
                    requires: None, value: Box::new(BinOp{name, left, right, info: info.clone()}.to_node()), info});
            }
        },
        n => args.push(Let{name: "it".to_string(), requires: None, value: Box::new(n.clone()), info: n.get_info()}),
    }

    return args;
}

impl Token {
    pub fn get_info(&self) -> Info {
        Info {loc: Some(self.pos.clone()), ty: None}
    }
}

impl Loc {
    pub fn get_info(self) -> Info {
        Info {loc: Some(self), ty: None}
    }
}

fn nud(mut toks: VecDeque<Token>) -> (Node, VecDeque<Token>) {
    match toks.pop_front() {
        None => (Err{msg: "Unexpected eof, expected expr".to_string(), info: Info::default()}.to_node(), toks),
        Some(head) => match head.tok_type {
            TokenType::NumLit => (Prim::I32(head.value.parse().unwrap(), head.get_info()).to_node(), toks),
            TokenType::StringLit => (Prim::Str(head.value.clone(), head.get_info()).to_node(), toks),
            TokenType::Op => {
                let (lbp, _) = binding_power(&head);
                let (right, new_toks) = expr(toks, lbp);
                return (
                    UnOp {
                        name: head.value.clone(),
                        inner: Box::new(right),
                        info: head.get_info(),
                    }.to_node(),
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
                    (open, Some(Token{value: close, tok_type: TokenType::CloseBracket, pos})) => {
                        match (open, close.as_str()) {
                            ("(", ")") => {},
                            ("[", "]") => {},
                            ("{", "}") => {},
                            (open, chr) => {
                                panic!(format!("Unexpected closing bracket for {}, found {} at {:?}.", open, chr, pos));
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
                    Sym{name: head.value.clone(), depth: None, info: head.get_info()}.to_node(),
                    toks,
                );
            },
            TokenType::Unknown | TokenType::Whitespace => panic!("Lexer should not produce unknown or whitespace"),
        },
    }
}

fn led(mut toks: VecDeque<Token>, left: Node) -> (Node, VecDeque<Token>) {
    // println!("here {:?} {:?}", toks, left);
    match toks.front() {
        Some(Token{tok_type: TokenType::CloseBracket, value: _, pos}) => {return (Err{msg: "Close bracket".to_string(), info: pos.clone().get_info()}.to_node(), toks);}
        _ => {}
    }

    match toks.pop_front() {
        None => (Err{msg: "Unexpected eof, expected expr tail".to_string(), info: left.get_info()}.to_node(), toks),
        Some(head) => match head.tok_type {
            TokenType::NumLit => (Prim::I32(head.value.parse().unwrap(), head.get_info()).to_node(), toks),
            TokenType::StringLit => (Prim::Str(head.value.clone(), head.get_info()).to_node(), toks),
            TokenType::Sym => {
                toks.push_front(Token{tok_type: TokenType::Op, value: ";".to_string(), pos: head.pos.clone()});
                toks.push_front(head);
                return led(toks, left);
            },
            TokenType::Op => {
                let (lbp, assoc_right) = binding_power(&head);
                let (right, new_toks) = expr(toks, lbp - if assoc_right {1} else {0});
                if head.value == "=".to_string() {
                    match left {
                        Node::SymNode(s) => {
                            return (Let {
                                name: s.name,
                                requires: None,
                                value: Box::new(right),
                                info: head.get_info(),
                            }.to_node(), new_toks);
                        },
                        Node::ApplyNode(a) => {
                            match *a.inner {
                                Node::SymNode(s) => {
                                    return (Let {
                                        name: s.name,
                                        requires: Some(vec![]),
                                        value: Box::new(right),
                                        info: head.get_info(),
                                    }.to_node(), new_toks);
                                },
                                _ => panic!(format!("Cannot assign to {}", a.to_node()))
                            }
                        },
                        _ => panic!(format!("Cannot assign to {}", left))
                    }
                }
                let info = head.get_info();
                return (
                    BinOp {
                        name: head.value,
                        left: Box::new(left),
                        right: Box::new(right),
                        info,
                    }.to_node(),
                    new_toks,
                );
            },
            TokenType::CloseBracket => {
                panic!("Unexpected close bracket");
            }
            TokenType::OpenBracket => {
                if head.value.as_str() == "(" && toks.front().map(|t| &t.value) == Some(&")".to_string()) {
                    toks.pop_front();
                    return (Apply{inner: Box::new(left), args: vec![], info: head.get_info()}.to_node(), toks);
                }
                let (inner, mut new_toks) = expr(toks, 0);
                // TODO: Handle empty parens
                let close = new_toks.front();
                match (head.value.as_str(), close) {
                    (open, Some(Token{value: close, tok_type: TokenType::CloseBracket, pos: _})) => {
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
                let args = get_defs(inner);
                return (Apply{inner: Box::new(left), args, info: head.get_info()}.to_node(), new_toks);
            },
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
        match update {
            (Node::Error(_), new_toks) => { return (left, new_toks); }
            _ => {}
        }
        left = update.0;
        toks = update.1;
    }

    return (left, toks);
}

pub fn parse_file(filename: String, contents: String) -> Node {
    parse_impl(Some(filename), contents)
}

fn parse_impl(filename: Option<String>, contents: String) -> Node {
    let mut toks: VecDeque<Token> = VecDeque::new();

    let mut pos = Loc{filename, ..Loc::default()};
    let mut chars = VecDeque::from_iter(contents.chars());
    loop {
        let (next, new_chars) = lex_head(chars, &mut pos);

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
    use super::parse_impl;
    use super::super::ast::*;
    use Prim::*;

    fn parse(contents: String) -> Node {
        parse_impl(None, contents)
    }

    fn num_lit(x: i32) -> Box<Node> {
        Box::new(I32(x, Info::default()).to_node())
    }

    fn str_lit(x: String) -> Box<Node> {
        Box::new(Str(x, Info::default()).to_node())
    }

    #[test]
    fn parse_num() {
        assert_eq!(parse("12".to_string()), I32(12, Info::default()).to_node());
    }

    #[test]
    fn parse_str() {
        assert_eq!(parse("\"hello world\"".to_string()),
            Str("hello world".to_string(), Info::default()).to_node());
    }

    #[test]
    fn parse_un_op() {
        assert_eq!(parse("-12".to_string()), UnOp {name: "-".to_string(),
       inner: Box::new(I32(12, Info::default()).to_node()), info: Info::default()}.to_node());
    }

    #[test]
    fn parse_min_op() {
        assert_eq!(parse("14-12".to_string()),
        BinOp {
            name: "-".to_string(),
            left: num_lit(14),
            right: num_lit(12),
            info: Info::default()
        }.to_node());
    }

    #[test]
    fn parse_mul_op() {
        assert_eq!(parse("14*12".to_string()),
        BinOp {
            name: "*".to_string(),
            left: num_lit(14),
            right: num_lit(12),
            info: Info::default()
        }.to_node());
    }

    #[test]
    fn parse_add_mul_precedence() {
        assert_eq!(parse("3+2*4".to_string()),
        BinOp {
            name: "+".to_string(),
            left: num_lit(3),
            right: Box::new(
                BinOp {
                    name: "*".to_string(),
                    left: num_lit(2),
                    right: num_lit(4),
                    info: Info::default()
                }.to_node()
            ),
            info: Info::default()
        }.to_node());
    }

    #[test]
    fn parse_mul_add_precedence() {
        assert_eq!(parse("3*2+4".to_string()),
        BinOp {
            name: "+".to_string(),
            left: Box::new(
                BinOp {
                    name: "*".to_string(),
                    left: num_lit(3),
                    right: num_lit(2),
                    info: Info::default()
                }.to_node()
            ),
            right: num_lit(4),
            info: Info::default()
        }.to_node());
    }

    #[test]
    fn parse_mul_add_parens() {
        assert_eq!(parse("3*(2+4)".to_string()),
        BinOp {
            name: "*".to_string(),
            left: num_lit(3),
            right: Box::new(
                BinOp {
                    name: "+".to_string(),
                    left: num_lit(2),
                    right: num_lit(4),
                    info: Info::default()
                }.to_node()
            ),
            info: Info::default()
        }.to_node());
    }

    #[test]
    fn parse_add_str() {
        assert_eq!(parse("\"hello\"+\" world\"".to_string()),
            BinOp {
                name: "+".to_string(),
                left: str_lit("hello".to_string()),
                right: str_lit(" world".to_string()),
                info: Info::default()
            }.to_node());
    }

}
