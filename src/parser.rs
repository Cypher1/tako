use std::collections::VecDeque;

use super::ast::*;
use super::database::Compiler;
use super::errors::TError;
use super::location::*;
use super::tokens::*;

fn binding_power(db: &dyn Compiler, tok: &Token) -> (i32, bool) {
    let bind = match &tok.tok_type {
        TokenType::Op => {
            // Look up extern operators
            let op_name = tok.value.as_str();
            if let Some(operator_info) = db.get_extern_operator(op_name.to_owned()) {
                return operator_info;
            }
            match op_name {
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
                "+" => 70,
                "-" => 70,
                "!" => 70,
                "*" => 80,
                "/" => 80,
                "%" => 80,
                "." => 100,
                "(" => 110,
                ")" => 110,
                "[" => 110,
                "]" => 110,
                "{" => 110,
                "}" => 110,
                op => panic!(format!("Unknown operator {}", op)),
            }
        }
        TokenType::NumLit => 1000,
        _ => 1000, // TODO impossible
    };
    let assoc_right = match &tok.tok_type {
        TokenType::Op => matches!(tok.value.as_str(), "="),
        _ => false,
    };
    (bind, assoc_right)
}

fn get_defs(root: Node) -> Vec<Let> {
    use Node::*;
    let mut args = vec![];

    match root {
        LetNode(n) => args.push(n),
        SymNode(n) => args.push(Let {
            name: n.name.clone(),
            args: None,
            info: n.get_info(),
            value: Box::new(n.to_node()),
        }),
        BinOpNode(BinOp {
            name,
            left,
            right,
            info,
        }) => {
            if name == "," {
                args.append(&mut get_defs(*left));
                args.append(&mut get_defs(*right));
            } else {
                args.push(Let {
                    name: "it".to_string(),
                    args: None,
                    value: Box::new(
                        BinOp {
                            name,
                            left,
                            right,
                            info: info.clone(),
                        }
                        .to_node(),
                    ),
                    info,
                });
            }
        }
        n => args.push(Let {
            name: "it".to_string(),
            args: None,
            value: Box::new(n.clone()),
            info: n.get_info(),
        }),
    }

    args
}

impl Token {
    pub fn get_info(&self) -> Info {
        self.pos.clone().get_info()
    }
}

impl Loc {
    pub fn get_info(self) -> Info {
        Info {
            loc: Some(self),
            ..Info::default()
        }
    }
}

fn nud(db: &dyn Compiler, mut toks: VecDeque<Token>) -> (Node, VecDeque<Token>) {
    if let Some(head) = toks.pop_front() {
        match head.tok_type {
            TokenType::NumLit => (
                Prim::I32(head.value.parse().unwrap(), head.get_info()).to_node(),
                toks,
            ),
            TokenType::StringLit => (
                Prim::Str(head.value.clone(), head.get_info()).to_node(),
                toks,
            ),
            TokenType::Op => {
                let (lbp, _) = binding_power(db, &head);
                let (right, new_toks) = expr(db, toks, lbp);
                (
                    UnOp {
                        name: head.value.clone(),
                        inner: Box::new(right),
                        info: head.get_info(),
                    }
                    .to_node(),
                    new_toks,
                )
            }
            TokenType::CloseBracket => {
                panic!("Unexpected close bracket {}", head.value);
            }
            TokenType::OpenBracket => {
                let (inner, mut new_toks) = expr(db, toks, 0);
                // TODO require close bracket.
                let close = new_toks.front();
                match (head.value.as_str(), close) {
                    (
                        open,
                        Some(Token {
                            value: close,
                            tok_type: TokenType::CloseBracket,
                            pos,
                        }),
                    ) => {
                        match (open, close.as_str()) {
                            ("(", ")") => {}
                            ("[", "]") => {}
                            ("{", "}") => {}
                            (open, chr) => {
                                panic!(format!(
                                    "Unexpected closing bracket for {}, found {} at {:?}.",
                                    open, chr, pos
                                ));
                            }
                        };
                    }
                    (open, chr) => {
                        panic!("Unclosed bracket {} found {:?}", open, chr);
                    }
                }
                new_toks.pop_front();
                (inner, new_toks)
            }
            TokenType::Sym => {
                // TODO: Consider making these globals.
                if head.value == "true" {
                    return (Prim::Bool(true, head.get_info()).to_node(), toks);
                }
                if head.value == "false" {
                    return (Prim::Bool(false, head.get_info()).to_node(), toks);
                }
                (
                    Sym {
                        name: head.value.clone(),
                        info: head.get_info(),
                    }
                    .to_node(),
                    toks,
                )
            }
            TokenType::Unknown | TokenType::Whitespace => {
                panic!("Lexer should not produce unknown or whitespace")
            }
        }
    } else {
        (
            Err {
                msg: "Unexpected eof, expected expr".to_string(),
                info: Info::default(),
            }
            .to_node(),
            toks,
        )
    }
}

fn led(db: &dyn Compiler, mut toks: VecDeque<Token>, left: Node) -> (Node, VecDeque<Token>) {
    // eprintln!("here {:?} {:?}", toks, left);
    if let Some(Token {
        tok_type: TokenType::CloseBracket,
        pos,
        ..
    }) = toks.front()
    {
        return (
            Err {
                msg: "Close bracket".to_string(),
                info: pos.clone().get_info(),
            }
            .to_node(),
            toks,
        );
    }

    match toks.pop_front() {
        None => (
            Err {
                msg: "Unexpected eof, expected expr tail".to_string(),
                info: left.get_info(),
            }
            .to_node(),
            toks,
        ),
        Some(head) => match head.tok_type {
            TokenType::NumLit | TokenType::StringLit | TokenType::Sym => {
                let pos = head.pos.clone();
                toks.push_front(head);
                toks.push_front(Token {
                    tok_type: TokenType::Op,
                    value: ";".to_string(),
                    pos,
                });
                (left, toks)
            }
            TokenType::Op => {
                let (lbp, assoc_right) = binding_power(db, &head);
                let (right, new_toks) = expr(db, toks, lbp - if assoc_right { 1 } else { 0 });
                if head.value == "=" {
                    match left {
                        Node::SymNode(s) => {
                            return (
                                Let {
                                    name: s.name,
                                    args: None,
                                    value: Box::new(right),
                                    info: head.get_info(),
                                }
                                .to_node(),
                                new_toks,
                            );
                        }
                        Node::ApplyNode(a) => match *a.inner {
                            Node::SymNode(s) => {
                                return (
                                    Let {
                                        name: s.name,
                                        args: Some(a.args.iter().map(|l| l.to_sym()).collect()),
                                        value: Box::new(right),
                                        info: head.get_info(),
                                    }
                                    .to_node(),
                                    new_toks,
                                );
                            }
                            _ => panic!(format!("Cannot assign to {}", a.to_node())),
                        },
                        _ => panic!(format!("Cannot assign to {}", left)),
                    }
                }
                let info = head.get_info();
                (
                    BinOp {
                        name: head.value,
                        left: Box::new(left),
                        right: Box::new(right),
                        info,
                    }
                    .to_node(),
                    new_toks,
                )
            }
            TokenType::CloseBracket => {
                panic!("Unexpected close bracket");
            }
            TokenType::OpenBracket => {
                if head.value.as_str() == "("
                    && toks.front().map(|t| &t.value) == Some(&")".to_string())
                {
                    toks.pop_front();
                    return (
                        Apply {
                            inner: Box::new(left),
                            args: vec![],
                            info: head.get_info(),
                        }
                        .to_node(),
                        toks,
                    );
                }
                let (inner, mut new_toks) = expr(db, toks, 0);
                // TODO: Handle empty parens
                let close = new_toks.front();
                match (head.value.as_str(), close) {
                    (
                        open,
                        Some(Token {
                            value: close,
                            tok_type: TokenType::CloseBracket,
                            ..
                        }),
                    ) => {
                        match (open, close.as_str()) {
                            ("(", ")") => {}
                            ("[", "]") => {}
                            ("{", "}") => {}
                            (open, chr) => {
                                panic!(format!(
                                    "Unexpected closing bracket for {}, found {}.",
                                    open, chr
                                ));
                            }
                        };
                    }
                    (open, chr) => {
                        panic!("Unclosed bracket {}, found {:?}", open, chr);
                    }
                }
                new_toks.pop_front();
                // Introduce arguments
                let args = get_defs(inner);
                (
                    Apply {
                        inner: Box::new(left),
                        args,
                        info: head.get_info(),
                    }
                    .to_node(),
                    new_toks,
                )
            }
            TokenType::Unknown | TokenType::Whitespace => {
                panic!("Lexer should not produce unknown or whitespace")
            }
        },
    }
}

fn expr(db: &dyn Compiler, init_toks: VecDeque<Token>, init_lbp: i32) -> (Node, VecDeque<Token>) {
    // TODO: Name updates fields, this is confusing (0 is tree, 1 is toks)
    let init_update = nud(db, init_toks);
    let mut left: Node = init_update.0;
    let mut toks: VecDeque<Token> = init_update.1;
    loop {
        match toks.front() {
            None => break,
            Some(token) => {
                let (lbp, _) = binding_power(db, token);
                if init_lbp >= lbp {
                    break;
                }
            }
        }
        let update = led(db, toks, left.clone());
        if let (Node::Error(_), new_toks) = update {
            return (left, new_toks);
        }
        left = update.0;
        toks = update.1;
    }

    (left, toks)
}

pub fn lex(db: &dyn Compiler, module: Path) -> Result<VecDeque<Token>, TError> {
    let filename = db.filename(module);
    let contents = db.file(filename.clone())?;
    let mut toks: VecDeque<Token> = VecDeque::new();

    let mut pos = Loc {
        filename: Some(filename),
        ..Loc::default()
    };
    let mut chars = contents.chars().peekable();
    loop {
        let (next, new_chars) = lex_head(chars, &mut pos);

        // eprintln!("LEXING {:?}", next);

        if next.tok_type == TokenType::Unknown {
            break; // TODO done / skip?
        }

        // If valid, take the token and move on.
        toks.push_back(next);
        chars = new_chars;
    }

    // eprintln!("Toks: {:?}", toks);
    Ok(toks)
}

pub fn parse(module: &Path, db: &dyn Compiler) -> Result<Node, TError> {
    let toks = db.lex_file(module.clone())?;
    if db.debug() > 0 {
        eprintln!("parsing file... {:?}", &module);
    }
    let (root, left_over) = expr(db, toks, 0);

    if !left_over.is_empty() {
        panic!("Oh no: Left over tokens {:?}", left_over);
    }
    if db.options().show_ast {
        eprintln!("ast: {}", root);
    }
    Ok(root)
}

#[cfg(test)]
pub mod tests {
    use crate::ast::*;
    use crate::cli_options::Options;
    use crate::database::{Compiler, DB};
    use std::sync::Arc;
    use Prim::*;

    pub fn parse_with_db(db: &mut dyn Compiler, contents: String) -> Node {
        let filename = "test.tk";
        let module = db.module_name(filename.to_owned());
        db.set_file(filename.to_owned(), Ok(Arc::new(contents)));
        db.set_options(Options::default());
        db.parse_file(module).expect("failed to parse file")
    }

    fn parse(contents: String) -> Node {
        let mut db = DB::default();
        parse_with_db(&mut db, contents)
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
        assert_eq!(
            parse("\"hello world\"".to_string()),
            Str("hello world".to_string(), Info::default()).to_node()
        );
    }

    #[test]
    fn parse_un_op() {
        assert_eq!(
            parse("-12".to_string()),
            UnOp {
                name: "-".to_string(),
                inner: Box::new(I32(12, Info::default()).to_node()),
                info: Info::default()
            }
            .to_node()
        );
    }

    #[test]
    fn parse_min_op() {
        assert_eq!(
            parse("14-12".to_string()),
            BinOp {
                name: "-".to_string(),
                left: num_lit(14),
                right: num_lit(12),
                info: Info::default()
            }
            .to_node()
        );
    }

    #[test]
    fn parse_mul_op() {
        assert_eq!(
            parse("14*12".to_string()),
            BinOp {
                name: "*".to_string(),
                left: num_lit(14),
                right: num_lit(12),
                info: Info::default()
            }
            .to_node()
        );
    }

    #[test]
    fn parse_add_mul_precedence() {
        assert_eq!(
            parse("3+2*4".to_string()),
            BinOp {
                name: "+".to_string(),
                left: num_lit(3),
                right: Box::new(
                    BinOp {
                        name: "*".to_string(),
                        left: num_lit(2),
                        right: num_lit(4),
                        info: Info::default()
                    }
                    .to_node()
                ),
                info: Info::default()
            }
            .to_node()
        );
    }

    #[test]
    fn parse_mul_add_precedence() {
        assert_eq!(
            parse("3*2+4".to_string()),
            BinOp {
                name: "+".to_string(),
                left: Box::new(
                    BinOp {
                        name: "*".to_string(),
                        left: num_lit(3),
                        right: num_lit(2),
                        info: Info::default()
                    }
                    .to_node()
                ),
                right: num_lit(4),
                info: Info::default()
            }
            .to_node()
        );
    }

    #[test]
    fn parse_mul_add_parens() {
        assert_eq!(
            parse("3*(2+4)".to_string()),
            BinOp {
                name: "*".to_string(),
                left: num_lit(3),
                right: Box::new(
                    BinOp {
                        name: "+".to_string(),
                        left: num_lit(2),
                        right: num_lit(4),
                        info: Info::default()
                    }
                    .to_node()
                ),
                info: Info::default()
            }
            .to_node()
        );
    }

    #[test]
    fn parse_add_str() {
        assert_eq!(
            parse("\"hello\"+\" world\"".to_string()),
            BinOp {
                name: "+".to_string(),
                left: str_lit("hello".to_string()),
                right: str_lit(" world".to_string()),
                info: Info::default()
            }
            .to_node()
        );
    }

    #[test]
    fn parse_strings_followed_by_raw_values() {
        assert_eq!(
            parse("\"hello world\"\n7".to_string()),
            BinOp {
                name: ";".to_string(),
                left: Box::new(str_lit("hello world".to_string()).to_node()),
                right: num_lit(7),
                info: Info::default()
            }
            .to_node()
        );
    }

    #[test]
    fn parse_strings_with_operators_and_trailing_values_in_let() {
        assert_eq!(
            parse("x()= !\"hello world\"\n7".to_string()),
            BinOp {
                name: ";".to_string(),
                left: Box::new(
                    Let {
                        name: "x".to_string(),
                        args: Some(vec![]),
                        value: Box::new(
                            UnOp {
                                name: "!".to_string(),
                                inner: str_lit("hello world".to_string()),
                                info: Info::default(),
                            }
                            .to_node()
                        ),
                        info: Info::default(),
                    }
                    .to_node()
                ),
                right: num_lit(7),
                info: Info::default()
            }
            .to_node()
        );
    }
}
