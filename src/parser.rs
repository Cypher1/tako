use std::collections::VecDeque;
use std::sync::Arc;

use super::ast::*;
use super::database::Compiler;
use super::errors::TError;
use super::externs::{Direction, Semantic};
use super::location::*;
use super::primitives::Val;
use super::tokens::*;

fn binding(db: &dyn Compiler, tok: &Token) -> Result<Semantic, TError> {
    db.get_extern_operator(tok.value.to_owned())
}

fn binding_dir(db: &dyn Compiler, tok: &Token) -> Result<Direction, TError> {
    Ok(match binding(db, tok)? {
        Semantic::Operator { assoc, .. } => assoc,
        Semantic::Func => Direction::Left,
    })
}

fn binding_power(db: &dyn Compiler, tok: &Token) -> Result<i32, TError> {
    Ok(match binding(db, tok)? {
        Semantic::Operator { binding, .. } => binding,
        Semantic::Func => 1000,
    })
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

fn nud(db: &dyn Compiler, mut toks: VecDeque<Token>) -> Result<(Node, VecDeque<Token>), TError> {
    if let Some(head) = toks.pop_front() {
        match head.tok_type {
            TokenType::NumLit => Ok((
                Node::ValNode(Val::I32(head.value.parse().unwrap()), head.get_info()),
                toks,
            )),
            TokenType::StringLit => Ok((
                Node::ValNode(Val::Str(head.value.clone()), head.get_info()),
                toks,
            )),
            TokenType::Op => {
                let lbp = binding_power(db, &head)?;
                let (right, new_toks) = expr(db, toks, lbp)?;
                Ok((
                    UnOp {
                        name: head.value.clone(),
                        inner: Box::new(right),
                        info: head.get_info(),
                    }
                    .to_node(),
                    new_toks,
                ))
            }
            TokenType::CloseBracket => {
                panic!(
                    "Unexpected close bracket {}: {:?}",
                    head.value,
                    head.get_info()
                );
            }
            TokenType::OpenBracket => {
                let (inner, mut new_toks) = expr(db, toks, 0)?;
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
                Ok((inner, new_toks))
            }
            TokenType::Sym => {
                // TODO: Consider making these globals.
                if head.value == "true" {
                    return Ok((Val::Bool(true).to_node(), toks));
                }
                if head.value == "false" {
                    return Ok((Val::Bool(false).to_node(), toks));
                }
                Ok((
                    Sym {
                        name: head.value.clone(),
                        info: head.get_info(),
                    }
                    .to_node(),
                    toks,
                ))
            }
            TokenType::Unknown | TokenType::Whitespace => {
                panic!("Lexer should not produce unknown or whitespace")
            }
        }
    } else {
        Ok((
            TError::FailedParse("Unexpected eof, expected expr".to_string(), Info::default())
                .to_node(),
            toks,
        ))
    }
}

fn get_defs(args: Node) -> Vec<Let> {
    if let Node::SymNode(symn) = args {
        return vec![symn.as_let()];
    }
    if let Node::LetNode(letn) = args {
        return vec![letn];
    }
    if let Node::BinOpNode(BinOp {
        name,
        left,
        right,
        info: _,
    }) = args.clone()
    {
        if name == "," {
            let mut left = get_defs(*left);
            left.append(&mut get_defs(*right));
            return left;
        }
    }
    vec![Let {
        name: "it".to_string(),
        args: None,
        info: args.get_info(),
        value: Box::new(args),
    }]
}

fn led(
    db: &dyn Compiler,
    mut toks: VecDeque<Token>,
    mut left: Node,
) -> Result<(Node, VecDeque<Token>), TError> {
    if let Some(Token {
        tok_type: TokenType::CloseBracket,
        pos,
        ..
    }) = toks.front()
    {
        return Ok((
            TError::FailedParse("Exected Close bracket".to_string(), pos.clone().get_info())
                .to_node(),
            toks,
        ));
    }

    match toks.pop_front() {
        None => Ok((
            TError::FailedParse(
                "Unexpected eof, expected expr tail".to_string(),
                left.get_info(),
            )
            .to_node(),
            toks,
        )),
        Some(head) => match head.tok_type {
            TokenType::NumLit | TokenType::StringLit | TokenType::Sym => {
                let pos = head.pos.clone();
                toks.push_front(head);
                toks.push_front(Token {
                    tok_type: TokenType::Op,
                    value: ",".to_string(),
                    pos,
                });
                Ok((left, toks))
            }
            TokenType::Op => {
                let lbp = binding_power(db, &head)?;
                let assoc = binding_dir(db, &head)?;
                let (right, new_toks) = expr(
                    db,
                    toks,
                    lbp - match assoc {
                        Direction::Left => 0,
                        Direction::Right => 1,
                    },
                )?;
                match head.value.as_str() {
                    ":" => {
                        left.get_mut_info().ty = Some(Box::new(right));
                        return Ok((left, new_toks));
                    }
                    "|-" => match left {
                        Node::SymNode(s) => {
                            return Ok((
                                Abs {
                                    name: s.name,
                                    value: Box::new(right),
                                    info: head.get_info(),
                                }
                                .to_node(),
                                new_toks,
                            ))
                        }
                        _ => panic!(format!("Cannot abstract over {}", left)),
                    },
                    "=" => match left {
                        Node::SymNode(s) => {
                            return Ok((
                                Let {
                                    name: s.name,
                                    args: None,
                                    value: Box::new(right),
                                    info: head.get_info(),
                                }
                                .to_node(),
                                new_toks,
                            ))
                        }
                        Node::ApplyNode(a) => match *a.inner {
                            Node::SymNode(s) => {
                                return Ok((
                                    Let {
                                        name: s.name,
                                        args: Some(a.args),
                                        value: Box::new(right),
                                        info: head.get_info(),
                                    }
                                    .to_node(),
                                    new_toks,
                                ))
                            }
                            _ => panic!(format!("Cannot assign to {}", a.to_node())),
                        },
                        _ => panic!(format!("Cannot assign to {}", left)),
                    },
                    _ => {}
                }
                Ok((
                    BinOp {
                        info: head.get_info(),
                        name: head.value,
                        left: Box::new(left),
                        right: Box::new(right),
                    }
                    .to_node(),
                    new_toks,
                ))
            }
            TokenType::CloseBracket => panic!("Unexpected close bracket"),
            TokenType::OpenBracket => {
                if head.value.as_str() == "("
                    && toks.front().map(|t| &t.value) == Some(&")".to_string())
                {
                    toks.pop_front();
                    return Ok((
                        Apply {
                            inner: Box::new(left),
                            args: vec![],
                            info: head.get_info(),
                        }
                        .to_node(),
                        toks,
                    ));
                }
                let (args, mut new_toks) = expr(db, toks, 0)?;
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
                Ok((
                    Apply {
                        inner: Box::new(left),
                        args: get_defs(args),
                        info: head.get_info(),
                    }
                    .to_node(),
                    new_toks,
                ))
            }
            TokenType::Unknown | TokenType::Whitespace => {
                panic!("Lexer should not produce unknown or whitespace")
            }
        },
    }
}

fn expr(
    db: &dyn Compiler,
    init_toks: VecDeque<Token>,
    init_lbp: i32,
) -> Result<(Node, VecDeque<Token>), TError> {
    // TODO: Name update's fields, this is confusing (0 is tree, 1 is toks)
    let init_update = nud(db, init_toks)?;
    let mut left: Node = init_update.0;
    let mut toks: VecDeque<Token> = init_update.1;
    loop {
        match toks.front() {
            None => break,
            Some(token) => {
                if init_lbp >= binding_power(db, token)? {
                    break;
                }
            }
        }
        let update = led(db, toks, left.clone())?;
        if let (Node::Error(_), new_toks) = update {
            return Ok((left, new_toks));
        }
        left = update.0;
        toks = update.1;
    }
    Ok((left, toks))
}

pub fn lex(db: &dyn Compiler, module: PathRef) -> Result<VecDeque<Token>, TError> {
    let filename = db.filename(module.to_vec());
    lex_string(db, module, &db.file(filename)?.to_string())
}

pub fn lex_string(
    db: &dyn Compiler,
    module: PathRef,
    contents: &str,
) -> Result<VecDeque<Token>, TError> {
    let filename = db.filename(module.to_vec());
    let mut toks: VecDeque<Token> = VecDeque::new();

    let mut pos = Loc {
        filename: Some(filename),
        ..Loc::default()
    };
    let mut chars = contents.chars().peekable();
    loop {
        let (next, new_chars) = lex_head(chars, &mut pos);
        if next.tok_type == TokenType::Unknown {
            break; // TODO done / skip?
        }
        // If valid, take the token and move on.
        toks.push_back(next);
        chars = new_chars;
    }
    Ok(toks)
}

pub fn parse_string(
    db: &dyn Compiler,
    module: PathRef,
    text: &Arc<String>,
) -> Result<Node, TError> {
    let toks = db.lex_string(module.to_vec(), text.clone())?;
    if db.debug_level() > 0 {
        eprintln!("parsing str... {}", path_to_string(&module));
    }
    let (root, left_over) = expr(db, toks, 0)?;

    if !left_over.is_empty() {
        panic!("Oh no: Left over tokens {:?}", left_over);
    }
    if db.options().show_ast {
        eprintln!("ast: {}", root);
    }
    Ok(root)
}

pub fn parse(db: &dyn Compiler, module: PathRef) -> Result<Node, TError> {
    let toks = db.lex_file(module.to_vec())?;
    if db.debug_level() > 0 {
        eprintln!("parsing file... {}", path_to_string(&module));
    }
    let (root, left_over) = expr(db, toks, 0)?;

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
    use super::parse_string;
    use crate::ast::*;
    use crate::database::Compiler;
    use crate::database::DB;
    use crate::primitives::Val;
    use Val::*;

    fn parse(contents: String) -> Node {
        use crate::cli_options::Options;
        use std::sync::Arc;
        let mut db = DB::default();
        let filename = "test.tk";
        db.set_options(Options::default());
        let module = db.module_name(filename.to_owned());
        parse_string(&db, &module, &Arc::new(contents)).expect("failed to parse string")
    }

    fn num_lit(x: i32) -> Box<Node> {
        Box::new(I32(x).to_node())
    }

    fn str_lit(x: String) -> Box<Node> {
        Box::new(Str(x).to_node())
    }

    #[test]
    fn parse_num() {
        assert_eq!(parse("12".to_string()), I32(12).to_node());
    }

    #[test]
    fn parse_str() {
        assert_eq!(
            parse("\"hello world\"".to_string()),
            Str("hello world".to_string()).to_node()
        );
    }

    #[test]
    fn parse_un_op() {
        assert_eq!(
            parse("-12".to_string()),
            UnOp {
                name: "-".to_string(),
                inner: Box::new(I32(12).to_node()),
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
                name: ",".to_string(),
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
            parse("x()= !\"hello world\";\n7".to_string()),
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
