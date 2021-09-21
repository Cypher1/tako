use log::debug;
use specs::Entity;
use std::collections::VecDeque;
use std::sync::Arc;

use crate::ast::{
    path_to_string, Abs, Apply, BinOp, HasInfo, Info, Let, Node, PathRef, Sym, Symbol, ToNode, UnOp,
};
use crate::ast_node::{AstNode, AstTerm, DefinitionHead};
use crate::database::DBStorage;
use crate::errors::TError;
use crate::externs::{Direction, Semantic};
use crate::location::Loc;
use crate::primitives::{int32, string, Prim, Val};
use crate::tokens::{lex_head, Token, TokenType};

fn binding(storage: &mut DBStorage, tok: &Token) -> Semantic {
    storage.get_extern_operator(&tok.value)
}

fn binding_dir(storage: &mut DBStorage, tok: &Token) -> Direction {
    match binding(storage, tok) {
        Semantic::Operator { assoc, .. } => assoc,
        Semantic::Func => Direction::Left,
    }
}

fn binding_power(storage: &mut DBStorage, tok: &Token) -> i32 {
    match binding(storage, tok) {
        Semantic::Operator { binding, .. } => binding,
        Semantic::Func => 1000,
    }
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

fn nud(
    storage: &mut DBStorage,
    mut toks: VecDeque<Token>,
    path: PathRef,
) -> Result<(Node, AstNode, VecDeque<Token>), TError> {
    if let Some(head) = toks.pop_front() {
        match head.tok_type {
            TokenType::NumLit => {
                let val = int32(head.value.parse().expect("Unexpected numeric character"));
                Ok((
                    Node::ValNode(val.clone(), head.get_info()),
                    AstTerm::Value(val).into_node(&head.pos, None),
                    toks,
                ))
            }
            TokenType::StringLit => {
                let val = string(&head.value);
                Ok((
                    Node::ValNode(val.clone(), head.get_info()),
                    AstTerm::Value(val).into_node(&head.pos, None),
                    toks,
                ))
            }
            TokenType::Op => {
                let lbp = binding_power(storage, &head);
                let (right, right_node, new_toks) = expr(storage, toks, lbp, path)?;
                let right_entity = storage.store_node(right_node, path);
                Ok((
                    UnOp {
                        name: head.value.clone(),
                        inner: Box::new(right),
                        info: head.get_info(),
                    }
                    .into_node(),
                    AstTerm::DefinitionHead(DefinitionHead {
                        name: vec![Symbol::new(&head.value)],
                        params: Some(vec![right_entity]),
                        path: path.to_vec(),
                    })
                    .into_node(&head.pos, None),
                    new_toks,
                ))
            }
            TokenType::CloseBracket => Err(TError::ParseError(
                format!("Unexpected close bracket {}", head.value),
                head.get_info(),
            )),
            TokenType::OpenBracket => {
                let (inner, inner_node, mut new_toks) = expr(storage, toks, 0, path)?;
                // TODO require close bracket.
                let close = new_toks.front();
                match (head.value.as_str(), close) {
                    (
                        open,
                        Some(Token {
                            value: close,
                            tok_type: TokenType::CloseBracket,
                            pos: _,
                        }),
                    ) => {
                        match (open, close.as_str()) {
                            ("(", ")") | ("[", "]") | ("{", "}") => {}
                            (open, chr) => {
                                return Err(TError::ParseError(
                                    format!(
                                        "Unexpected closing bracket for {}, found {}",
                                        open, chr
                                    ),
                                    head.get_info(),
                                ));
                            }
                        };
                    }
                    (open, chr) => {
                        return Err(TError::ParseError(
                            format!("Unclosed bracket {} found {:?}", open, chr),
                            head.get_info(),
                        ));
                    }
                }
                new_toks.pop_front();
                Ok((inner, inner_node, new_toks))
            }
            TokenType::Sym => {
                // TODO: Consider making these globals.
                if head.value == "true" || head.value == "false" {
                    let val = Val::PrimVal(Prim::Bool(head.value == "true"));
                    return Ok((
                        val.clone().into_node(),
                        AstTerm::Value(val).into_node(&head.pos, None),
                        toks,
                    ));
                }
                Ok((
                    Sym {
                        name: head.value.clone(),
                        info: head.get_info(),
                    }
                    .into_node(),
                    AstTerm::DefinitionHead(DefinitionHead {
                        name: vec![Symbol::new(&head.value)],
                        params: None,
                        path: path.to_vec(),
                    })
                    .into_node(&head.pos, None),
                    toks,
                ))
            }
            TokenType::Unknown | TokenType::Whitespace => Err(TError::ParseError(
                "Lexer should not produce unknown or whitespace".to_string(),
                head.get_info(),
            )),
        }
    } else {
        Err(TError::ParseError(
            "Unexpected eof, expected expression or close paren".to_string(),
            Info::default(),
        ))
    }
}

fn get_defs(args: Node) -> Vec<Let> {
    if let Node::SymNode(sym_node) = args {
        return vec![sym_node.as_let()];
    }
    if let Node::LetNode(let_node) = args {
        return vec![let_node];
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
        info: args.get_info().clone(),
        value: Box::new(args),
    }]
}

fn led(
    storage: &mut DBStorage,
    mut toks: VecDeque<Token>,
    mut left: Node,
    left_node: AstNode,
    path: PathRef,
) -> Result<(Node, AstNode, VecDeque<Token>), TError> {
    if let Some(Token {
        tok_type: TokenType::CloseBracket,
        pos,
        ..
    }) = toks.front()
    {
        return Err(TError::ParseError(
            "Unexpected Close bracket".to_string(),
            pos.clone().get_info(),
        ));
    }
    match toks.pop_front() {
        None => Err(TError::ParseError(
            "Unexpected eof, expected expr tail".to_string(),
            left.get_info().clone(),
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
                Ok((left, left_node, toks))
            }
            TokenType::Op => {
                let lbp = binding_power(storage, &head);
                let assoc = binding_dir(storage, &head);
                let parse_right = |storage, path| {
                    expr(
                        storage,
                        toks,
                        lbp - match assoc {
                            Direction::Left => 0,
                            Direction::Right => 1,
                        },
                        path,
                    )
                };
                match head.value.as_str() {
                    ":" => {
                        let (right, right_node, new_toks) = parse_right(storage, path)?;
                        left.get_mut_info().ty = Some(Box::new(right));
                        let right_entity = storage.store_node(right_node, path);
                        return Ok((
                            left,
                            AstNode {
                                ty: Some(right_entity),
                                ..left_node
                            },
                            new_toks,
                        ));
                    }
                    "," => {
                        let (right, right_node, new_toks) = parse_right(storage, path)?;
                        return Ok((
                            BinOp {
                                info: head.get_info(),
                                name: head.value.clone(),
                                left: Box::new(left),
                                right: Box::new(right),
                            }
                            .into_node(),
                            if let AstTerm::Sequence(mut left) = left_node.term {
                                let right_entity = storage.store_node(right_node, path);
                                left.push(right_entity);
                                AstTerm::Sequence(left).into_node(&head.pos, left_node.ty)
                            } else {
                                let left_entity = storage.store_node(left_node, path);
                                let right_entity = storage.store_node(right_node, path);
                                AstTerm::Sequence(vec![left_entity, right_entity])
                                    .into_node(&head.pos, None)
                            },
                            new_toks,
                        ));
                    }
                    "|-" => match left {
                        Node::SymNode(s) => {
                            let left_entity = storage.store_node(left_node, path);
                            let (right, right_node, new_toks) = parse_right(storage, path)?;
                            let right_entity = storage.store_node(right_node, path);
                            return Ok((
                                Abs {
                                    name: s.name,
                                    value: Box::new(right),
                                    info: head.get_info(),
                                }
                                .into_node(),
                                AstTerm::DefinitionHead(DefinitionHead {
                                    name: vec![Symbol::new(&head.value)],
                                    params: Some(vec![left_entity, right_entity]),
                                    path: path.to_vec(),
                                })
                                .into_node(&head.pos, None),
                                new_toks,
                            ));
                        }
                        _ => {
                            return Err(TError::ParseError(
                                format!("Cannot abstract over {}", left),
                                head.get_info(),
                            ))
                        }
                    },
                    "=" => match left {
                        Node::SymNode(s) => {
                            let mut def_path = path.to_vec();
                            def_path.push(Symbol::new(&s.name));
                            let (right, right_node, new_toks) = parse_right(storage, &def_path)?;
                            let right_entity = storage.store_node(right_node, path);
                            let loc = left_node.loc.clone();
                            return Ok((
                                Let {
                                    name: s.name,
                                    args: None,
                                    value: Box::new(right),
                                    info: head.get_info(),
                                }
                                .into_node(),
                                left_node.into_definition(storage, right_entity, &loc)?,
                                new_toks,
                            ));
                        }
                        Node::ApplyNode(a) => match &*a.inner {
                            Node::SymNode(s) => {
                                let loc = a
                                    .inner
                                    .get_info()
                                    .loc
                                    .clone()
                                    .expect("This shouldn't be option");
                                let mut def_path = path.to_vec();
                                def_path.push(Symbol::new(&s.name));
                                let (right, right_node, new_toks) =
                                    parse_right(storage, &def_path)?;
                                let right_entity = storage.store_node(right_node, path);
                                return Ok((
                                    Let {
                                        name: s.name.clone(),
                                        args: Some(a.args.clone()),
                                        value: Box::new(right),
                                        info: head.get_info(),
                                    }
                                    .into_node(),
                                    left_node.into_definition(storage, right_entity, &loc)?,
                                    new_toks,
                                ));
                            }
                            _ => {
                                return Err(TError::ParseError(
                                    format!("Cannot assign to {}", a.into_node()),
                                    head.get_info(),
                                ))
                            }
                        },
                        _ => {
                            return Err(TError::ParseError(
                                format!("Cannot assign to {}", left),
                                head.get_info(),
                            ))
                        }
                    },
                    _ => {}
                }
                let left_entity = storage.store_node(left_node, path);
                let (right, right_node, new_toks) = parse_right(storage, path)?;
                let right_entity = storage.store_node(right_node, path);
                Ok((
                    BinOp {
                        info: head.get_info(),
                        name: head.value.clone(),
                        left: Box::new(left),
                        right: Box::new(right),
                    }
                    .into_node(),
                    AstTerm::DefinitionHead(DefinitionHead {
                        name: vec![Symbol::new(&head.value)],
                        params: Some(vec![left_entity, right_entity]),
                        path: path.to_vec(),
                    })
                    .into_node(&head.pos, None),
                    new_toks,
                ))
            }
            TokenType::CloseBracket => Err(TError::ParseError(
                "Unexpected close bracket".to_string(),
                head.get_info(),
            )),
            TokenType::OpenBracket => {
                if head.value.as_str() == "("
                    && toks.front().map(|t| &t.value) == Some(&")".to_string())
                {
                    toks.pop_front();
                    let loc = left_node.loc.clone();
                    return Ok((
                        Apply {
                            inner: Box::new(left),
                            args: vec![],
                            info: head.get_info(),
                        }
                        .into_node(),
                        match left_node.term {
                            AstTerm::DefinitionHead(head) => {
                                AstTerm::DefinitionHead(DefinitionHead {
                                    params: Some(vec![]), // TODO: check parsms was None
                                    ..head
                                })
                            }
                            _ => AstTerm::Call {
                                inner: storage.store_node(left_node, path),
                                children: vec![],
                            },
                        }
                        .into_node(&loc, None),
                        toks,
                    ));
                }
                let mut arg_path = path.to_vec();
                arg_path.push(Symbol::Anon);
                let (args, args_node, mut new_toks) = expr(storage, toks, 0, &arg_path)?;
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
                            ("(", ")") | ("[", "]") | ("{", "}") => {}
                            (open, chr) => {
                                return Err(TError::ParseError(
                                    format!(
                                        "Unexpected closing bracket for {}, found {}.",
                                        open, chr
                                    ),
                                    head.get_info(),
                                ));
                            }
                        };
                    }
                    (open, chr) => {
                        return Err(TError::ParseError(
                            format!("Unclosed bracket {}, found {:?}", open, chr),
                            head.get_info(),
                        ));
                    }
                }
                new_toks.pop_front();
                // Introduce arguments
                let loc = left_node.loc.clone();
                Ok((
                    Apply {
                        inner: Box::new(left),
                        args: get_defs(args),
                        info: head.get_info(),
                    }
                    .into_node(),
                    match left_node.term {
                        AstTerm::DefinitionHead(head) => AstTerm::DefinitionHead(DefinitionHead {
                            params: Some(storage.store_node_set(args_node, path)), // TODO: check params was none
                            ..head
                        }),
                        _ => AstTerm::Call {
                            inner: storage.store_node(left_node, path),
                            children: storage.store_node_set(args_node, path),
                        },
                    }
                    .into_node(&loc, None),
                    new_toks,
                ))
            }
            TokenType::Unknown | TokenType::Whitespace => Err(TError::ParseError(
                "Lexer should not produce unknown or whitespace".to_string(),
                head.get_info(),
            )),
        },
    }
}

fn expr(
    storage: &mut DBStorage,
    init_toks: VecDeque<Token>,
    init_lbp: i32,
    path: PathRef,
) -> Result<(Node, AstNode, VecDeque<Token>), TError> {
    // TODO: Name update's fields, this is confusing (0 is tree, 1 is toks)
    let init_update = nud(storage, init_toks, path)?;
    let mut left: Node = init_update.0;
    let mut left_node = init_update.1;
    let mut toks: VecDeque<Token> = init_update.2;
    loop {
        match toks.front() {
            None => break,
            Some(token) => {
                if init_lbp >= binding_power(storage, token) {
                    break;
                }
            }
        }
        let update = led(storage, toks.clone(), left.clone(), left_node.clone(), path);
        // TODO: Only retry on parse failures...
        if let Ok(update) = update {
            left = update.0;
            left_node = update.1;
            toks = update.2;
        } else {
            // dbg!("back tracking: ", update);
            return Ok((left, left_node, toks));
        }
    }
    Ok((left, left_node, toks))
}

fn lex_string(
    storage: &mut DBStorage,
    module: PathRef,
    contents: &str,
) -> Result<VecDeque<Token>, TError> {
    let filename = storage.filename(module);
    let mut toks: VecDeque<Token> = VecDeque::new();

    let mut pos = Loc {
        filename: Some(filename),
        ..Loc::default()
    };
    let mut chars = contents.chars().peekable();
    loop {
        let (next, mut new_chars) = lex_head(chars, &mut pos);
        if next.tok_type == TokenType::Unknown {
            if new_chars.peek().is_none() {
                break; // Finished
            }
            return Err(TError::UnknownToken(
                next,
                Info {
                    loc: Some(pos),
                    ..Info::default()
                },
                module.to_vec(),
            ));
        }
        // If valid, take the token and move on.
        toks.push_back(next);
        chars = new_chars;
    }
    Ok(toks)
}

pub fn parse_string(
    storage: &mut DBStorage,
    module: PathRef,
    text: &Arc<String>,
) -> Result<(Node, Entity), TError> {
    let toks = lex_string(storage, module, text)?;
    debug!("Parsing contents... {}", path_to_string(module));
    let (root, root_node, left_over) = expr(storage, toks, 0, module)?;
    let root_entity = storage.store_node(root_node, module);

    if let Some(head) = left_over.front() {
        return Err(TError::ParseError(
            format!("Oh no: Left over tokens {:?}", left_over),
            head.get_info(),
        ));
    }
    debug!("ast: {}", root);
    Ok((root, root_entity))
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::components::*;
    use crate::database::Requirement;
    use crate::errors::TError;
    use crate::location::Loc;
    use crate::matcher::Matcher;
    use crate::primitives::{int32, string};
    use pretty_assertions::assert_eq;

    type Test = Result<(), TError>;

    fn parse_impl(storage: &mut DBStorage, contents: &str) -> Result<(Node, Entity), TError> {
        let filename = "test.tk";
        let module = storage.module_name(filename);
        parse_string(storage, &module, &Arc::new(contents.to_string()))
    }

    fn parse(contents: &str) -> Result<Node, TError> {
        let mut storage = DBStorage::default();
        Ok(parse_impl(&mut storage, contents)?.0)
    }

    fn dbg_parse_entities(contents: &str) -> Result<String, TError> {
        Ok(parse_entities(contents)?.1.format_entities())
    }

    fn parse_entities(contents: &str) -> Result<(Entity, DBStorage), TError> {
        let mut storage = DBStorage::default();
        let root = parse_impl(&mut storage, contents)?.1;
        Ok((root, storage))
    }

    fn num_lit(x: i32) -> Box<Node> {
        Box::new(int32(x).into_node())
    }

    fn str_lit(x: &str) -> Box<Node> {
        Box::new(string(x).into_node())
    }

    #[test]
    fn parse_num() -> Test {
        assert_eq!(parse("12")?, int32(12).into_node());
        Ok(())
    }

    #[test]
    fn parse_str() -> Test {
        assert_eq!(parse("\"hello world\"")?, string("hello world").into_node());
        Ok(())
    }

    #[test]
    fn parse_un_op() -> Test {
        assert_eq!(
            parse("-12")?,
            UnOp {
                name: "-".to_string(),
                inner: Box::new(int32(12).into_node()),
                info: Info::default()
            }
            .into_node()
        );
        Ok(())
    }

    #[test]
    fn parse_min_op() -> Test {
        assert_eq!(
            parse("14-12")?,
            BinOp {
                name: "-".to_string(),
                left: num_lit(14),
                right: num_lit(12),
                info: Info::default()
            }
            .into_node()
        );
        Ok(())
    }

    #[test]
    fn parse_mul_op() -> Test {
        assert_eq!(
            parse("14*12")?,
            BinOp {
                name: "*".to_string(),
                left: num_lit(14),
                right: num_lit(12),
                info: Info::default()
            }
            .into_node()
        );
        Ok(())
    }

    #[test]
    fn parse_add_mul_precedence() -> Test {
        assert_eq!(
            parse("3+2*4")?,
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
                    .into_node()
                ),
                info: Info::default()
            }
            .into_node()
        );
        Ok(())
    }

    #[test]
    fn parse_mul_add_precedence() -> Test {
        assert_eq!(
            parse("3*2+4")?,
            BinOp {
                name: "+".to_string(),
                left: Box::new(
                    BinOp {
                        name: "*".to_string(),
                        left: num_lit(3),
                        right: num_lit(2),
                        info: Info::default()
                    }
                    .into_node()
                ),
                right: num_lit(4),
                info: Info::default()
            }
            .into_node()
        );
        Ok(())
    }

    #[test]
    fn parse_mul_add_parens() -> Test {
        assert_eq!(
            parse("3*(2+4)")?,
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
                    .into_node()
                ),
                info: Info::default()
            }
            .into_node()
        );
        Ok(())
    }

    #[test]
    fn parse_add_str() -> Test {
        assert_eq!(
            parse("\"hello\"+\" world\"")?,
            BinOp {
                name: "+".to_string(),
                left: str_lit("hello"),
                right: str_lit(" world"),
                info: Info::default()
            }
            .into_node()
        );
        Ok(())
    }

    #[test]
    fn parse_strings_followed_by_raw_values() -> Test {
        assert_eq!(
            parse("\"hello world\"\n7")?,
            BinOp {
                name: ",".to_string(),
                left: Box::new(str_lit("hello world").into_node()),
                right: num_lit(7),
                info: Info::default()
            }
            .into_node()
        );
        Ok(())
    }

    #[test]
    fn parse_strings_with_operators_and_trailing_values_in_let() -> Test {
        assert_eq!(
            parse("x()= !\"hello world\";\n7")?,
            BinOp {
                name: ";".to_string(),
                left: Box::new(
                    Let {
                        name: "x".to_string(),
                        args: Some(vec![]),
                        value: Box::new(
                            UnOp {
                                name: "!".to_string(),
                                inner: str_lit("hello world"),
                                info: Info::default(),
                            }
                            .into_node()
                        ),
                        info: Info::default(),
                    }
                    .into_node()
                ),
                right: num_lit(7),
                info: Info::default()
            }
            .into_node()
        );
        Ok(())
    }

    #[test]
    fn entity_parse_num() -> Test {
        assert_str_eq!(
            dbg_parse_entities("12")?,
            "\
Entity 0:
 - HasValue(12)
 - InstancesAt(test.tk:1:1)"
        );
        Ok(())
    }

    fn assert_eq_err<T: PartialEq + std::fmt::Debug, E: std::fmt::Display>(
        res: Result<T, E>,
        rhs: T,
    ) -> Result<(), E> {
        match &res {
            Ok(_) => {}
            Err(err) => {
                eprintln!("{0}", err);
            }
        }
        assert_eq!(res?, rhs);
        Ok(())
    }

    #[test]
    fn match_entity_parse_num() -> Test {
        let (root, storage) = parse_entities("12")?;
        Ok(assert_eq_err(
            Requirement::with_instances_at(InstancesAt(set![Loc::new("test.tk", 1, 1)]))
                .expected(
                    Requirement::with_has_value(HasValue(Val::PrimVal(Prim::I32(12)))),
                )
                .run(&storage),
            root,
        )?)
    }

    #[test]
    fn entity_parse_num_with_type_annotation() -> Test {
        assert_str_eq!(
            dbg_parse_entities("12 : Int")?,
            "\
Entity 0:
 - DefinedAt(None)
 - SymbolRef { name: [Int], context: [test.tk] }
 - InstancesAt(test.tk:1:6)
Entity 1:
 - HasType(Entity(0, Generation(1)))
 - HasValue(12)
 - InstancesAt(test.tk:1:1)"
        );
        Ok(())
    }

    #[test]
    fn entity_parse_expr_with_type_annotation() -> Test {
        assert_str_eq!(
            dbg_parse_entities("3 * 4 : Int")?,
            "\
Entity 0:
 - HasValue(3)
 - InstancesAt(test.tk:1:1)
Entity 1:
 - HasValue(4)
 - InstancesAt(test.tk:1:5)
Entity 2:
 - DefinedAt(None)
 - SymbolRef { name: [Int], context: [test.tk] }
 - InstancesAt(test.tk:1:9)
Entity 3:
 - DefinedAt(None)
 - SymbolRef { name: [*], context: [test.tk] }
 - InstancesAt(test.tk:1:3)
Entity 4:
 - Call(Entity(3, Generation(1)), [Entity(0, Generation(1)), Entity(1, Generation(1))])
 - HasType(Entity(2, Generation(1)))
 - InstancesAt(test.tk:1:3)"
        );
        Ok(())
    }

    #[test]
    fn entity_parse_expr_containing_value_with_type_annotation() -> Test {
        assert_str_eq!(
            dbg_parse_entities("3 * (4 : Int)")?,
            "\
Entity 0:
 - HasValue(3)
 - InstancesAt(test.tk:1:1)
Entity 1:
 - DefinedAt(None)
 - SymbolRef { name: [Int], context: [test.tk] }
 - InstancesAt(test.tk:1:10)
Entity 2:
 - HasType(Entity(1, Generation(1)))
 - HasValue(4)
 - InstancesAt(test.tk:1:6)
Entity 3:
 - DefinedAt(None)
 - SymbolRef { name: [*], context: [test.tk] }
 - InstancesAt(test.tk:1:3)
Entity 4:
 - Call(Entity(3, Generation(1)), [Entity(0, Generation(1)), Entity(2, Generation(1))])
 - InstancesAt(test.tk:1:3)"
        );
        Ok(())
    }

    #[test]
    fn entity_parse_expr_with_value_type_annotation() -> Test {
        assert_str_eq!(
            dbg_parse_entities("(3 * 4) : 12")?,
            "\
Entity 0:
 - HasValue(3)
 - InstancesAt(test.tk:1:2)
Entity 1:
 - HasValue(4)
 - InstancesAt(test.tk:1:6)
Entity 2:
 - HasValue(12)
 - InstancesAt(test.tk:1:11)
Entity 3:
 - DefinedAt(None)
 - SymbolRef { name: [*], context: [test.tk] }
 - InstancesAt(test.tk:1:4)
Entity 4:
 - Call(Entity(3, Generation(1)), [Entity(0, Generation(1)), Entity(1, Generation(1))])
 - HasType(Entity(2, Generation(1)))
 - InstancesAt(test.tk:1:4)"
        );
        Ok(())
    }

    #[test]
    fn entity_parse_str() -> Test {
        assert_str_eq!(
            dbg_parse_entities("\"hello world\"")?,
            "\
Entity 0:
 - HasValue('hello world')
 - InstancesAt(test.tk:1:1)"
        );
        Ok(())
    }

    #[test]
    fn entity_parse_str_with_type_annotation() -> Test {
        assert_str_eq!(
            dbg_parse_entities("\"hello world\" : String")?,
            "\
Entity 0:
 - DefinedAt(None)
 - SymbolRef { name: [String], context: [test.tk] }
 - InstancesAt(test.tk:1:17)
Entity 1:
 - HasType(Entity(0, Generation(1)))
 - HasValue('hello world')
 - InstancesAt(test.tk:1:1)"
        );
        Ok(())
    }

    #[test]
    fn entity_parse_un_op() -> Test {
        assert_str_eq!(
            dbg_parse_entities("-12")?,
            "\
Entity 0:
 - HasValue(12)
 - InstancesAt(test.tk:1:2)
Entity 1:
 - DefinedAt(None)
 - SymbolRef { name: [-], context: [test.tk] }
 - InstancesAt(test.tk:1:1)
Entity 2:
 - Call(Entity(1, Generation(1)), [Entity(0, Generation(1))])
 - InstancesAt(test.tk:1:1)"
        );
        Ok(())
    }

    #[test]
    fn entity_parse_min_op() -> Test {
        assert_str_eq!(
            dbg_parse_entities("14-12")?,
            "\
Entity 0:
 - HasValue(14)
 - InstancesAt(test.tk:1:1)
Entity 1:
 - HasValue(12)
 - InstancesAt(test.tk:1:4)
Entity 2:
 - DefinedAt(None)
 - SymbolRef { name: [-], context: [test.tk] }
 - InstancesAt(test.tk:1:3)
Entity 3:
 - Call(Entity(2, Generation(1)), [Entity(0, Generation(1)), Entity(1, Generation(1))])
 - InstancesAt(test.tk:1:3)"
        );
        Ok(())
    }

    #[test]
    fn entity_parse_mul_op() -> Test {
        assert_str_eq!(
            dbg_parse_entities("14*12")?,
            "\
Entity 0:
 - HasValue(14)
 - InstancesAt(test.tk:1:1)
Entity 1:
 - HasValue(12)
 - InstancesAt(test.tk:1:4)
Entity 2:
 - DefinedAt(None)
 - SymbolRef { name: [*], context: [test.tk] }
 - InstancesAt(test.tk:1:3)
Entity 3:
 - Call(Entity(2, Generation(1)), [Entity(0, Generation(1)), Entity(1, Generation(1))])
 - InstancesAt(test.tk:1:3)"
        );
        Ok(())
    }

    #[test]
    fn entity_parse_add_mul_precedence() -> Test {
        assert_str_eq!(
            dbg_parse_entities("3+2*4")?,
            "\
Entity 0:
 - HasValue(3)
 - InstancesAt(test.tk:1:1)
Entity 1:
 - HasValue(2)
 - InstancesAt(test.tk:1:3)
Entity 2:
 - HasValue(4)
 - InstancesAt(test.tk:1:5)
Entity 3:
 - DefinedAt(None)
 - SymbolRef { name: [*], context: [test.tk] }
 - InstancesAt(test.tk:1:4)
Entity 4:
 - Call(Entity(3, Generation(1)), [Entity(1, Generation(1)), Entity(2, Generation(1))])
 - InstancesAt(test.tk:1:4)
Entity 5:
 - DefinedAt(None)
 - SymbolRef { name: [+], context: [test.tk] }
 - InstancesAt(test.tk:1:2)
Entity 6:
 - Call(Entity(5, Generation(1)), [Entity(0, Generation(1)), Entity(4, Generation(1))])
 - InstancesAt(test.tk:1:2)" // TODO: should report the left most child node.
        );
        Ok(())
    }

    #[test]
    fn entity_parse_mul_add_precedence() -> Test {
        assert_str_eq!(
            dbg_parse_entities("3*2+4")?,
            "\
Entity 0:
 - HasValue(3)
 - InstancesAt(test.tk:1:1)
Entity 1:
 - HasValue(2)
 - InstancesAt(test.tk:1:3)
Entity 2:
 - DefinedAt(None)
 - SymbolRef { name: [*], context: [test.tk] }
 - InstancesAt(test.tk:1:2)
Entity 3:
 - Call(Entity(2, Generation(1)), [Entity(0, Generation(1)), Entity(1, Generation(1))])
 - InstancesAt(test.tk:1:2)
Entity 4:
 - HasValue(4)
 - InstancesAt(test.tk:1:5)
Entity 5:
 - DefinedAt(None)
 - SymbolRef { name: [+], context: [test.tk] }
 - InstancesAt(test.tk:1:4)
Entity 6:
 - Call(Entity(5, Generation(1)), [Entity(3, Generation(1)), Entity(4, Generation(1))])
 - InstancesAt(test.tk:1:4)"
        );
        Ok(())
    }

    #[test]
    fn entity_parse_mul_add_parens() -> Test {
        assert_str_eq!(
            dbg_parse_entities("3*(2+4)")?,
            "\
Entity 0:
 - HasValue(3)
 - InstancesAt(test.tk:1:1)
Entity 1:
 - HasValue(2)
 - InstancesAt(test.tk:1:4)
Entity 2:
 - HasValue(4)
 - InstancesAt(test.tk:1:6)
Entity 3:
 - DefinedAt(None)
 - SymbolRef { name: [+], context: [test.tk] }
 - InstancesAt(test.tk:1:5)
Entity 4:
 - Call(Entity(3, Generation(1)), [Entity(1, Generation(1)), Entity(2, Generation(1))])
 - InstancesAt(test.tk:1:5)
Entity 5:
 - DefinedAt(None)
 - SymbolRef { name: [*], context: [test.tk] }
 - InstancesAt(test.tk:1:2)
Entity 6:
 - Call(Entity(5, Generation(1)), [Entity(0, Generation(1)), Entity(4, Generation(1))])
 - InstancesAt(test.tk:1:2)"
        );
        Ok(())
    }

    #[test]
    fn entity_parse_add_str() -> Test {
        assert_str_eq!(
            dbg_parse_entities("\"hello\"+\" world\"")?,
            "\
Entity 0:
 - HasValue('hello')
 - InstancesAt(test.tk:1:1)
Entity 1:
 - HasValue(' world')
 - InstancesAt(test.tk:1:9)
Entity 2:
 - DefinedAt(None)
 - SymbolRef { name: [+], context: [test.tk] }
 - InstancesAt(test.tk:1:8)
Entity 3:
 - Call(Entity(2, Generation(1)), [Entity(0, Generation(1)), Entity(1, Generation(1))])
 - InstancesAt(test.tk:1:8)"
        );
        Ok(())
    }

    #[test]
    fn entity_parse_strings_followed_by_raw_values() -> Test {
        assert_str_eq!(
            dbg_parse_entities("\"hello world\"\n7")?,
            "\
Entity 0:
 - HasValue('hello world')
 - InstancesAt(test.tk:1:1)
Entity 1:
 - HasValue(7)
 - InstancesAt(test.tk:2:1)
Entity 2:
 - Sequence([Entity(0, Generation(1)), Entity(1, Generation(1))])
 - InstancesAt(test.tk:2:1)"
        );
        Ok(())
    }

    #[test]
    fn entity_parse_kwargs() -> Test {
        assert_str_eq!(
            dbg_parse_entities("f(arg=\"hello world\")")?,
            "\
Entity 0:
 - HasValue('hello world')
 - InstancesAt(test.tk:1:7)
Entity 1:
 - Definition { names: [[arg]], params: None, implementations: [Entity(0, Generation(1))], path: [test.tk, _] }
 - InstancesAt(test.tk:1:3)
Entity 2:
 - DefinedAt(None)
 - SymbolRef { name: [f], context: [test.tk] }
 - InstancesAt(test.tk:1:1)
Entity 3:
 - Call(Entity(2, Generation(1)), [Entity(1, Generation(1))])
 - InstancesAt(test.tk:1:1)"
        );
        Ok(())
    }

    #[test]
    fn entity_parse_mul_functions() -> Test {
        assert_str_eq!(
            dbg_parse_entities("mul(x, y)= x*y")?,
            "\
Entity 0:
 - DefinedAt(None)
 - SymbolRef { name: [x], context: [test.tk, _] }
 - InstancesAt(test.tk:1:5)
Entity 1:
 - DefinedAt(None)
 - SymbolRef { name: [y], context: [test.tk, _] }
 - InstancesAt(test.tk:1:8)
Entity 2:
 - DefinedAt(None)
 - SymbolRef { name: [x], context: [test.tk, mul] }
 - InstancesAt(test.tk:1:12)
Entity 3:
 - DefinedAt(None)
 - SymbolRef { name: [y], context: [test.tk, mul] }
 - InstancesAt(test.tk:1:14)
Entity 4:
 - DefinedAt(None)
 - SymbolRef { name: [*], context: [test.tk, mul] }
 - InstancesAt(test.tk:1:13)
Entity 5:
 - Call(Entity(4, Generation(1)), [Entity(2, Generation(1)), Entity(3, Generation(1))])
 - InstancesAt(test.tk:1:13)
Entity 6:
 - Definition { names: [[mul]], params: Some([Entity(0, Generation(1)), Entity(1, Generation(1))]), implementations: [Entity(5, Generation(1))], path: [test.tk] }
 - InstancesAt(test.tk:1:1)"
        );
        Ok(())
    }

    #[test]
    fn entity_parse_strings_with_operators_and_trailing_values_in_let() -> Test {
        assert_str_eq!(
            dbg_parse_entities("x()= !\"hello world\";\n7")?,
            "\
Entity 0:
 - HasValue('hello world')
 - InstancesAt(test.tk:1:7)
Entity 1:
 - DefinedAt(None)
 - SymbolRef { name: [!], context: [test.tk, x] }
 - InstancesAt(test.tk:1:6)
Entity 2:
 - Call(Entity(1, Generation(1)), [Entity(0, Generation(1))])
 - InstancesAt(test.tk:1:6)
Entity 3:
 - Definition { names: [[x]], params: Some([]), implementations: [Entity(2, Generation(1))], path: [test.tk] }
 - InstancesAt(test.tk:1:1)
Entity 4:
 - HasValue(7)
 - InstancesAt(test.tk:2:1)
Entity 5:
 - DefinedAt(None)
 - SymbolRef { name: [;], context: [test.tk] }
 - InstancesAt(test.tk:1:20)
Entity 6:
 - Call(Entity(5, Generation(1)), [Entity(3, Generation(1)), Entity(4, Generation(1))])
 - InstancesAt(test.tk:1:20)"
        );
        Ok(())
    }
}
