use specs::Entity;
use std::collections::VecDeque;
use std::sync::Arc;

use crate::ast::*;
use crate::database::{AstNode, AstNodeData, DBStorage};
use crate::errors::TError;
use crate::externs::{Direction, Semantic};
use crate::location::*;
use crate::primitives::{int32, string, Prim, Val};
use crate::tokens::*;

fn binding(storage: &mut DBStorage, tok: &Token) -> Result<Semantic, TError> {
    storage.get_extern_operator(tok.value.to_owned())
}

fn binding_dir(storage: &mut DBStorage, tok: &Token) -> Result<Direction, TError> {
    Ok(match binding(storage, tok)? {
        Semantic::Operator { assoc, .. } => assoc,
        Semantic::Func => Direction::Left,
    })
}

fn binding_power(storage: &mut DBStorage, tok: &Token) -> Result<i32, TError> {
    Ok(match binding(storage, tok)? {
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

fn nud(
    storage: &mut DBStorage,
    mut toks: VecDeque<Token>,
) -> Result<(Node, AstNodeData, VecDeque<Token>), TError> {
    if let Some(head) = toks.pop_front() {
        match head.tok_type {
            TokenType::NumLit => {
                let val = int32(head.value.parse().expect("Unexpected numeric character"));
                Ok((
                    Node::ValNode(val.clone(), head.get_info()),
                    AstNode::Value(val).into_data(head.pos),
                    toks,
                ))
            }
            TokenType::StringLit => {
                let val = string(&head.value);
                Ok((
                    Node::ValNode(val.clone(), head.get_info()),
                    AstNode::Value(val).into_data(head.pos),
                    toks,
                ))
            }
            TokenType::Op => {
                let lbp = binding_power(storage, &head)?;
                let (right, right_node, new_toks) = expr(storage, toks, lbp)?;
                let inner_node = AstNode::Symbol(head.value.clone()).into_data(head.pos.clone());
                let inner = storage.store_node(inner_node);
                let right_entity = storage.store_node(right_node);
                Ok((
                    UnOp {
                        name: head.value.clone(),
                        inner: Box::new(right),
                        info: head.get_info(),
                    }
                    .into_node(),
                    AstNode::Apply {
                        inner,
                        children: vec![right_entity],
                    }
                    .into_data(head.pos),
                    new_toks,
                ))
            }
            TokenType::CloseBracket => Err(TError::ParseError(
                format!("Unexpected close bracket {}", head.value),
                head.get_info(),
            )),
            TokenType::OpenBracket => {
                let (inner, inner_node, mut new_toks) = expr(storage, toks, 0)?;
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
                            ("(", ")") => {}
                            ("[", "]") => {}
                            ("{", "}") => {}
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
                        AstNode::Value(val).into_data(head.pos),
                        toks,
                    ));
                }
                Ok((
                    Sym {
                        name: head.value.clone(),
                        info: head.get_info(),
                    }
                    .into_node(),
                    AstNode::Symbol(head.value).into_data(head.pos),
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
            "Unexpected eof, expected expr".to_string(),
            Info::default(),
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
    storage: &mut DBStorage,
    mut toks: VecDeque<Token>,
    mut left: Node,
    left_node: AstNodeData,
) -> Result<(Node, AstNodeData, VecDeque<Token>), TError> {
    if let Some(Token {
        tok_type: TokenType::CloseBracket,
        pos,
        ..
    }) = toks.front()
    {
        return Err(TError::ParseError(
            "Expected Close bracket".to_string(),
            pos.clone().get_info(),
        ));
    }

    match toks.pop_front() {
        None => Err(TError::ParseError(
            "Unexpected eof, expected expr tail".to_string(),
            left.get_info(),
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
                let lbp = binding_power(storage, &head)?;
                let assoc = binding_dir(storage, &head)?;
                let (right, right_node, new_toks) = expr(
                    storage,
                    toks,
                    lbp - match assoc {
                        Direction::Left => 0,
                        Direction::Right => 1,
                    },
                )?;
                match head.value.as_str() {
                    ":" => {
                        left.get_mut_info().ty = Some(Box::new(right));
                        // TODO: Add the type for the entity
                        return Ok((left, left_node, new_toks));
                    }
                    "," => {
                        return Ok((
                            BinOp {
                                info: head.get_info(),
                                name: head.value.clone(),
                                left: Box::new(left),
                                right: Box::new(right),
                            }
                            .into_node(),
                            match left_node.node {
                                AstNode::Chain(mut left) => {
                                    let right_entity = storage.store_node(right_node);
                                    left.push(right_entity);
                                    AstNode::Chain(left).into_data(head.pos)
                                }
                                _ => {
                                    let left_entity = storage.store_node(left_node);
                                    let right_entity = storage.store_node(right_node);
                                    AstNode::Chain(vec![left_entity, right_entity])
                                        .into_data(head.pos)
                                }
                            },
                            new_toks,
                        ));
                    }
                    "|-" => match left {
                        Node::SymNode(s) => {
                            let left_entity = storage.store_node(left_node);
                            let inner = storage.store_node(
                                AstNode::Symbol(head.value.clone()).into_data(head.pos.clone()),
                            );
                            let right_entity = storage.store_node(right_node);
                            return Ok((
                                Abs {
                                    name: s.name,
                                    value: Box::new(right),
                                    info: head.get_info(),
                                }
                                .into_node(),
                                AstNode::Apply {
                                    inner,
                                    children: vec![left_entity, right_entity],
                                }
                                .into_data(head.pos),
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
                    "=" => {
                        let right_entity = storage.store_node(right_node);
                        match left {
                            Node::SymNode(s) => {
                                return Ok((
                                    Let {
                                        name: s.name.clone(),
                                        args: None,
                                        value: Box::new(right),
                                        info: head.get_info(),
                                    }
                                    .into_node(),
                                    AstNode::Definition {
                                        name: s.name,
                                        args: None,
                                        implementations: vec![right_entity],
                                    }
                                    .into_data(head.pos),
                                    new_toks,
                                ))
                            }
                            Node::ApplyNode(a) => match *a.inner {
                                Node::SymNode(s) => {
                                    return Ok((
                                        Let {
                                            name: s.name.clone(),
                                            args: Some(a.args),
                                            value: Box::new(right),
                                            info: head.get_info(),
                                        }
                                        .into_node(),
                                        AstNode::Definition {
                                            name: s.name,
                                            args: Some(storage.store_node_set(left_node)),
                                            implementations: vec![right_entity],
                                        }
                                        .into_data(head.pos),
                                        new_toks,
                                    ))
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
                        }
                    }
                    _ => {}
                }
                let left_entity = storage.store_node(left_node);
                let inner = storage
                    .store_node(AstNode::Symbol(head.value.clone()).into_data(head.pos.clone()));
                let right_entity = storage.store_node(right_node);
                Ok((
                    BinOp {
                        info: head.get_info(),
                        name: head.value,
                        left: Box::new(left),
                        right: Box::new(right),
                    }
                    .into_node(),
                    AstNode::Apply {
                        inner,
                        children: vec![left_entity, right_entity],
                    }
                    .into_data(head.pos),
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
                    return Ok((
                        Apply {
                            inner: Box::new(left),
                            args: vec![],
                            info: head.get_info(),
                        }
                        .into_node(),
                        left_node,
                        toks,
                    ));
                }
                let (args, args_node, mut new_toks) = expr(storage, toks, 0)?;
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
                Ok((
                    Apply {
                        inner: Box::new(left),
                        args: get_defs(args),
                        info: head.get_info(),
                    }
                    .into_node(),
                    AstNode::Apply {
                        inner: storage.store_node(left_node),
                        children: storage.store_node_set(args_node),
                    }
                    .into_data(head.pos),
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
) -> Result<(Node, AstNodeData, VecDeque<Token>), TError> {
    // TODO: Name update's fields, this is confusing (0 is tree, 1 is toks)
    let init_update = nud(storage, init_toks)?;
    let mut left: Node = init_update.0;
    let mut left_node = init_update.1;
    let mut toks: VecDeque<Token> = init_update.2;
    loop {
        match toks.front() {
            None => break,
            Some(token) => {
                if init_lbp >= binding_power(storage, token)? {
                    break;
                }
            }
        }
        let update = led(storage, toks.clone(), left.clone(), left_node.clone());
        // TODO: Only retry on parse failures...
        if let Ok(update) = update {
            left = update.0;
            left_node = update.1;
            toks = update.2;
        } else {
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
    let filename = storage.filename(module.to_vec());
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
    storage: &mut DBStorage,
    module: PathRef,
    text: &Arc<String>,
) -> Result<(Node, Entity), TError> {
    let toks = lex_string(storage, module, text)?;
    if storage.debug_level() > 0 {
        eprintln!("parsing str... {}", path_to_string(module));
    }
    let (root, root_node, left_over) = expr(storage, toks, 0)?;
    let root_entity = storage.store_node(root_node);

    if let Some(head) = left_over.front() {
        return Err(TError::ParseError(
            format!("Oh no: Left over tokens {:?}", left_over),
            head.get_info(),
        ));
    }
    if storage.options.show_ast {
        eprintln!("ast: {}", root);
    }
    Ok((root, root_entity))
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::errors::TError;
    use crate::primitives::{int32, string};
    use pretty_assertions::assert_eq;

    type Test = Result<(), TError>;

    fn parse_impl(storage: &mut DBStorage, contents: &str) -> Result<(Node, Entity), TError> {
        let filename = "test.tk";
        let module = storage.module_name(filename.to_owned());
        parse_string(storage, &module, &Arc::new(contents.to_string()))
    }

    fn parse(contents: &str) -> Result<Node, TError> {
        let mut storage = DBStorage::default();
        Ok(parse_impl(&mut storage, contents)?.0)
    }

    fn dbg_parse_entities(contents: &str) -> Result<String, TError> {
        let mut storage = DBStorage::default();
        let _out = parse_impl(&mut storage, contents)?.1;
        Ok(storage.format_entities())
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
 - HasValue(12)"
        );
        Ok(())
    }
    #[test]
    fn entity_parse_str() -> Test {
        assert_str_eq!(
            dbg_parse_entities("\"hello world\"")?,
            "\
Entity 0:
 - HasValue('hello world')"
        );
        Ok(())
    }

    #[test]
    fn entity_parse_un_op() -> Test {
        assert_str_eq!(
            dbg_parse_entities("-12")?,
            "\
Entity 0:
 - HasSymbol(\"-\")
 - IsSymbol
Entity 1:
 - HasValue(12)
Entity 2:
 - HasChildren([Entity(1, Generation(1))])
 - HasInner(Entity(0, Generation(1)))"
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
Entity 1:
 - HasSymbol(\"-\")
 - IsSymbol
Entity 2:
 - HasValue(12)
Entity 3:
 - HasChildren([Entity(0, Generation(1)), Entity(2, Generation(1))])
 - HasInner(Entity(1, Generation(1)))"
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
Entity 1:
 - HasSymbol(\"*\")
 - IsSymbol
Entity 2:
 - HasValue(12)
Entity 3:
 - HasChildren([Entity(0, Generation(1)), Entity(2, Generation(1))])
 - HasInner(Entity(1, Generation(1)))"
        );
        Ok(())
    }

    #[test]
    fn entity_parse_add_mul_precedence() -> Test {
        assert_str_eq!(
            dbg_parse_entities("3+2*4")?,
            "\
Entity 0:
 - HasValue(2)
Entity 1:
 - HasSymbol(\"*\")
 - IsSymbol
Entity 2:
 - HasValue(4)
Entity 3:
 - HasValue(3)
Entity 4:
 - HasSymbol(\"+\")
 - IsSymbol
Entity 5:
 - HasChildren([Entity(0, Generation(1)), Entity(2, Generation(1))])
 - HasInner(Entity(1, Generation(1)))
Entity 6:
 - HasChildren([Entity(3, Generation(1)), Entity(5, Generation(1))])
 - HasInner(Entity(4, Generation(1)))"
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
Entity 1:
 - HasSymbol(\"*\")
 - IsSymbol
Entity 2:
 - HasValue(2)
Entity 3:
 - HasChildren([Entity(0, Generation(1)), Entity(2, Generation(1))])
 - HasInner(Entity(1, Generation(1)))
Entity 4:
 - HasSymbol(\"+\")
 - IsSymbol
Entity 5:
 - HasValue(4)
Entity 6:
 - HasChildren([Entity(3, Generation(1)), Entity(5, Generation(1))])
 - HasInner(Entity(4, Generation(1)))"
        );
        Ok(())
    }

    #[test]
    fn entity_parse_mul_add_parens() -> Test {
        assert_str_eq!(
            dbg_parse_entities("3*(2+4)")?,
            "\
Entity 0:
 - HasValue(2)
Entity 1:
 - HasSymbol(\"+\")
 - IsSymbol
Entity 2:
 - HasValue(4)
Entity 3:
 - HasValue(3)
Entity 4:
 - HasSymbol(\"*\")
 - IsSymbol
Entity 5:
 - HasChildren([Entity(0, Generation(1)), Entity(2, Generation(1))])
 - HasInner(Entity(1, Generation(1)))
Entity 6:
 - HasChildren([Entity(3, Generation(1)), Entity(5, Generation(1))])
 - HasInner(Entity(4, Generation(1)))"
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
Entity 1:
 - HasSymbol(\"+\")
 - IsSymbol
Entity 2:
 - HasValue(' world')
Entity 3:
 - HasChildren([Entity(0, Generation(1)), Entity(2, Generation(1))])
 - HasInner(Entity(1, Generation(1)))"
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
Entity 1:
 - HasValue(7)
Entity 2:
 - HasChildren([Entity(0, Generation(1)), Entity(1, Generation(1))])"
        );
        Ok(())
    }

    #[test]
    fn entity_parse_strings_with_operators_and_trailing_values_in_let() -> Test {
        assert_str_eq!(
            dbg_parse_entities("x()= !\"hello world\";\n7")?,
            "\
Entity 0:
 - HasSymbol(\"!\")
 - IsSymbol
Entity 1:
 - HasValue('hello world')
Entity 2:
 - HasChildren([Entity(1, Generation(1))])
 - HasInner(Entity(0, Generation(1)))
Entity 3:
 - HasSymbol(\"x\")
 - IsSymbol
Entity 4:
 - HasArguments(Some([Entity(3, Generation(1))]))
 - HasChildren([Entity(2, Generation(1))])
 - HasSymbol(\"x\")
 - IsDefinition
Entity 5:
 - HasSymbol(\";\")
 - IsSymbol
Entity 6:
 - HasValue(7)
Entity 7:
 - HasChildren([Entity(4, Generation(1)), Entity(6, Generation(1))])
 - HasInner(Entity(5, Generation(1)))"
        );
        Ok(())
    }
}
