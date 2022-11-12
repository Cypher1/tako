// use rand::Rng;
use crate::ast::{Ast, NodeId};
use crate::error::TError;
use crate::tokens::Token;
use log::{debug, trace};

use static_assertions::*;
assert_eq_size!(Partial, [u8;8]);
assert_eq_size!([Partial;2], [u8;16]);

pub fn parse(filepath: &str, tokens: &[Token]) -> Result<Ast, TError> {
    let tokens = tokens.iter().peekable();
    debug!("Parse {}", filepath);
    let mut ast = Ast::new(filepath);
    expr(&mut ast, tokens);
    // TODO: REMOVE THIS (it's just to test the threading model)
    // let mut rng = rand::thread_rng();
    // std::thread::sleep(std::time::Duration::from_secs(rng.gen_range(0..10)));
    // TODO: parsing!!!
    Ok(ast)
}

type Partial = (Token, NodeId);

fn expr<'a, T: Iterator<Item=&'a Token>>(_ast: &mut Ast, mut tokens: std::iter::Peekable<T>) {
    let head = if let Some(head) = tokens.next() {
        *head
    } else {
        return;
    };
    let _stack: Vec<Partial> = vec![];
    let _left: Partial = (head, NodeId::max());
    loop {
        if let Some(tok) = tokens.next() {
            trace!("tok {tok:?}");
        } else {
            break
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::ast::*;
    use crate::tokens::lex;

    const TEST_FILE1: &str = "test.tk";

    #[test]
    fn parse_literal() -> Result<(), TError> {
        let tokens = lex("123")?;
        let Ast {
            roots: _, literals, ..
        } = parse(TEST_FILE1, &tokens)?;

        dbg!(&literals);

        assert_eq!(
            literals,
            vec![(NodeId::from_raw(0), Literal {
                kind: LiteralKind::Integer,
                encoded: "123".to_string()
            })],
            "Should have parsed a number"
        );

        Ok(())
    }

    #[test]
    fn parse_add_literals() -> Result<(), TError> {
        crate::ensure_initialized();
        let tokens = lex("1+2")?;
        let Ast {
            symbols,
            calls,
            literals,
            ..
        } = parse(TEST_FILE1, &tokens)?;

        assert_eq!(
            literals,
            vec![(NodeId::from_raw(0), Literal {
                kind: LiteralKind::Integer,
                encoded: "1".to_string()
            }),
            (NodeId::from_raw(2), Literal {
                kind: LiteralKind::Integer,
                encoded: "2".to_string()
            })],
            "Should have parsed a number"
        );

        dbg!(symbols);
        dbg!(calls);
        dbg!(literals);
        todo!();
    }

    #[test]
    fn parse_add_mul_literals() -> Result<(), TError> {
        crate::ensure_initialized();
        let tokens = lex("1+2*3")?;
        let Ast {
            symbols,
            calls,
            literals,
            ..
        } = parse(TEST_FILE1, &tokens)?;

        dbg!(symbols);
        dbg!(calls);
        dbg!(literals);

        todo!();
    }

    #[test]
    fn parse_mul_add_literals() -> Result<(), TError> {
        crate::ensure_initialized();
        let tokens = lex("1+2*3")?;
        let Ast {
            symbols,
            calls,
            literals,
            ..
        } = parse(TEST_FILE1, &tokens)?;

        dbg!(symbols);
        dbg!(calls);
        dbg!(literals);

        todo!();
    }
}
/*
use crate::ast::{
    Abs, Apply, HasInfo, Info, Let, Node, PathRef, Sym, Symbol,
};
use crate::compiler_context::CompilerContext;
use crate::error::TError;
use crate::location::Loc;
use crate::primitives::{int32, string, Prim, Val};
*/
/*
#[derive(Debug, Clone)]
enum Semantic {
    Operator { binding: u32, associativity: Direction },
    Func
}

#[derive(Debug, Copy, Clone)]
enum Direction {
    Left,
    Right,
}

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
                                        args: Some(a.args),
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
                                args: vec![],
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
                            args: storage.store_node_set(args_node, path),
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
        toks.push(next);
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
*/
/*
#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::components::*;
    use crate::error::TError;
    use crate::matcher::Matcher;
    use crate::pretty_assertions::assert_eq_err;

    type Test = Result<(), TError>;

    static TEST_FN: &str = "test.tk";

    fn parse_entities(contents: &str) -> Result<(Entity, DBStorage), TError> {
        let mut storage = DBStorage::default();
        let module = storage.module_name(TEST_FN);
        let root = parse_string(&mut storage, &module, &Arc::new(contents.to_string()))?.1;
        Ok((root, storage))
    }

    #[test]
    fn parse_num() -> Test {
        let (root, storage) = parse_entities("12")?;
        assert_eq_err(
            HasValue::new(Prim::I32(12)).at(TEST_FN, 1, 1).run(&storage),
            root,
        )
    }

    #[test]
    fn parse_num_with_type_annotation() -> Test {
        let (root, storage) = parse_entities("12 : Int")?;
        assert_eq_err(
            SymbolRef::new(path!("Int"), path!(TEST_FN))
                .at(TEST_FN, 1, 6)
                .with(|ty_id| {
                    HasValue::new(Prim::I32(12))
                        .expect(HasType(*ty_id))
                        .at(TEST_FN, 1, 1)
                })
                .run(&storage)
                .map(|res| res.1),
            root,
        )
    }

    #[test]
    fn parse_expr_with_type_annotation() -> Test {
        let (root, storage) = parse_entities("3 * 4 : Int")?;
        let node_3 = HasValue::new(Prim::I32(3)).at(TEST_FN, 1, 1);
        let node_mul = SymbolRef::new(path!("*"), path!(TEST_FN)).at(TEST_FN, 1, 3);
        let node_4 = HasValue::new(Prim::I32(4)).at(TEST_FN, 1, 5);
        let int_ty = SymbolRef::new(path!("Int"), path!(TEST_FN)).at(TEST_FN, 1, 9);
        assert_eq_err(
            (((node_3, node_mul), node_4), int_ty)
                .with(|(((n_3, n_mul), n_4), n_ty)| {
                    Call::new(*n_mul, &[*n_3, *n_4])
                        .expect(HasType(*n_ty))
                        .at(TEST_FN, 1, 3)
                })
                .run(&storage)
                .map(|res| res.1),
            root,
        )
    }

    #[test]
    fn parse_expr_containing_value_with_type_annotation() -> Test {
        let (root, storage) = parse_entities("3 * (4 : Int)")?;
        let node_3 = HasValue::new(Prim::I32(3)).at(TEST_FN, 1, 1);
        let int_ty = SymbolRef::new(path!("Int"), path!(TEST_FN)).at(TEST_FN, 1, 10);
        let node_4 = int_ty.with(|n_ty| {
            HasValue::new(Prim::I32(4))
                .expect(HasType(*n_ty))
                .at(TEST_FN, 1, 6)
        });
        let node_mul = SymbolRef::new(path!("*"), path!(TEST_FN)).at(TEST_FN, 1, 3);
        assert_eq_err(
            ((node_3, node_mul), node_4)
                .with(|((n_3, n_mul), (_n_ty, n_4))| {
                    Call::new(*n_mul, &[*n_3, *n_4]).at(TEST_FN, 1, 3)
                })
                .run(&storage)
                .map(|res| res.1),
            root,
        )
    }

    #[test]
    fn parse_expr_with_value_type_annotation() -> Test {
        let (root, storage) = parse_entities("(3 * 4) : 12")?;
        let node_3 = HasValue::new(Prim::I32(3)).at(TEST_FN, 1, 2);
        let node_4 = HasValue::new(Prim::I32(4)).at(TEST_FN, 1, 6);
        let node_12 = HasValue::new(Prim::I32(12)).at(TEST_FN, 1, 11);
        let node_mul = SymbolRef::new(path!("*"), path!(TEST_FN)).at(TEST_FN, 1, 4);
        assert_eq_err(
            (((node_3, node_mul), node_4), node_12)
                .with(|(((n_3, n_mul), n_4), n_12)| {
                    Call::new(*n_mul, &[*n_3, *n_4])
                        .expect(HasType(*n_12))
                        .at(TEST_FN, 1, 4)
                })
                .run(&storage)
                .map(|res| res.1),
            root,
        )
    }

    #[test]
    fn parse_str() -> Test {
        let (root, storage) = parse_entities("\"hello world\"")?;
        let txt = "hello world".to_string();
        assert_eq_err(
            HasValue::new(Prim::Str(txt))
                .at(TEST_FN, 1, 1)
                .run(&storage),
            root,
        )
    }

    #[test]
    fn parse_str_with_type_annotation() -> Test {
        let (root, storage) = parse_entities("\"hello world\" : String")?;
        assert_eq_err(
            SymbolRef::new(path!("String"), path!(TEST_FN))
                .at(TEST_FN, 1, 17)
                .with(|ty_id| {
                    HasValue::new(Prim::Str("hello world".to_string()))
                        .expect(HasType(*ty_id))
                        .at(TEST_FN, 1, 1)
                })
                .run(&storage)
                .map(|res| res.1),
            root,
        )
    }

    #[test]
    fn parse_un_op() -> Test {
        let (root, storage) = parse_entities("-12")?;
        let node_12 = HasValue::new(Prim::I32(12)).at(TEST_FN, 1, 2);
        let node_neg = SymbolRef::new(path!("-"), path!(TEST_FN)).at(TEST_FN, 1, 1);
        assert_eq_err(
            (node_12, node_neg)
                .with(|(n_12, n_neg)| Call::new(*n_neg, &[*n_12]).at(TEST_FN, 1, 1))
                .run(&storage)
                .map(|res| res.1),
            root,
        )
    }

    #[test]
    fn parse_min_op() -> Test {
        let (root, storage) = parse_entities("14-12")?;
        let node_12 = HasValue::new(Prim::I32(12)).at(TEST_FN, 1, 4);
        let node_14 = HasValue::new(Prim::I32(14)).at(TEST_FN, 1, 1);
        let node_neg = SymbolRef::new(path!("-"), path!(TEST_FN)).at(TEST_FN, 1, 3);
        assert_eq_err(
            ((node_12, node_neg), node_14)
                .with(|((n_12, n_neg), n_14)| Call::new(*n_neg, &[*n_14, *n_12]).at(TEST_FN, 1, 3))
                .run(&storage)
                .map(|res| res.1),
            root,
        )
    }

    #[test]
    fn parse_mul_op() -> Test {
        let (root, storage) = parse_entities("14+12")?;
        let node_12 = HasValue::new(Prim::I32(12)).at(TEST_FN, 1, 4);
        let node_14 = HasValue::new(Prim::I32(14)).at(TEST_FN, 1, 1);
        let node_pls = SymbolRef::new(path!("+"), path!(TEST_FN)).at(TEST_FN, 1, 3);
        assert_eq_err(
            ((node_12, node_pls), node_14)
                .with(|((n_12, n_pls), n_14)| Call::new(*n_pls, &[*n_14, *n_12]).at(TEST_FN, 1, 3))
                .run(&storage)
                .map(|res| res.1),
            root,
        )
    }

    #[test]
    fn parse_add_mul_precedence() -> Test {
        let (root, storage) = parse_entities("3+2*4")?;
        let node_3 = HasValue::new(Prim::I32(3)).at(TEST_FN, 1, 1);
        let node_pls = SymbolRef::new(path!("+"), path!(TEST_FN)).at(TEST_FN, 1, 2);
        let node_2 = HasValue::new(Prim::I32(2)).at(TEST_FN, 1, 3);
        let node_mul = SymbolRef::new(path!("*"), path!(TEST_FN)).at(TEST_FN, 1, 4);
        let node_4 = HasValue::new(Prim::I32(4)).at(TEST_FN, 1, 5);
        assert_eq_err(
            ((((node_3, node_pls), node_2), node_mul), node_4)
                .with(|((((_n_3, _n_pls), n_2), n_mul), n_4)| {
                    Call::new(*n_mul, &[*n_2, *n_4]).at(TEST_FN, 1, 4)
                })
                .with(|(((((n_3, n_pls), _n_2), _n_mul), _n_4), n_8)| {
                    Call::new(*n_pls, &[*n_3, *n_8]).at(TEST_FN, 1, 2)
                })
                .run(&storage)
                .map(|res| res.1),
            root,
        )
    }

    #[test]
    fn parse_mul_add_precedence() -> Test {
        let (root, storage) = parse_entities("3*2+4")?;
        let node_3 = HasValue::new(Prim::I32(3)).at(TEST_FN, 1, 1);
        let node_mul = SymbolRef::new(path!("*"), path!(TEST_FN)).at(TEST_FN, 1, 2);
        let node_2 = HasValue::new(Prim::I32(2)).at(TEST_FN, 1, 3);
        let node_pls = SymbolRef::new(path!("+"), path!(TEST_FN)).at(TEST_FN, 1, 4);
        let node_4 = HasValue::new(Prim::I32(4)).at(TEST_FN, 1, 5);
        assert_eq_err(
            (
                (
                    ((node_3, node_mul), node_2).with(|((n_3, n_mul), n_2)| {
                        Call::new(*n_mul, &[*n_3, *n_2]).at(TEST_FN, 1, 2)
                    }),
                    node_pls,
                ),
                node_4,
            )
                .with(|(((_, n_6), n_pls), n_4)| Call::new(*n_pls, &[*n_6, *n_4]).at(TEST_FN, 1, 4))
                .run(&storage)
                .map(|res| res.1),
            root,
        )
    }

    #[test]
    fn parse_mul_add_parens() -> Test {
        let (root, storage) = parse_entities("3*(2+4)")?;
        let node_3 = HasValue::new(Prim::I32(3)).at(TEST_FN, 1, 1);
        let node_mul = SymbolRef::new(path!("*"), path!(TEST_FN)).at(TEST_FN, 1, 2);
        let node_2 = HasValue::new(Prim::I32(2)).at(TEST_FN, 1, 4);
        let node_pls = SymbolRef::new(path!("+"), path!(TEST_FN)).at(TEST_FN, 1, 5);
        let node_4 = HasValue::new(Prim::I32(4)).at(TEST_FN, 1, 6);
        assert_eq_err(
            (
                (node_3, node_mul),
                ((node_2, node_pls), node_4)
                    .with(|((n_2, n_pls), n_4)| Call::new(*n_pls, &[*n_2, *n_4]).at(TEST_FN, 1, 5)),
            )
                .with(|((n_3, n_mul), (_, n_6))| Call::new(*n_mul, &[*n_3, *n_6]).at(TEST_FN, 1, 2))
                .run(&storage)
                .map(|res| res.1),
            root,
        )
    }

    #[test]
    fn parse_add_str() -> Test {
        let (root, storage) = parse_entities("\"hello\"+\" world\"")?;
        let node_hello = HasValue::new(Prim::Str("hello".to_string())).at(TEST_FN, 1, 1);
        let node_world = HasValue::new(Prim::Str(" world".to_string())).at(TEST_FN, 1, 9);
        let node_pls = SymbolRef::new(path!("+"), path!(TEST_FN)).at(TEST_FN, 1, 8);
        assert_eq_err(
            ((node_hello, node_pls), node_world)
                .with(|((n_h, n_pls), n_w)| Call::new(*n_pls, &[*n_h, *n_w]).at(TEST_FN, 1, 8))
                .run(&storage)
                .map(|res| res.1),
            root,
        )
    }

    #[test]
    fn parse_strings_followed_by_raw_values() -> Test {
        let (root, storage) = parse_entities("\"hello world\"\n7")?;
        let node_hello = HasValue::new(Prim::Str("hello world".to_string())).at(TEST_FN, 1, 1);
        let node_7 = HasValue::new(Prim::I32(7)).at(TEST_FN, 2, 1);
        assert_eq_err(
            (node_hello, node_7)
                .with(|(n_h, n_7)| Sequence(vec![*n_h, *n_7]).at(TEST_FN, 2, 1))
                .run(&storage)
                .map(|res| res.1),
            root,
        )
    }

    #[test]
    fn parse_kwargs() -> Test {
        let (root, storage) = parse_entities("f(arg=\"hello world\")")?;
        let node_hello = HasValue::new(Prim::Str("hello world".to_string())).at(TEST_FN, 1, 7);
        let node_f = SymbolRef::new(path!("f"), path!(TEST_FN)).at(TEST_FN, 1, 1);
        assert_eq_err(
            (
                node_hello.with(|n_h| {
                    Definition {
                        names: vec![path!("arg")],
                        params: None,
                        implementations: vec![*n_h],
                        path: path!(TEST_FN, "_"),
                    }
                    .at(TEST_FN, 1, 3)
                }),
                node_f,
            )
                .with(|((_n_h, n_a), n_f)| {
                    Call {
                        inner: *n_f,
                        args: vec![*n_a],
                    }
                    .at(TEST_FN, 1, 1)
                })
                .run(&storage)
                .map(|res| res.1),
            root,
        )
    }

    #[test]
    fn parse_mul_functions() -> Test {
        let (root, storage) = parse_entities("mul(x, y)= x*y")?;
        let arg_x = SymbolRef::new(path!("x"), path!(TEST_FN, "_")).at(TEST_FN, 1, 5);
        let arg_y = SymbolRef::new(path!("y"), path!(TEST_FN, "_")).at(TEST_FN, 1, 8);
        let node_x = SymbolRef::new(path!("x"), path!(TEST_FN, "mul")).at(TEST_FN, 1, 12);
        let node_mul = SymbolRef::new(path!("*"), path!(TEST_FN, "mul")).at(TEST_FN, 1, 13);
        let node_y = SymbolRef::new(path!("y"), path!(TEST_FN, "mul")).at(TEST_FN, 1, 14);
        let res = ((node_x, node_mul), node_y).with(|((n_x, n_mul), n_y)| {
            Call {
                inner: *n_mul,
                args: vec![*n_x, *n_y],
            }
            .at(TEST_FN, 1, 13)
        });
        let def = ((arg_x, arg_y), res).with(|((n_x, n_y), (_, n_r))| {
            Definition {
                names: vec![path!("mul")],
                params: Some(vec![*n_x, *n_y]),
                implementations: vec![*n_r],
                path: path!(TEST_FN),
            }
            .at(TEST_FN, 1, 1)
        });
        assert_eq_err(def.run(&storage).map(|res| res.1), root)
    }

    #[test]
    fn parse_strings_with_operators_and_trailing_values_in_let() -> Test {
        let (root, storage) = parse_entities("x()= !\"hello world\";\n7")?;
        let node_hello = HasValue::new(Prim::Str("hello world".to_string())).at(TEST_FN, 1, 7);
        let node_not = SymbolRef::new(path!("!"), path!(TEST_FN, "x")).at(TEST_FN, 1, 6);
        let x_impl = (node_hello, node_not).with(|(n_h, n_n)| {
            Call {
                inner: *n_n,
                args: vec![*n_h],
            }
            .at(TEST_FN, 1, 6)
        });
        let x_def = x_impl.with(|(_n_h, n_i)| {
            Definition {
                names: vec![path!("x")],
                params: Some(vec![]),
                implementations: vec![*n_i],
                path: path!(TEST_FN),
            }
            .at(TEST_FN, 1, 1)
        });
        let node_7 = HasValue::new(Prim::I32(7)).at(TEST_FN, 2, 1);
        let node_semi = SymbolRef::new(path!(";"), path!(TEST_FN)).at(TEST_FN, 1, 20);
        assert_eq_err(
            ((x_def, node_semi), node_7)
                .with(|(((_, n_d), n_s), n_7)| {
                    Call {
                        inner: *n_s,
                        args: vec![*n_d, *n_7],
                    }
                    .at(TEST_FN, 1, 20)
                })
                .run(&storage)
                .map(|res| res.1),
            root,
        )
    }
}
*/
