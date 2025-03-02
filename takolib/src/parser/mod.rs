use log::error;
use std::path::Path;

use smallvec::SmallVec;
use tree_sitter::{Language, Node as TreeNode};
use tree_sitter::{Parser as TSParser, Tree};

use tokens::{Symbol, Token};

use crate::ast::string_interner::StrId;
use crate::{
    ast::{location::Location, nodes::Op, Ast, Call, Definition, NodeId},
    error::TError,
};

pub mod semantics;

pub const KEYWORDS: &[&str] = &[]; // TODO: Recover from tako.l

// TODO: REMOVE
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenType {
    Op(Symbol), // An operator (i.e. a known symbol used as a prefix or infix operator).
    Comma,      // A regular comma.
    Ident,      // A named value.
    Atom,       // A symbol starting with a '$', used differently to symbols which have values.
    // Literals (i.e. tokens representing values):
    NumberLit,
    ColorLit,
    // Short strings can be stored as symbols.
    StringLit,
    // Format string parts:
    FmtStringLitStart,
    FmtStringLitMid,
    FmtStringLitEnd,
    // If a symbol (normally strings) is too long, we will store it as multiple repeated tokens,
    // of the same kind preceeded by a 'Group' token.
    Group,
}

// TODO: REMOVE
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum OpBinding {
    PostfixOp,
    PrefixOp,
    PrefixOrInfixBinOp,
    InfixOrPostfixBinOp,
    InfixBinOp,
    Open(Symbol),  // the associated Closer
    Close(Symbol), // the associated Opener
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ParseError {
    UnexpectedEof, // TODO: Add context.
    UnexpectedTokenTypeExpectedOperator {
        got: TokenType,
        location: Location,
    },
    UnexpectedTokenTypeExpectedAssignment {
        got: TokenType,
        location: Location,
    },
    UnexpectedTokenType {
        got: TokenType,
        location: Location,
        expected: TokenType,
    },
    UnexpectedTokenTypeInExpression {
        got: TokenType,
        location: Location,
    },
    ParseIntError {
        message: String,
        location: Option<Location>,
    },
    AmbiguousExpression {
        left: Symbol,
        right: Symbol,
        location: Location,
    },
    UnexpectedExpressionInDefinitionArguments {
        arg: NodeId,
        arg_str: String,
        location: Location,
    },
    MissingLeftHandSideOfOperator {
        op: Symbol,
        bind_type: OpBinding,
        location: Location,
    },
    MissingRightHandSideOfOperator {
        op: Symbol,
        bind_type: OpBinding,
        location: Location,
    },
    UnparsedTokens {
        token: TokenType,
        location: Location,
    },
}

impl ParseError {
    pub fn location(&self) -> Option<&Location> {
        match self {
            Self::UnexpectedEof => None,
            Self::ParseIntError { location, .. } => location.as_ref(),
            Self::UnexpectedTokenTypeExpectedOperator { got: _, location }
            | Self::UnexpectedTokenTypeExpectedAssignment { got: _, location }
            | Self::UnexpectedTokenType {
                got: _,
                location,
                expected: _,
            }
            | Self::UnexpectedTokenTypeInExpression { got: _, location }
            | Self::AmbiguousExpression {
                left: _,
                right: _,
                location,
            }
            | Self::UnexpectedExpressionInDefinitionArguments { location, .. }
            | Self::MissingLeftHandSideOfOperator { location, .. } => Some(location),
            Self::MissingRightHandSideOfOperator { location, .. } => Some(location),
            Self::UnparsedTokens { location, .. } => Some(location),
        }
    }
}
impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

// TODO: REMOVE
pub mod tokens {
    use crate::error::TError;

    #[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
    pub enum Symbol {
        MultiCommentOpen,
        MultiCommentClose,
        Comment,

        OpenParen,
        CloseParen,
        OpenCurly,
        CloseCurly,
        OpenBracket,
        CloseBracket,

        Eqs,
        NotEqs,
        Lt,
        Gt,
        LtEqs,
        GtEqs,

        AddAssign,
        SubAssign,
        DivAssign,
        MulAssign,
        AndAssign,
        OrAssign,
        BitXorAssign,
        LogicalAndAssign,
        LogicalOrAssign,
        ModuloAssign,
        Assign,

        LogicalNot,
        Shebang,
        Hash,
        Modulo,
        And,
        LogicalAnd,
        Mul,
        Exp,
        Add,
        Sub,
        Arrow,
        Dot,
        Range,
        Spread,
        Div,
        HasType,
        Sequence,
        LeftShift,
        LeftPipe,
        DoubleArrow,
        RightShift,
        Try,
        GetAddress,
        BitXor,
        Or,
        RightPipe,
        LogicalOr,
        BitNot,

        Lambda,
        Pi,
        Sigma,
        Forall,
        Exists,
    }

    impl std::fmt::Display for Symbol {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(
                f,
                "{}",
                match self {
                    Self::Hash => "#",
                    Self::Shebang => "#!",
                    Self::Comment => "//",
                    Self::MultiCommentOpen => "/*",
                    Self::MultiCommentClose => "*/",
                    // Basics
                    Self::Add => "+",
                    Self::Sub => "-",
                    Self::Div => "/",
                    Self::Mul => "*",
                    Self::Exp => "**",
                    Self::LogicalNot => "!",
                    Self::BitNot => "~",
                    Self::And => "&",
                    Self::BitXor => "^",
                    Self::Or => "|",
                    Self::LogicalAnd => "&&",
                    Self::LogicalOr => "||",
                    Self::Modulo => "%",
                    Self::GetAddress => "@",
                    Self::HasType => ":",
                    Self::Try => "?",
                    Self::Dot => ".",
                    Self::Range => "..",
                    Self::Spread => "...",
                    Self::Sequence => ";",
                    Self::Arrow => "->",
                    Self::DoubleArrow => "=>",
                    Self::LeftShift => "<<",
                    Self::RightShift => ">>",
                    Self::LeftPipe => "<|",
                    Self::RightPipe => "|>",
                    // Assignment versions
                    Self::Assign => "=",
                    Self::AddAssign => "+=",
                    Self::SubAssign => "-=",
                    Self::DivAssign => "/=",
                    Self::MulAssign => "*=",
                    Self::AndAssign => "&=",
                    Self::OrAssign => "|=",
                    Self::BitXorAssign => "^=",
                    Self::LogicalAndAssign => "&&=",
                    Self::LogicalOrAssign => "||=",
                    Self::ModuloAssign => "%=",
                    // Quantification
                    Self::Lambda => "λ",
                    Self::Sigma => "Σ",
                    Self::Pi => "Π",
                    Self::Forall => "∀",
                    Self::Exists => "∃",
                    // Comparisons
                    Self::Eqs => "==",
                    Self::NotEqs => "!=",
                    Self::Lt => "<",
                    Self::Gt => ">",
                    Self::LtEqs => "<=",
                    Self::GtEqs => ">=",
                    Self::OpenParen => "(",
                    Self::CloseParen => ")",
                    Self::OpenCurly => "{",
                    Self::CloseCurly => "}",
                    Self::OpenBracket => "[",
                    Self::CloseBracket => "]",
                }
            )
        }
    }
    #[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
    pub enum Token {}
    impl std::fmt::Display for Token {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{self:?}")
        }
    }

    pub fn lex(_s: &str) -> Result<Vec<Token>, TError> {
        // TODO: Remove
        Ok(vec![])
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct ParseParams {
    binding: semantics::BindingMode,
    name: Option<StrId>,
    children: SmallVec<NodeId, 2>,
}

fn handle_subtree(
    ts_node: TreeNode<'_>,
    _file: &Path,
    input: &str,
    ast: &mut Ast,
    parent_params: &mut ParseParams,
) -> Result<Option<NodeId>, TError> {
    // TODO: Check that this is large enough but not too large
    let mut children_walker = ts_node.walk();
    let mut params = ParseParams::default();
    for ts_child in ts_node.children(&mut children_walker) {
        if !ts_child.is_named() {
            // BIG assumption being made here...
            continue;
        }
        let child = handle_subtree(ts_child, _file, input, ast, &mut params)?;

        // TODO: Check that this subtree is allowed.

        if let Some(child) = child {
            params.children.push(child);
        }
    }
    // TODO: Handle merging
    // TODO: Handle constructing this kind of node from it's children
    let nt = &ast.node_types;

    // TODO: Handle ts_node.is_missing()
    // TODO: Handle ts_node.is_extra()
    // TODO: Handle ts_node.is_error()

    // TODO: Handle complex node types?

    // Handle text extraction node types
    let contents = ts_node.utf8_text(input.as_bytes()).unwrap();
    let start: u16 = ts_node.start_byte().try_into().expect("big index 1");
    let end: u16 = ts_node.end_byte().try_into().expect("big index 2");
    let length: u8 = (end - start).try_into().expect("big index 3");
    let start_pos = ts_node.start_position();
    let end_pos = ts_node.end_position();
    let loc = Location { start, length };
    if ts_node.kind_id() == nt._ident {
        // println!("IDENT {:?} {:?}..{:?} {:?}..{:?}", contents, start, end, start_pos, end_pos);
        let t = ast.string_interner.register_str(contents);
        parent_params.name = Some(t);
        let b = ast.add_identifier(t, loc);
        return Ok(Some(b));
    }
    if ts_node.kind_id() == nt._forall {
        println!(
            "FORALL {:?} {:?}..{:?} {:?}..{:?}",
            contents, start, end, start_pos, end_pos
        );
        //let _s = ast.string_interner.register_str_by_loc(contents, start);
        //let b = ast.add_literal(t, loc);
        parent_params.binding = semantics::BindingMode::Forall;
        return Ok(None);
    }
    if ts_node.kind_id() == nt._binding {
        println!(
            "BINDING {:?} {:?}..{:?} {:?}..{:?}",
            contents, start, end, start_pos, end_pos
        );
        let name = if let Some(name) = params.name {
            name
        } else {
            todo!("Missing name")
        };
        let d = Definition {
            mode: params.binding,
            name,
            arguments: None,
            implementation: None,
        };
        let n = ast.add_definition(d, loc);
        return Ok(Some(n));
    }
    if ts_node.kind_id() == nt._assign {
        println!(
            "ASSIGN {:?} {:?}..{:?} {:?}..{:?}",
            contents, start, end, start_pos, end_pos
        );
        assert_eq!(params.children.len(), 2);
        let n = ast.add_implementation(params.children[0], params.children[1]);
        return Ok(Some(n));
    }
    if ts_node.kind_id() == nt._int_literal {
        // println!("INT_LITERAL {:?} {:?}..{:?} {:?}..{:?}", contents, start, end, start_pos, end_pos);
        let _s = ast.string_interner.register_str_by_loc(contents, start);
        let t = semantics::Literal::Numeric; // ("123456789");
        let b = ast.add_literal(t, loc);
        return Ok(Some(b));
    }
    if ts_node.kind_id() == nt._float_literal {
        // println!("FLOAT_LITERAL {:?} {:?}..{:?} {:?}..{:?}", contents, start, end, start_pos, end_pos);
        let _s = ast.string_interner.register_str_by_loc(contents, start);
        let t = semantics::Literal::Numeric; // ("123456789");
        let b = ast.add_literal(t, loc);
        return Ok(Some(b));
    }
    if ts_node.kind_id() == nt._string_literal {
        // println!("STRIMG_LITERAL {:?} {:?}..{:?} {:?}..{:?}", contents, start, end, start_pos, end_pos);
        let _s = ast.string_interner.register_str_by_loc(contents, start);
        let t = semantics::Literal::String; // ("123456789");
        let b = ast.add_literal(t, loc);
        return Ok(Some(b));
    }
    if ts_node.kind_id() == nt._hex_literal {
        // println!("HEX_LITERAL {:?} {:?}..{:?} {:?}..{:?}", contents, start, end, start_pos, end_pos);
        let _s = ast.string_interner.register_str_by_loc(contents, start);
        let t = semantics::Literal::Numeric; // ("123456789");
        let b = ast.add_literal(t, loc);
        return Ok(Some(b));
    }
    if ts_node.kind_id() == nt._color {
        // println!("COLOR {:?} {:?}..{:?} {:?}..{:?}", contents, start, end, start_pos, end_pos);
        let _s = ast.string_interner.register_str_by_loc(contents, start);
        let t = semantics::Literal::Color; // ("123456789");
        let b = ast.add_literal(t, loc);
        return Ok(Some(b));
    }
    if ts_node.kind_id() == nt._field {
        // println!("MUL {:?} {:?}..{:?} {:?}..{:?}: {:?}", contents, start, end, start_pos, end_pos, params.children);
        assert_eq!(params.children.len(), 2);
        let op = ast.add_op(
            Op {
                op: Symbol::Dot,
                args: params.children,
            },
            loc,
        );
        return Ok(Some(op));
    }
    if ts_node.kind_id() == nt._mul {
        // println!("MUL {:?} {:?}..{:?} {:?}..{:?}: {:?}", contents, start, end, start_pos, end_pos, params.children);
        assert_eq!(params.children.len(), 2);
        let op = ast.add_op(
            Op {
                op: Symbol::Mul,
                args: params.children,
            },
            loc,
        );
        return Ok(Some(op));
    }
    if ts_node.kind_id() == nt._div {
        // println!("DIV {:?} {:?}..{:?} {:?}..{:?}: {:?}", contents, start, end, start_pos, end_pos, params.children);
        assert_eq!(params.children.len(), 2);
        let op = ast.add_op(
            Op {
                op: Symbol::Div,
                args: params.children,
            },
            loc,
        );
        return Ok(Some(op));
    }
    if ts_node.kind_id() == nt._sub {
        // println!("SUB {:?} {:?}..{:?} {:?}..{:?}: {:?}", contents, start, end, start_pos, end_pos, params.children);
        assert_eq!(params.children.len(), 2);
        let op = ast.add_op(
            Op {
                op: Symbol::Sub,
                args: params.children,
            },
            loc,
        );
        return Ok(Some(op));
    }
    if ts_node.kind_id() == nt._exp {
        // println!("EXP {:?} {:?}..{:?} {:?}..{:?}: {:?}", contents, start, end, start_pos, end_pos, params.children);
        assert_eq!(params.children.len(), 2);
        let op = ast.add_op(
            Op {
                op: Symbol::Exp,
                args: params.children,
            },
            loc,
        );
        return Ok(Some(op));
    }
    if ts_node.kind_id() == nt._add {
        // println!("ADD {:?} {:?}..{:?} {:?}..{:?}: {:?}", contents, start, end, start_pos, end_pos, params.children);
        assert_eq!(params.children.len(), 2);
        let op = ast.add_op(
            Op {
                op: Symbol::Add,
                args: params.children,
            },
            loc,
        );
        return Ok(Some(op));
    }
    if ts_node.kind_id() == nt._equals {
        // println!("EQS {:?} {:?}..{:?} {:?}..{:?}: {:?}", contents, start, end, start_pos, end_pos, params.children);
        assert_eq!(params.children.len(), 2);
        let op = ast.add_op(
            Op {
                op: Symbol::Eqs,
                args: params.children,
            },
            loc,
        );
        return Ok(Some(op));
    }
    if ts_node.kind_id() == nt._not_equals {
        // println!("EQS {:?} {:?}..{:?} {:?}..{:?}: {:?}", contents, start, end, start_pos, end_pos, params.children);
        assert_eq!(params.children.len(), 2);
        let op = ast.add_op(
            Op {
                op: Symbol::NotEqs,
                args: params.children,
            },
            loc,
        );
        return Ok(Some(op));
    }
    if ts_node.kind_id() == nt._less_than {
        // println!("LT {:?} {:?}..{:?} {:?}..{:?}: {:?}", contents, start, end, start_pos, end_pos, params.children);
        assert_eq!(params.children.len(), 2);
        let op = ast.add_op(
            Op {
                op: Symbol::Lt,
                args: params.children,
            },
            loc,
        );
        return Ok(Some(op));
    }
    if ts_node.kind_id() == nt._greater_than {
        // println!("GT {:?} {:?}..{:?} {:?}..{:?}: {:?}", contents, start, end, start_pos, end_pos, params.children);
        assert_eq!(params.children.len(), 2);
        let op = ast.add_op(
            Op {
                op: Symbol::Gt,
                args: params.children,
            },
            loc,
        );
        return Ok(Some(op));
    }
    if ts_node.kind_id() == nt._less_than_equals {
        // println!("LTE {:?} {:?}..{:?} {:?}..{:?}: {:?}", contents, start, end, start_pos, end_pos, params.children);
        assert_eq!(params.children.len(), 2);
        let op = ast.add_op(
            Op {
                op: Symbol::LtEqs,
                args: params.children,
            },
            loc,
        );
        return Ok(Some(op));
    }
    if ts_node.kind_id() == nt._greater_than_equals {
        // println!("GTE {:?} {:?}..{:?} {:?}..{:?}: {:?}", contents, start, end, start_pos, end_pos, params.children);
        assert_eq!(params.children.len(), 2);
        let op = ast.add_op(
            Op {
                op: Symbol::GtEqs,
                args: params.children,
            },
            loc,
        );
        return Ok(Some(op));
    }
    if ts_node.kind_id() == nt._neg {
        // println!("NEG {:?} {:?}..{:?} {:?}..{:?}: {:?}", contents, start, end, start_pos, end_pos, params.children);
        assert_eq!(params.children.len(), 1);
        let op = ast.add_op(
            Op {
                op: Symbol::Sub,
                args: params.children,
            },
            loc,
        );
        return Ok(Some(op));
    }
    if ts_node.kind_id() == nt._call {
        // println!(
        // "CALL {:?} {:?}..{:?} {:?}..{:?}: {:?}",
        // contents, start, end, start_pos, end_pos, params.children
        // );
        assert!(!params.children.is_empty());
        let call = ast.add_call(
            Call {
                inner: params.children[0],
                args: params.children[1..].into(),
            },
            loc,
        );
        return Ok(Some(call));
    }
    if ts_node.kind_id() == nt._has_type {
        //println!(
        //"HASTYPE {:?} {:?}..{:?} {:?}..{:?}: {:?}",
        //contents, start, end, start_pos, end_pos, params.children
        //);
        assert_eq!(params.children.len(), 2);
        let op = ast.add_annotation(params.children[0], params.children[1]);
        return Ok(Some(op));
    }
    if ts_node.kind_id() == nt._arrow {
        // TODO: Consider removing this syntax...
        //println!(
        //"ARROW {:?} {:?}..{:?} {:?}..{:?}: {:?}",
        //contents, start, end, start_pos, end_pos, params.children
        //);
        // TODO: Handle multiple inputs.
        assert_eq!(params.children.len(), 2);
        let op = ast.add_op(
            Op {
                op: Symbol::Arrow,
                args: params.children,
            },
            loc,
        );
        return Ok(Some(op));
    }
    if ts_node.kind_id() == nt._double_arrow {
        // TODO: Consider removing this syntax...
        //println!(
        //"DOUBLEARROW {:?} {:?}..{:?} {:?}..{:?}: {:?}",
        //contents, start, end, start_pos, end_pos, params.children
        //);
        // TODO: Handle multiple inputs.
        assert_eq!(params.children.len(), 2);
        let op = ast.add_op(
            Op {
                op: Symbol::DoubleArrow,
                args: params.children,
            },
            loc,
        );
        return Ok(Some(op));
    }
    if ts_node.kind_id() == nt._parens {
        //println!(
        //"PARENS {:?} {:?}..{:?} {:?}..{:?}: {:?}",
        //contents, start, end, start_pos, end_pos, params.children
        //);
        // Consider: Handle multiple values?
        assert_eq!(params.children.len(), 1);
        //let op = ast.add_op(
        //Op {
        //op: Symbol::OpenParen,
        //args: params.children,
        //},
        //loc,
        //);
        return Ok(Some(params.children[0]));
    }
    if ts_node.kind_id() == nt._container {
        //println!(
        //"CONTAINER {:?} {:?}..{:?} {:?}..{:?}: {:?}",
        //contents, start, end, start_pos, end_pos, params.children
        //);
        // Consider: Handle multiple values?
        let op = ast.add_op(
            Op {
                op: Symbol::OpenBracket,
                args: params.children,
            },
            loc,
        );
        return Ok(Some(op));
    }
    if ts_node.kind_id() == nt._set {
        //println!(
        //"SET {:?} {:?}..{:?} {:?}..{:?}: {:?}",
        //contents, start, end, start_pos, end_pos, params.children
        //);
        parent_params.children.extend(params.children);
        return Ok(None);
    }
    if ts_node.kind_id() == nt._sequence {
        //println!(
        //"SEQUENCE {:?} {:?}..{:?} {:?}..{:?}: {:?}",
        //contents, start, end, start_pos, end_pos, params.children
        //);
        // Consider: Handle many values?
        assert_eq!(params.children.len(), 2);
        let op = ast.add_op(
            Op {
                op: Symbol::Sequence,
                args: params.children,
            },
            loc,
        );
        return Ok(Some(op));
    }
    if ts_node.kind_id() == nt._block {
        //println!(
        //"BLOCK {:?} {:?}..{:?} {:?}..{:?}: {:?}",
        //contents, start, end, start_pos, end_pos, params.children
        //);
        // Consider: Handle many values?
        let op = ast.add_op(
            Op {
                op: Symbol::OpenCurly,
                args: params.children,
            },
            loc,
        );
        return Ok(Some(op));
    }
    if ts_node.kind_id() == nt._format_expression {
        //println!(
        //"BLOCK {:?} {:?}..{:?} {:?}..{:?}: {:?}",
        //contents, start, end, start_pos, end_pos, params.children
        //);
        // Consider: Handle many values?
        let op = ast.add_op(
            Op {
                op: Symbol::OpenCurly,
                args: params.children,
            },
            loc,
        );
        return Ok(Some(op));
    }
    if ts_node.kind_id() == nt._source_file {
        println!(
            "SRC {:?} {:?}..{:?} {:?}..{:?}: {:?}",
            contents, start, end, start_pos, end_pos, params.children
        );
        assert_eq!(params.children.len(), 1);
        let last_child = params.children.last().copied();
        return Ok(last_child);
    }
    let info = (
        ts_node.id(),
        ts_node.kind_id(),
        ts_node.kind(),
        ts_node.is_missing(),
        ts_node.is_extra(),
        ts_node.is_error(),
        ts_node.is_named(),
    );
    if ts_node.kind_id() == nt._shebang {
        return Ok(None);
    }
    if ts_node.kind_id() == nt._single_line_comment {
        return Ok(None);
    }
    if ts_node.kind_id() == nt._nesting_comment {
        return Ok(None);
    }
    let children_pretty: Vec<String> = params
        .children
        .iter()
        .map(|ch| format!("{}", ast.pretty_node(*ch)))
        .collect();
    println!("{:?}", children_pretty);

    todo!(
        "{:?} {:?} FROM {}",
        info,
        ts_node,
        ts_node.utf8_text(input.as_bytes()).unwrap()
    );
    // TODO: return the ID
    // Ok(None)
}

pub fn parse(file: &Path, input: &str, _tokens: &[Token]) -> Result<Ast, TError> {
    let mut ast = Ast::new(file.to_path_buf());

    // TODO: Put parser in a state to get caching
    // TODO: Set logger.
    let tako_lang: &Language = &tree_sitter_tako::LANGUAGE.into();
    let mut parser = TSParser::new();
    parser
        .set_language(tako_lang)
        .expect("Error loading Tako parser");

    let old_tree: Option<&Tree> = None;
    let Some(res) = parser.parse(input.as_bytes(), old_tree) else {
        error!("Unknown parser error");
        panic!("Unknown parser error");
    };
    // TODO: Handle errors
    println!("Result: {:?}", res);

    let ts_root = res.walk().node();
    let mut params = ParseParams::default();
    let Some(root) = handle_subtree(ts_root, file, input, &mut ast, &mut params)? else {
        todo!("Handle file with no root!?")
    };

    // Done converting to ast
    ast.roots.push(root);
    Ok(ast)
}

// TODO: Recover tests from ./old_mod.rs
