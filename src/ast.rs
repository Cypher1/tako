use super::location::*;
use super::types::*;
use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};

#[derive(Debug, PartialEq, Clone)]
pub struct Err {
    pub msg: String,
    pub info: Info,
}

impl ToNode for Err {
    fn to_node(self) -> Node {
        Node::Error(self)
    }
    fn get_info(&self) -> Info {
        self.info.clone()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Apply {
    pub inner: Box<Node>,
    pub args: Vec<Let>,
    pub info: Info,
}

impl ToNode for Apply {
    fn to_node(self) -> Node {
        Node::ApplyNode(self)
    }
    fn get_info(&self) -> Info {
        self.info.clone()
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Sym {
    pub name: String,
    pub depth: Option<i32>,
    pub info: Info,
}

impl Sym {
    pub fn new(name: String) -> Sym {
        Sym {
            name,
            depth: None,
            info: Info::default(),
        }
    }
}

impl ToNode for Sym {
    fn to_node(self) -> Node {
        Node::SymNode(self)
    }
    fn get_info(&self) -> Info {
        self.info.clone()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Prim {
    Bool(bool, Info),
    I32(i32, Info),
    Str(String, Info),
    Lambda(Box<Node>),
}

impl ToNode for Prim {
    fn to_node(self) -> Node {
        Node::PrimNode(self)
    }
    fn get_info(&self) -> Info {
        use Prim::*;
        match self {
            Bool(_, info) => info.clone(),
            I32(_, info) => info.clone(),
            Str(_, info) => info.clone(),
            Lambda(node) => (*node).get_info(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Let {
    pub name: String,
    pub value: Box<Node>,
    pub args: Option<Vec<Sym>>,
    pub is_function: bool,
    pub info: Info,
}

impl Let {
    pub fn to_sym(self: &Let) -> Sym {
        Sym {
            name: self.name.clone(),
            depth: None,
            info: self.get_info(),
        }
    }
}

impl ToNode for Let {
    fn to_node(self) -> Node {
        Node::LetNode(self)
    }
    fn get_info(&self) -> Info {
        self.info.clone()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnOp {
    pub name: String,
    pub inner: Box<Node>,
    pub info: Info,
}

impl ToNode for UnOp {
    fn to_node(self) -> Node {
        Node::UnOpNode(self)
    }
    fn get_info(&self) -> Info {
        self.info.clone()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BinOp {
    pub name: String,
    pub left: Box<Node>,
    pub right: Box<Node>,
    pub info: Info,
}

impl ToNode for BinOp {
    fn to_node(self) -> Node {
        Node::BinOpNode(self)
    }
    fn get_info(&self) -> Info {
        self.info.clone()
    }
}

#[derive(Clone)]
pub struct Info {
    pub loc: Option<Loc>,
    pub ty: Option<TypeInfo>,
    pub defined_at: Option<Vec<ScopeName>>,
}

impl std::fmt::Debug for Info {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.loc {
            Some(loc) => write!(f, "{:?}", loc),
            None => write!(f, ""),
        }?;
        match &self.ty {
            Some(ty) => write!(f, "{:?}", ty),
            None => write!(f, ""),
        }?;
        Ok(())
    }
}

impl Eq for Info {}
impl PartialEq for Info {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl Hash for Info {
    fn hash<H: Hasher>(&self, _state: &mut H) {}
}

impl Default for Info {
    fn default() -> Info {
        Info {
            loc: None,
            ty: None,
            defined_at: None,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypeInfo {
    reqs: Vec<Node>,
    structure: DataType,
}

// #[derive(Debug)]
#[derive(PartialEq, Clone)]
pub enum Node {
    Error(Err),
    SymNode(Sym),
    PrimNode(Prim),
    ApplyNode(Apply),
    LetNode(Let),
    UnOpNode(UnOp),
    BinOpNode(BinOp),
}

impl std::fmt::Debug for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Node::*;
        match self {
            Error(n) => n.fmt(f),
            SymNode(n) => n.fmt(f),
            PrimNode(n) => n.fmt(f),
            ApplyNode(n) => n.fmt(f),
            LetNode(n) => n.fmt(f),
            UnOpNode(n) => n.fmt(f),
            BinOpNode(n) => n.fmt(f),
        }
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use super::PrettyPrint;
        let mut ppr = PrettyPrint::default();
        match ppr.visit_root(&self.clone().to_root()) {
            Ok(res) => write!(f, "{}", res),
            Err(err) => write!(f, "{:#?}", err),
        }
    }
}

impl fmt::Display for Root {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.ast.fmt(f)
    }
}

impl ToNode for Node {
    fn to_node(self) -> Node {
        self
    }
    fn get_info(&self) -> Info {
        use Node::*;
        match self {
            Error(n) => n.get_info(),
            SymNode(n) => n.get_info(),
            PrimNode(n) => n.get_info(),
            ApplyNode(n) => n.get_info(),
            LetNode(n) => n.get_info(),
            UnOpNode(n) => n.get_info(),
            BinOpNode(n) => n.get_info(),
        }
    }
}

pub trait ToNode {
    fn to_node(self: Self) -> Node;
    fn get_info(self: &Self) -> Info;
}

impl Node {
    pub fn to_root(self: &Self) -> Root {
        Root {
            ast: self.clone(),
            graph: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ScopeName {
    Unknown(i32),
    Anon(i32),
    Named(String, i32),
}

#[derive(Debug, Clone)]
pub struct Definition {
    pub requires: Vec<Sym>,
    pub defines: HashMap<Sym, Vec<ScopeName>>,
}

pub type CallGraph = HashMap<Vec<ScopeName>, Definition>;

#[derive(Debug, Clone)]
pub struct Root {
    pub ast: Node,
    pub graph: CallGraph,
}

pub trait Visitor<State, Res, Final, ErrT> {
    fn visit_root(&mut self, e: &Root) -> Result<Final, ErrT>;

    fn handle_error(&mut self, state: &mut State, e: &Err) -> Result<Res, ErrT>;
    fn visit_sym(&mut self, state: &mut State, e: &Sym) -> Result<Res, ErrT>;
    fn visit_prim(&mut self, state: &mut State, e: &Prim) -> Result<Res, ErrT>;
    fn visit_apply(&mut self, state: &mut State, e: &Apply) -> Result<Res, ErrT>;
    fn visit_let(&mut self, state: &mut State, e: &Let) -> Result<Res, ErrT>;
    fn visit_un_op(&mut self, state: &mut State, e: &UnOp) -> Result<Res, ErrT>;
    fn visit_bin_op(&mut self, state: &mut State, e: &BinOp) -> Result<Res, ErrT>;

    fn visit(&mut self, state: &mut State, e: &Node) -> Result<Res, ErrT> {
        // eprintln!("{:?}", e);
        use Node::*;
        match e {
            Error(n) => self.handle_error(state, n),
            SymNode(n) => self.visit_sym(state, n),
            PrimNode(n) => self.visit_prim(state, n),
            ApplyNode(n) => self.visit_apply(state, n),
            LetNode(n) => self.visit_let(state, n),
            UnOpNode(n) => self.visit_un_op(state, n),
            BinOpNode(n) => self.visit_bin_op(state, n),
        }
    }
}
