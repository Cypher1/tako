use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};

use crate::database::Compiler;
use crate::database::DB;
use crate::errors::TError;
use crate::location::*;
use crate::tree::*;
use crate::types::*;

#[derive(Debug, PartialEq, Eq, Clone)]
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

#[derive(Debug, PartialEq, Eq, Clone)]
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
    pub info: Info,
}

impl ToNode for Sym {
    fn to_node(self) -> Node {
        Node::SymNode(self)
    }
    fn get_info(&self) -> Info {
        self.info.clone()
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Let {
    pub name: String,
    pub value: Box<Node>,
    pub args: Option<Vec<Sym>>, // TODO(cypher1): Args should be let nodes.
    pub is_function: bool,
    pub info: Info,
}

impl Let {
    pub fn to_sym(self: &Let) -> Sym {
        Sym {
            name: self.name.clone(),
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

#[derive(Debug, PartialEq, Eq, Clone)]
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

#[derive(Debug, PartialEq, Eq, Clone)]
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

#[derive(Clone, Debug)]
pub struct Definition {
    pub requires: Vec<Sym>,
    pub defines: HashMap<Sym, Vec<Symbol>>,
}

#[derive(Clone)]
pub struct Info {
    pub loc: Option<Loc>,
    pub ty: Option<TypeInfo>,
    pub defined_at: Option<Vec<Symbol>>,
    pub callable: bool,
}

impl Default for Info {
    fn default() -> Info {
        Info {
            loc: None,
            ty: None,
            defined_at: None,
            callable: false,
        }
    }
}

impl std::fmt::Debug for Info {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(loc) = &self.loc {
            write!(f, "{:?}", loc)?;
        }
        if let Some(ty) = &self.ty {
            write!(f, "{:?}", ty)?;
        }
        if let Some(def) = &self.defined_at {
            write!(f, " from {:?}", def)?;
        }
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

#[derive(Debug, PartialEq, Clone)]
pub struct TypeInfo {
    reqs: Vec<Node>,
    structure: DataType,
}

// #[derive(Debug)]
#[derive(PartialEq, Eq, Clone)]
pub enum Node {
    Error(Err),
    SymNode(Sym),
    PrimNode(Prim),
    ApplyNode(Apply),
    LetNode(Let),
    UnOpNode(UnOp),
    BinOpNode(BinOp),
    BuiltIn(String),
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
            BuiltIn(n) => n.fmt(f),
        }
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use super::PrettyPrint;
        let db = DB::default();
        match PrettyPrint::default().visit_root(&db, &self) {
            Ok(res) => write!(f, "{}", res),
            Err(err) => write!(f, "{:#?}", err),
        }
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
            BuiltIn(_) => Info::default(), // TODO: Add info about the built in.
        }
    }
}

pub trait ToNode {
    fn to_node(self: Self) -> Node;
    fn get_info(self: &Self) -> Info;
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Symbol {
    Anon(),
    Named(String),
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Symbol::Anon() => write!(f, "?")?,
            Symbol::Named(name) => write!(f, "{}", name)?,
        }
        Ok(())
    }
}

impl Symbol {
    pub fn to_name(self: &Symbol) -> String {
        match self {
            Symbol::Anon() => "?".to_owned(),
            Symbol::Named(name) => name.to_owned(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Entry {
    pub uses: Vec<Vec<Symbol>>,
    // pub requires: Vec<Sym>,
    // pub defines: HashMap<Sym, Vec<Symbol>>,
}

impl Default for Entry {
    fn default() -> Entry {
        Entry {
            uses: vec![],
            // requires: vec![],
            // defines: HashMap::new(),
        }
    }
}

pub type Table = HashTree<Symbol, Entry>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Root {
    pub ast: Node,
    pub table: Table,
}

impl fmt::Display for Root {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.ast.fmt(f)?;
        write!(f, "{:?}", self.table)?;
        Ok(())
    }
}

pub trait Visitor<State, Res, Final, Start = Root> {
    fn visit_root(&mut self, db: &dyn Compiler, e: &Start) -> Result<Final, TError>;

    fn handle_error(
        &mut self,
        db: &dyn Compiler,
        state: &mut State,
        e: &Err,
    ) -> Result<Res, TError>;
    fn visit_sym(&mut self, db: &dyn Compiler, state: &mut State, e: &Sym) -> Result<Res, TError>;
    fn visit_prim(&mut self, db: &dyn Compiler, state: &mut State, e: &Prim)
        -> Result<Res, TError>;
    fn visit_apply(
        &mut self,
        db: &dyn Compiler,
        state: &mut State,
        e: &Apply,
    ) -> Result<Res, TError>;
    fn visit_let(&mut self, db: &dyn Compiler, state: &mut State, e: &Let) -> Result<Res, TError>;
    fn visit_un_op(
        &mut self,
        db: &dyn Compiler,
        state: &mut State,
        e: &UnOp,
    ) -> Result<Res, TError>;
    fn visit_bin_op(
        &mut self,
        db: &dyn Compiler,
        state: &mut State,
        e: &BinOp,
    ) -> Result<Res, TError>;
    fn visit_built_in(
        &mut self,
        db: &dyn Compiler,
        state: &mut State,
        e: &str,
    ) -> Result<Res, TError>;

    fn visit(&mut self, db: &dyn Compiler, state: &mut State, e: &Node) -> Result<Res, TError> {
        // eprintln!("{:?}", e);
        use Node::*;
        match e {
            Error(n) => self.handle_error(db, state, n),
            SymNode(n) => self.visit_sym(db, state, n),
            PrimNode(n) => self.visit_prim(db, state, n),
            ApplyNode(n) => self.visit_apply(db, state, n),
            LetNode(n) => self.visit_let(db, state, n),
            UnOpNode(n) => self.visit_un_op(db, state, n),
            BinOpNode(n) => self.visit_bin_op(db, state, n),
            BuiltIn(n) => self.visit_built_in(db, state, n),
        }
    }

    fn process(root: &Start, db: &dyn Compiler) -> Result<Final, TError>
    where
        Self: Sized,
        Self: Default,
    {
        Self::default().visit_root(db, root)
    }
}
