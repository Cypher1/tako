use std::collections::HashSet;
use std::fmt;
use std::hash::{Hash, Hasher};

use crate::database::Compiler;
use crate::database::DB;
use crate::errors::TError;
use crate::location::*;
use crate::tree::*;
use crate::types::Type;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
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

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
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

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Prim {
    Bool(bool, Info),
    I32(i32, Info),
    Str(String, Info),
    Lambda(Box<Node>),
    TypeValue(Type, Info),
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
            TypeValue(_, info) => info.clone(),
        }
    }
}

// Consider finding way to turn lets into binary operators.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Let {
    pub name: String,
    pub value: Box<Node>,
    pub args: Option<Vec<Sym>>, // TODO(cypher1): Args should be let nodes.
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

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
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

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
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
    pub ty: Option<Box<Node>>,
    pub defined_at: Option<Path>,
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

// #[derive(Debug)]
#[derive(PartialEq, Eq, Clone, Hash)]
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
    Named(String, Option<String>), // name, (and for files) an optional extension
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Symbol::Anon() => write!(f, "?")?,
            Symbol::Named(name, None) => write!(f, "{}", name)?,
            Symbol::Named(name, Some(ext)) => write!(f, "{}.{}", name, ext)?,
        }
        Ok(())
    }
}

impl Symbol {
    pub fn new(name: String) -> Symbol {
        Symbol::Named(name, None)
    }
    pub fn to_name(self: &Symbol) -> String {
        match self {
            Symbol::Anon() => "".to_owned(),
            Symbol::Named(name, _) => name.to_owned(),
        }
    }
}

pub type Path = Vec<Symbol>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Entry {
    pub uses: HashSet<Path>,
    pub defined_at: Path,
    // pub requires: Vec<Sym>,
    // pub defines: HashMap<Sym, Path>,
}

impl Default for Entry {
    fn default() -> Entry {
        Entry {
            uses: HashSet::new(),
            defined_at: vec![], //TODO: Remove the default instance.
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
