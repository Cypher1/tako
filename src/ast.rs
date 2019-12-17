#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Clone)]
pub struct CallNode {
    pub name: String,
    pub args: Vec<LetNode>,
}

#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Clone)]
pub enum PrimValue {
    Unit,
    Bool(bool),
    I32(i32),
    Str(String),
}

#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Clone)]
pub struct LetNode {
    pub call: CallNode,
    pub value: Option<Box<Node>>,
}

#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Clone)]
pub struct UnOpNode {
    pub name: String,
    pub inner: Box<Node>,
}

#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Clone)]
pub struct BinOpNode {
    pub name: String,
    pub left: Box<Node>,
    pub right: Box<Node>,
}

#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Clone)]
pub enum Node {
    Error(String),
    Call(CallNode),
    Prim(PrimValue),
    Let(LetNode),
    UnOp(UnOpNode),
    BinOp(BinOpNode),
}

pub trait Visitor<U, V, E> {
    fn visit_root(&mut self, e: &Node) -> Result<V, E>;

    fn visit_prim(&mut self, e: &PrimValue) -> Result<U, E>;
    fn visit_call(&mut self, e: &CallNode) -> Result<U, E>;
    fn visit_let(&mut self, e: &LetNode) -> Result<U, E>;
    fn visit_un_op(&mut self, e: &UnOpNode) -> Result<U, E>;
    fn visit_bin_op(&mut self, e: &BinOpNode) -> Result<U, E>;
    fn handle_error(&mut self, e: &String) -> Result<U, E>;

    fn visit(&mut self, e: &Node) -> Result<U, E> {
        match e {
            Node::Error(n) => self.handle_error(n),
            Node::Call(n) => self.visit_call(n),
            Node::Prim(n) => self.visit_prim(n),
            Node::Let(n) => self.visit_let(n),
            Node::UnOp(n) => self.visit_un_op(n),
            Node::BinOp(n) => self.visit_bin_op(n),
        }
    }
}
