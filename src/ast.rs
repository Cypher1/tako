#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Clone)]
pub struct CallNode {
    pub name: String,
    //TODO: Args
}

#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Clone)]
pub struct LetNode {
    pub name: String,
    //TODO: Args
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
    Num(i32),
    Str(String),
    Let(LetNode),
    UnOp(UnOpNode),
    BinOp(BinOpNode),
}

pub trait Visitor<U, V, E> {
    fn visit_root(&mut self, e: &Node) -> Result<V, E>;

    fn visit_num(&mut self, e: &i32) -> Result<U, E>;
    fn visit_str(&mut self, _: &String) -> Result<U, E> {
        unimplemented!();
    }
    fn visit_call(&mut self, e: &CallNode) -> Result<U, E>;
    fn visit_let(&mut self, e: &LetNode) -> Result<U, E>;
    fn visit_un_op(&mut self, e: &UnOpNode) -> Result<U, E>;
    fn visit_bin_op(&mut self, e: &BinOpNode) -> Result<U, E>;
    fn handle_error(&mut self, e: &String) -> Result<U, E>;

    fn visit(&mut self, e: &Node) -> Result<U, E> {
        match e {
            Node::Error(n) => self.handle_error(n),
            Node::Call(n) => self.visit_call(n),
            Node::Num(n) => self.visit_num(n),
            Node::Str(n) => self.visit_str(n),
            Node::Let(n) => self.visit_let(n),
            Node::UnOp(n) => self.visit_un_op(n),
            Node::BinOp(n) => self.visit_bin_op(n),
        }
    }
}
