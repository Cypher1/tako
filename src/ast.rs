
#[derive(Debug)]
pub struct UnOpNode {
    pub name: String,
    pub inner: Box<Node>,
}

#[derive(Debug)]
pub struct BinOpNode {
    pub name: String,
    pub left: Box<Node>,
    pub right: Box<Node>,
}

#[derive(Debug)]
pub enum Node {
    Error(String),
    Num(i32),
    UnOp(UnOpNode),
    BinOp(BinOpNode),
}

pub trait Visitor<U, V> {
    fn visit_root(&mut self, e: &Node) -> V;

    fn visit_num(&mut self, e: &i32) -> U;
    fn visit_un_op(&mut self, e: &UnOpNode) -> U;
    fn visit_bin_op(&mut self, e: &BinOpNode) -> U;
    fn handle_error(&mut self, e: &String) -> U;

    fn visit(&mut self, e: &Node) -> U {
        match e {
            Node::Error(n) => self.handle_error(n),
            Node::Num(n) => self.visit_num(n),
            Node::UnOp(n) => self.visit_un_op(n),
            Node::BinOp(n) => self.visit_bin_op(n),
        }
    }
}
