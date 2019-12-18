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

pub trait Visitor<State, Res, Final, Err> {
    fn visit_root(&mut self, e: &Node) -> Result<Final, Err>;

    fn visit_prim(&mut self, e: &PrimValue) -> Result<Res, Err>;
    fn visit_call(&mut self, state: &mut State, e: &CallNode) -> Result<Res, Err>;
    fn visit_let(&mut self, state: &mut State, e: &LetNode) -> Result<Res, Err>;
    fn visit_un_op(&mut self, state: &mut State, e: &UnOpNode) -> Result<Res, Err>;
    fn visit_bin_op(&mut self, state: &mut State, e: &BinOpNode) -> Result<Res, Err>;
    fn handle_error(&mut self, state: &mut State, e: &String) -> Result<Res, Err>;

    fn visit(&mut self, state: &mut State, e: &Node) -> Result<Res, Err> {
        // println!("{:?}", e);
        match e {
            Node::Prim(n) => self.visit_prim(n),
            Node::Call(n) => self.visit_call(state, n),
            Node::Let(n) => self.visit_let(state, n),
            Node::UnOp(n) => self.visit_un_op(state, n),
            Node::BinOp(n) => self.visit_bin_op(state, n),
            Node::Error(n) => self.handle_error(state, n),
        }
    }
}
