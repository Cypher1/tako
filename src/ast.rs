#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Clone)]
pub struct Apply {
    pub inner: Box<Node>,
    pub args: Vec<Let>,
}

#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Clone)]
pub struct Sym {
    pub name: String,
}

#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Clone)]
pub enum Prim {
    Unit,
    Bool(bool),
    I32(i32),
    Str(String),
}

#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Clone)]
pub struct Let {
    pub name: String,
    pub value: Option<Box<Node>>,
}

#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Clone)]
pub struct UnOp {
    pub name: String,
    pub inner: Box<Node>,
}

#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Clone)]
pub struct BinOp {
    pub name: String,
    pub left: Box<Node>,
    pub right: Box<Node>,
}

#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Clone)]
pub enum Node {
    Error(String),
    SymNode(Sym),
    PrimNode(Prim),
    ApplyNode(Apply),
    LetNode(Let),
    UnOpNode(UnOp),
    BinOpNode(BinOp),
}

pub trait Visitor<State, Res, Final, Err> {
    fn visit_root(&mut self, e: &Node) -> Result<Final, Err>;

    fn handle_error(&mut self, state: &mut State, e: &String) -> Result<Res, Err>;
    fn visit_sym(&mut self, state: &mut State, e: &Sym) -> Result<Res, Err>;
    fn visit_prim(&mut self, e: &Prim) -> Result<Res, Err>;
    fn visit_apply(&mut self, state: &mut State, e: &Apply) -> Result<Res, Err>;
    fn visit_let(&mut self, state: &mut State, e: &Let) -> Result<Res, Err>;
    fn visit_un_op(&mut self, state: &mut State, e: &UnOp) -> Result<Res, Err>;
    fn visit_bin_op(&mut self, state: &mut State, e: &BinOp) -> Result<Res, Err>;

    fn visit(&mut self, state: &mut State, e: &Node) -> Result<Res, Err> {
        // println!("{:?}", e);
        use Node::*;
        match e {
            Error(n) => self.handle_error(state, n),
            SymNode(n) => self.visit_sym(state, n),
            PrimNode(n) => self.visit_prim(n),
            ApplyNode(n) => self.visit_apply(state, n),
            LetNode(n) => self.visit_let(state, n),
            UnOpNode(n) => self.visit_un_op(state, n),
            BinOpNode(n) => self.visit_bin_op(state, n),
        }
    }
}
