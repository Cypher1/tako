#[derive(Clone, Debug)]
pub enum Code<ID> {
    Partial(ID),
    Empty,
    Block(Vec<Code<ID>>),
    Struct(Vec<Code<ID>>),
    Expr(String),
    Statement(String),
    Template(String, Box<Code<ID>>),
    Assignment(String, Box<Code<ID>>),
    If {
        condition: Box<Code<ID>>,
        then: Box<Code<ID>>,
        then_else: Box<Code<ID>>,
    },
    Func {
        name: String,
        args: Vec<String>,
        return_type: String,
        body: Box<Code<ID>>,
        lambda: bool,
        call: bool,
    },
}

impl<ID> Code<ID> {
    pub fn with_expr(self: Code<ID>, f: &dyn Fn(String) -> Code<ID>) -> Code<ID> {
        match self {
            Code::Partial(ent) => Code::Partial(ent),
            Code::Empty => Code::Empty,
            Code::Expr(expr) => f(expr),
            Code::Struct(values) => Code::Struct(values),
            Code::Block(mut statements) => {
                let last = statements.pop().expect("Unexpected empty code block");
                statements.push(last.with_expr(f));
                Code::Block(statements)
            }
            Code::Statement(line) => Code::Statement(line),
            Code::Template(name, body) => Code::Template(name, Box::new(body.with_expr(f))),
            Code::Assignment(name, value) => Code::Assignment(name, Box::new(value.with_expr(f))),
            Code::If {
                condition,
                then,
                then_else,
            } => Code::If {
                condition,
                then,
                then_else,
            },
            Code::Func {
                name,
                args,
                mut body,
                lambda,
                call,
                return_type,
            } => {
                body = Box::new(body.with_expr(f));
                Code::Func {
                    name,
                    args,
                    body,
                    lambda,
                    call,
                    return_type,
                }
            }
        }
    }

    pub fn merge(self: Code<ID>, other: Code<ID>) -> Code<ID> {
        match (self, other) {
            (Code::Empty, right) => right,
            (left, Code::Empty) => left,
            (Code::Block(mut left), Code::Block(right)) => {
                left.extend(right);
                Code::Block(left)
            }
            (mut left, Code::Block(mut right)) => {
                if let Code::Expr(expr) = left {
                    left = Code::Statement(expr);
                }
                right.insert(0, left);
                Code::Block(right) // Backwards?
            }
            (Code::Block(mut left), right) => {
                for line in &mut left {
                    if let Code::Expr(expr) = line {
                        *line = Code::Statement(expr.clone());
                    }
                }
                left.push(right);
                Code::Block(left)
            }
            (mut left, right) => {
                if let Code::Expr(expr) = left {
                    left = Code::Statement(expr);
                }
                Code::Block(vec![left, right])
            }
        }
    }
}
