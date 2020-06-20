use super::ast::*;
use super::cli_options::Options;
use super::errors::TError;

use std::collections::HashSet;

// Walks the AST compiling it to wasm.
pub struct Compiler {
    functions: Vec<Code>,
    includes: HashSet<String>,
    pub flags: HashSet<String>,
}

#[derive(Clone, Debug)]
pub enum Code {
    Block(Vec<Code>),
    Expr(String),
    Statement(String),
    If {
        condition: Box<Code>,
        then: Box<Code>,
        then_else: Box<Code>,
    },
    Func {
        name: String,
        args: Vec<String>,
        return_type: String,
        body: Box<Code>,
        lambda: bool,
    },
}

impl Code {
    fn with_expr(self: Code, f: &dyn Fn(String) -> Code) -> Code {
        match self {
            Code::Expr(expr) => f(expr.to_owned()),
            Code::Block(mut statements) => {
                let last = statements.pop().unwrap();
                statements.push(last.with_expr(f));
                Code::Block(statements)
            }
            Code::Statement(line) => Code::Statement(line),
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
                return_type,
            } => {
                body = Box::new(body.with_expr(f));
                Code::Func {
                    name,
                    args,
                    body,
                    lambda,
                    return_type,
                }
            }
        }
    }

    fn merge(self: Code, other: Code) -> Code {
        match (self, other) {
            (Code::Block(mut left), Code::Block(right)) => {
                left.extend(right);
                Code::Block(left)
            }
            (left, Code::Block(mut right)) => {
                right.insert(0, left);
                Code::Block(right) // Backwards?
            }
            (Code::Block(mut left), right) => {
                left.push(right);
                Code::Block(left)
            }
            (left, right) => Code::Block(vec![left, right]),
        }
    }
}

pub fn make_name(def: Vec<ScopeName>) -> String {
    let def_n: Vec<String> = def.iter().map(|n| n.clone().to_name()).collect();
    def_n.join("_")
}

fn pretty_print_block(src: Code, indent: &str) -> String {
    // Calculate the expression as well...
    // TODO: Consider if it is dropped (should it be stored? is it a side effect?)
    match src {
        Code::Block(statements) => {
            let new_indent = indent.to_string() + "  ";
            let body: Vec<String> = statements
                .iter()
                .map(|x| pretty_print_block(x.clone(), &new_indent))
                .collect();
            format!("{{{}{indent}}}", body.join(""), indent = indent,)
        }
        Code::Statement(line) => format!("{}{}", indent, line),
        Code::Expr(line) => line,
        Code::If {
            condition,
            then,
            then_else,
        } => {
            let cond = pretty_print_block(*condition, &indent);
            let body = pretty_print_block(*then, &indent);
            let then_else = pretty_print_block(*then_else, &indent);
            format!(
                "{indent}if({}) {} else {}",
                cond,
                body,
                then_else,
                indent = indent,
            )
        }
        Code::Func {
            name,
            args,
            return_type,
            body: inner,
            lambda,
        } => {
            let body = if let Code::Block(_) = *inner {
                pretty_print_block(*inner, &indent)
            } else {
                // Auto wrap statements in blocks.
                pretty_print_block(Code::Block(vec![*inner]), &indent)
            };
            if lambda {
                format!(
                    "{indent}const auto {} = [&] ({}) {};",
                    name,
                    args.join(", "),
                    body,
                    indent = indent
                )
            } else {
                format!(
                    "{indent}{} {}({}) {}",
                    return_type,
                    name,
                    args.join(", "),
                    body,
                    indent = indent
                )
            }
        }
    }
}

type Res = Result<Code, TError>;
type State = ();
type Out = (String, HashSet<String>);

impl Compiler {
    fn build_call1(&mut self, before: &str, inner: Code) -> Code {
        inner.with_expr(&|exp| Code::Expr(format!("{}({})", before, exp)))
    }
    fn build_call2(&mut self, before: &str, mid: &str, left: Code, right: Code) -> Code {
        left.with_expr(&|left_expr| {
            right.clone().with_expr(&|right_expr| {
                Code::Expr(format!("{}({}{}{})", before, left_expr, mid, right_expr))
            })
        })
    }
}

impl Visitor<State, Code, Out> for Compiler {
    fn new(_opts: &Options) -> Compiler {
        Compiler {
            functions: vec![],
            includes: HashSet::new(),
            flags: HashSet::new(),
        }
    }

    fn visit_root(&mut self, root: &Root) -> Result<Out, TError> {
        let mut main_info = root.ast.get_info();
        main_info.defined_at = Some(vec![]);
        let main_let = Let {
            info: main_info,
            name: "main".to_string(),
            value: Box::new(root.ast.clone()),
            args: Some(vec![]),
            is_function: true,
        };
        let main = match self.visit_let(&mut (), &main_let)? {
            Code::Func {
                name: _,
                args: _,
                body,
                lambda: _,
                return_type: _,
            } => Code::Func {
                name: "main".to_string(),
                args: vec!["int argc".to_string(), "char* argv[]".to_string()],
                body,
                lambda: false,
                return_type: "int".to_string(),
            },
            _ => panic!("main must be a Func"),
        };

        // TODO(cypher1): Use a writer.
        let mut code = "".to_string();

        // #includes
        for inc in self.includes.iter() {
            code = format!("{}{}\n", code, inc);
        }

        // Forward declarations
        for func in self.functions.clone().iter() {
            match &func {
                Code::Func { name, args, .. } => {
                    code = format!("{}{}({});\n", code, name, args.join(", "))
                }
                _ => panic!("Cannot create function from non-function"),
            }
        }

        self.functions.push(main);

        // Definitions
        for func in self.functions.iter().clone() {
            let function = pretty_print_block(func.to_owned(), "\n");
            code = format!("{}{}", code, function);
        }

        Ok((code + "\n", self.flags.clone()))
    }

    fn visit_sym(&mut self, _state: &mut State, expr: &Sym) -> Res {
        // eprintln!(
        //   "to_c: visit {}, {:?}",
        // expr.name,
        //   expr.get_info().defined_at
        // );
        let name = make_name(
            expr.get_info()
                .defined_at
                .expect("Could not find definition for symbol"),
        );
        Ok(Code::Expr(name).clone())
    }

    fn visit_prim(&mut self, _state: &mut State, expr: &Prim) -> Res {
        use Prim::*;
        match expr {
            I32(n, _) => Ok(Code::Expr(n.to_string())),
            Bool(true, _) => Ok(Code::Expr(1.to_string())),
            Bool(false, _) => Ok(Code::Expr(0.to_string())),
            Str(s, _) => Ok(Code::Expr(format!("{:?}", s))),
            _ => unimplemented!(),
        }
    }

    fn visit_apply(&mut self, state: &mut State, expr: &Apply) -> Res {
        // eprintln!("apply here: {:?}", expr);
        let val = self.visit(state, &expr.inner)?;
        let args: Vec<Code> = expr
            .args
            .iter()
            .map(|s| {
                self.visit(state, &s.value)
                    .expect("Could not find definition for apply argument")
            })
            .collect();
        let mut arg_exprs = vec![];
        for arg in args.iter() {
            match &arg {
                Code::Expr(arg_expr) => arg_exprs.push(arg_expr.clone()),
                _ => panic!("Unexpected block used as apply argument"),
            }
        }
        // TODO: require label is none.
        let arg_str = arg_exprs.join(", ");
        match val {
            Code::Expr(expr) => {
                let with_args = format!("{}({})", expr, arg_str);
                Ok(Code::Expr(with_args))
            }
            _ => panic!("Don't know how to apply arguments to a block"),
        }
    }

    fn visit_let(&mut self, state: &mut State, expr: &Let) -> Res {
        // eprintln!("let here: {:?}", expr.get_info().defined_at);
        // eprintln!("args: {:?}", expr.args);
        // for arg in (&expr.args).as_ref().unwrap_or(&vec![]) {
        // eprintln!("  arg: {:?} {:?}", arg.name, arg.get_info().defined_at);
        // }
        // eprintln!("value: {}", expr.value);
        let name = make_name(
            expr.get_info()
                .defined_at
                .expect("Could not find definition for let"),
        );
        let body = Box::new(self.visit(state, &expr.value)?);
        if expr.is_function {
            let args: Vec<String> = expr
                .args
                .as_ref()
                .unwrap_or(&vec![])
                .iter()
                .map(|s| {
                    format!(
                        "const int {}",
                        make_name(
                            s.get_info()
                                .defined_at
                                .expect("Could not find definition for let argument"),
                        )
                    )
                })
                .collect();

            let node = Code::Func {
                name: format!("{}", name),
                args,
                return_type: "int".to_string(),
                body,
                lambda: true,
            };

            return Ok(node.with_expr(&|exp| Code::Statement(format!("return {};", exp))));
        }
        Ok(body.with_expr(&|x| Code::Statement(format!("const int {} = {};", name, x))))
    }

    fn visit_un_op(&mut self, state: &mut State, expr: &UnOp) -> Res {
        let code = self.visit(state, &expr.inner)?;
        let info = expr.get_info();
        let res = match expr.name.as_str() {
            "+" => self.build_call1("", code),
            "-" => self.build_call1("-", code),
            "!" => self.build_call1("!", code),
            op => return Err(TError::UnknownPrefixOperator(op.to_string(), info)),
        };
        Ok(res)
    }

    fn visit_bin_op(&mut self, state: &mut State, expr: &BinOp) -> Res {
        let info = expr.get_info();
        let left = self.visit(state, &expr.left.clone())?;
        let right = self.visit(state, &expr.right.clone())?;
        // TODO: require 2 children
        let res = match expr.name.as_str() {
            "*" => self.build_call2("", "*", left, right),
            "+" => self.build_call2("", "+", left, right),
            "/" => self.build_call2("", "/", left, right), // TODO: require divisibility
            "-" => self.build_call2("", "-", left, right),
            "==" => self.build_call2("", "==", left, right),
            "!=" => self.build_call2("", "!=", left, right),
            ">" => self.build_call2("", ">", left, right),
            "<" => self.build_call2("", "<", left, right),
            ">=" => self.build_call2("", ">=", left, right),
            "<=" => self.build_call2("", "<=", left, right),
            "^" => {
                self.includes.insert("#include <math.h>".to_string());
                self.flags.insert("-lm".to_string());
                self.build_call2("pow", ", ", left, right)
            } // TODO: require pos pow
            "-|" => {
                // TODO: handle 'error' values more widly.
                let done = Code::If {
                    condition: Box::new(left),
                    then: Box::new(right),
                    then_else: Box::new(Code::Statement("throw 101;".to_string())),
                };
                return Ok(done);
            }
            ";" => {
                // TODO: handle 'error' values more widly.
                // TODO: ORDERING
                return Ok(left.merge(right));
            }
            op => return Err(TError::UnknownInfixOperator(op.to_string(), info)),
        };
        // TODO: Short circuiting of deps.
        Ok(res)
    }

    fn handle_error(&mut self, _state: &mut State, expr: &Err) -> Res {
        Err(TError::FailedParse(expr.msg.clone(), expr.get_info()))
    }
}
