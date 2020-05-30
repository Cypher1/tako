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
    Line(String),
    If {
        condition: Box<Code>,
        then: Vec<Code>,
        then_else: Vec<Code>,
    },
    Func {
        name: String,
        args: Vec<String>,
        body: Vec<Code>,
    },
    Lambda {
        name: String,
        args: Vec<String>,
        body: Vec<Code>,
    },
}

impl Code {
    fn new(expr: String) -> Code {
        Code::Line(expr)
    }
    fn with_expr(self: Code, f: &dyn Fn(String) -> Code) -> Code {
        match self {
            Code::Line(expr) => f(expr.to_owned()),
            Code::If {
                condition,
                then,
                then_else
            } => {

                Code::If {
                    condition,
                    then,
                    then_else
                }
            },
            Code::Func {
                name,
                args,
                mut body
            } => {
                let last = body.last_mut().unwrap();
                *last = last.clone().with_expr(f);
                Code::Func {
                    name,
                    args,
                    body
                }
            },
            Code::Lambda {
                name,
                args,
                mut body
            } => {
                let last = body.last_mut().unwrap();
                *last = last.clone().with_expr(f);
                Code::Lambda {
                    name,
                    args,
                    body
                }
            },
        }
    }
}

pub fn make_name(def: Vec<ScopeName>) -> String {
    let def_n: Vec<String> = def.iter().map(|n| n.clone().to_name()).collect();
    def_n.join("_")
}

fn pretty_print_block(src: Code, indent: &str) -> String {
    let new_indent = indent.to_string() + "  ";
    // Calculate the expression as well...
    // TODO: Consider if it is dropped (should it be stored? is it a side effect?)
    match src {
        Code::Line(expr) => expr,
        Code::If {
            condition,
            then,
            then_else,
        } => {
            let cond = pretty_print_block(*condition, &new_indent);
            let body: Vec<String> = then
                .iter()
                .map(|x| pretty_print_block(x.clone(), &new_indent))
                .collect();
            let then_else: Vec<String> = then_else
                .iter()
                .map(|x| pretty_print_block(x.clone(), &new_indent))
                .collect();
            format!(
                "{indent}if({}) {{{new_indent}{}{indent}}} else {{{new_indent}{}{indent}}}",
                cond,
                body.join(&new_indent),
                then_else.join(&new_indent),
                indent = indent,
                new_indent = new_indent
            )
        }
        Code::Lambda {
            name,
            args,
            body: inner,
        } => {
            let body: Vec<String> = inner
                .iter()
                .map(|x| pretty_print_block(x.clone(), &new_indent))
                .collect();
            format!(
                "{indent}const auto {} = [&] ({}) {{{new_indent}{}{indent}}};",
                name,
                args.join(", "),
                body.join(&new_indent),
                indent = indent,
                new_indent = new_indent
            )
        }
        Code::Func {
            name,
            args,
            body: inner,
        } => {
            let body: Vec<String> = inner
                .iter()
                .map(|x| pretty_print_block(x.clone(), &new_indent))
                .collect();
            format!(
                "{indent}{}({}) {{{new_indent}{}{indent}}}",
                name,
                args.join(", "),
                body.join(&new_indent),
                indent = indent,
                new_indent = new_indent
            )
        }
    }
}

type Res = Result<Code, TError>;
type State = ();
type Out = (String, HashSet<String>);

impl Compiler {
    fn build_call1(&mut self, before: &str, inner: Code) -> Code {
        inner.with_expr(&|exp| Code::Line(format!("{}({})", before, exp)))
    }
    fn build_call2(&mut self, before: &str, mid: &str, left: Code, right: Code) -> Code {
        left.with_expr(
            &|left_expr| right.clone().with_expr(
                &|right_expr| Code::Line(format!("{}({}{}{})", before, left_expr, mid, right_expr))
            )
        )
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
            } => Code::Func {
                name: "int main".to_string(),
                args: vec!["int argc".to_string(), "char* argv[]".to_string()],
                body,
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
        eprintln!(
            "to_c: visit {}, {:?}",
            expr.name,
            expr.get_info().defined_at
        );
        let name = make_name(
            expr.get_info()
                .defined_at
                .expect("Could not find definition for symbol"),
        );
        Ok(Code::new(name).clone())
    }

    fn visit_prim(&mut self, _state: &mut State, expr: &Prim) -> Res {
        use Prim::*;
        match expr {
            I32(n, _) => Ok(Code::new(n.to_string())),
            Bool(true, _) => Ok(Code::new(1.to_string())),
            Bool(false, _) => Ok(Code::new(0.to_string())),
            Str(s, _) => Ok(Code::new(format!("{:?}", s))),
            _ => unimplemented!(),
        }
    }

    fn visit_apply(&mut self, state: &mut State, expr: &Apply) -> Res {
        eprintln!("apply here: {:?}", expr);
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
                Code::Line(arg_expr) => arg_exprs.push(arg_expr.clone()),
                _ => panic!("Unexpected block used as apply argument"),
            }
        }
        // TODO: require label is none.
        let arg_str = arg_exprs.join(", ");
        match val {
            Code::Line(expr) => {
                let with_args = format!("{}({})", expr, arg_str);
                Ok(Code::Line(with_args))
            }
            _ => panic!("Don't know how to apply arguments to a block"),
        }
    }

    fn visit_let(&mut self, state: &mut State, expr: &Let) -> Res {
        eprintln!("let here: {:?}", expr.get_info().defined_at);
        eprintln!("args: {:?}", expr.args);
        for arg in (&expr.args).as_ref().unwrap_or(&vec![]) {
            eprintln!("  arg: {:?} {:?}", arg.name, arg.get_info().defined_at);
        }
        eprintln!("value: {}", expr.value);
        let name = make_name(
            expr.get_info()
                .defined_at
                .expect("Could not find definition for let"),
        );
        let code = self.visit(state, &expr.value)?;
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
                name,
                args,
                body: vec![code],
            };

            return Ok(node.with_expr(&|exp| Code::Line(format!("return {};", exp))));
        }
        Ok(code.with_expr(&|x| Code::new(format!("const int {} = {};", name, x))))
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
                    then: vec![right],
                    then_else: vec![Code::Line("throw 101;".to_string())],
                };
                return Ok(done);
            }
            ";" => {
                // TODO: handle 'error' values more widly.
                // TODO: ORDERING
                return Ok(right);
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
