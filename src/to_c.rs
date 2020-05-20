use super::ast::*;
use super::cli_options::Options;
use super::errors::TError;
use super::tree::*;

use std::collections::HashSet;

// Walks the AST compiling it to wasm.
pub struct Compiler {
    functions: Vec<Tree<Code>>,
    includes: HashSet<String>,
    pub flags: HashSet<String>,
}

#[derive(Clone)]
pub enum Code {
    Line(String),
    Block {
        // e.g. Some("int main", vec!["int argc", "char* argv[]"])
        label: Option<(String, Vec<String>)>,
        body: Box<Tree<Code>>, // e.g. 3;
                               // Should print as
                               // int main(int argc, char* argv[]) {
                               //   ... // Lines from this nodes children
                               //   return 3;
                               // }
    },
}

impl Code {
    fn new(expr: String) -> Code {
        Code::Line(expr)
    }
    fn block(label: Option<(String, Vec<String>)>, expr: Code) -> Code {
        Code::Block {
            label,
            body: Box::new(Tree {
                value: expr,
                children: vec![],
            }),
        }
    }
    fn get_expr(self: &Code) -> &String {
        match self {
            Code::Line(expr) => expr,
            Code::Block { label: _, body } => body.value.get_expr(),
        }
    }
}

pub fn make_name(def: Vec<ScopeName>) -> String {
    let def_n: Vec<String> = def.iter().map(|n| n.clone().to_name()).collect();
    def_n.join("_")
}

fn pretty_print_block(src: Tree<Code>, indent: &str) -> String {
    let mut block = "".to_string();
    let new_indent = indent.to_string() + "  ";
    for child in src.children.iter() {
        let contents = pretty_print_block(child.to_owned(), &new_indent);
        block = format!("{}{}", block, contents);
    }
    // Calculate the expression as well...
    // TODO: Consider if it is dropped (should it be stored? is it a side effect?)
    match src.value {
        Code::Block { label, body } => {
            let inner = pretty_print_block(*body, &new_indent);
            let rest = format!("{{{}{}\n{}}};", block, inner, indent);
            match label {
                Some((label, args)) => {
                    format!("\n{}{}({}) {}", indent, label, args.join(", "), rest)
                }
                None => format!("\n{}{}", indent, rest),
            }
        }
        Code::Line(expr) => format!("{}\n{}{}", block, indent, expr),
    }
}

type Res = Result<Tree<Code>, TError>;
type State = ();
type Out = (String, HashSet<String>);

impl Compiler {
    fn build_call1(&mut self, before: &str, inner: Code) -> String {
        format!("{}({})", before, inner.get_expr())
    }
    fn build_call2(&mut self, before: &str, mid: &str, left: Code, right: Code) -> String {
        format!("{}({}{}{})", before, left.get_expr(), mid, right.get_expr())
    }
}

impl Visitor<State, Tree<Code>, Out> for Compiler {
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
            Tree {
                value: Code::Block { label: _, body },
                children,
            } => Tree {
                value: Code::Block {
                    label: Some((
                        "int main".to_string(),
                        vec!["int argc".to_string(), "char* argv[]".to_string()],
                    )),
                    body,
                },
                children,
            },
            _ => panic!("main must be a block"),
        };

        // TODO(cypher1): Use a writer.
        let mut code = "".to_string();

        // #includes
        for inc in self.includes.iter() {
            code = format!("{}{}\n", code, inc);
        }

        // Forward declarations
        for func in self.functions.clone().iter() {
            match &func.value {
                Code::Block {
                    label: None,
                    body: _,
                } => panic!("Cannot create function without 'label'"),
                Code::Line(_) => panic!("Cannot create function without 'block'"),
                Code::Block {
                    label: Some((label, args)),
                    ..
                } => {
                    code = format!("{}{}({});\n", code, label, args.join(", "));
                }
            }
        }

        self.functions.push(main);

        // Definitions
        for func in self.functions.iter().clone() {
            let function = pretty_print_block(func.to_owned(), "");
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
        Ok(to_root(Code::new(name).clone()))
    }

    fn visit_prim(&mut self, _state: &mut State, expr: &Prim) -> Res {
        use Prim::*;
        match expr {
            I32(n, _) => Ok(to_root(Code::new(n.to_string()))),
            Bool(true, _) => Ok(to_root(Code::new(1.to_string()))),
            Bool(false, _) => Ok(to_root(Code::new(0.to_string()))),
            Str(s, _) => Ok(to_root(Code::new(format!("{:?}", s)))),
            _ => unimplemented!(),
        }
    }

    fn visit_apply(&mut self, state: &mut State, expr: &Apply) -> Res {
        eprintln!("apply here: {:?}", expr);
        let val = self.visit(state, &expr.inner)?;
        let args: Vec<Tree<Code>> = expr
            .args
            .iter()
            .map(|s| {
                self.visit(state, &s.value)
                    .expect("Could not find definition for apply argument")
            })
            .collect();
        let mut children = vec![];
        let mut arg_exprs = vec![];
        for arg in args.iter() {
            match &arg.value {
                Code::Line(arg_expr) => arg_exprs.push(arg_expr.clone()),
                Code::Block { .. } => panic!("Unexpected block used as apply argument"),
            }
            children.extend(arg.children.clone());
        }
        // TODO: require label is none.
        let arg_str = arg_exprs.join(", ");
        match val.value {
            Code::Line(expr) => {
                let with_args = format!("{}({})", expr, arg_str);
                Ok(Tree {
                    value: Code::Line(with_args),
                    children,
                })
            }
            Code::Block { .. } => panic!("Don't know how to apply arguments to a block"),
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
        let ret = code.value.get_expr(); // TODO: Handle body.children?
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

            let label = format!("const auto {} = [&] ", name);
            let node = Code::block(Some((label, args)), Code::new(format!("return {};", ret)));

            return Ok(Tree {
                value: node,
                children: code.children,
            });
        }
        Ok(Tree {
            value: Code::new(format!("const int {} = {};", name, ret)),
            children: code.children,
        })
    }

    fn visit_un_op(&mut self, state: &mut State, expr: &UnOp) -> Res {
        let code = self.visit(state, &expr.inner)?;
        let info = expr.get_info();
        let res = match expr.name.as_str() {
            "+" => self.build_call1("", code.value),
            "-" => self.build_call1("-", code.value),
            "!" => self.build_call1("!", code.value),
            op => return Err(TError::UnknownPrefixOperator(op.to_string(), info)),
        };
        Ok(Tree {
            value: Code::new(res),
            children: code.children,
        })
    }

    fn visit_bin_op(&mut self, state: &mut State, expr: &BinOp) -> Res {
        let info = expr.get_info();
        let left = self.visit(state, &expr.left.clone())?;
        let right = self.visit(state, &expr.right.clone())?;
        let mut children = vec![];
        children.extend(left.children);
        // TODO: require 2 children
        let s = match expr.name.as_str() {
            "*" => self.build_call2("", "*", left.value, right.value),
            "+" => self.build_call2("", "+", left.value, right.value),
            "/" => self.build_call2("", "/", left.value, right.value), // TODO: require divisibility
            "-" => self.build_call2("", "-", left.value, right.value),
            "==" => self.build_call2("", "==", left.value, right.value),
            "!=" => self.build_call2("", "!=", left.value, right.value),
            ">" => self.build_call2("", ">", left.value, right.value),
            "<" => self.build_call2("", "<", left.value, right.value),
            ">=" => self.build_call2("", ">=", left.value, right.value),
            "<=" => self.build_call2("", "<=", left.value, right.value),
            "^" => {
                self.includes.insert("#include <math.h>".to_string());
                self.flags.insert("-lm".to_string());
                self.build_call2("pow", ", ", left.value, right.value)
            } // TODO: require pos pow
            "-|" => {
                // TODO: handle 'error' values more widly.
                children.push(to_root(left.value.clone()));
                children.extend(right.children);
                return Ok(Tree {
                    children,
                    value: right.value,
                });
            }
            ";" => {
                // TODO: handle 'error' values more widly.
                children.push(to_root(left.value.clone()));
                children.extend(right.children);
                return Ok(Tree {
                    children,
                    value: right.value,
                });
            }
            op => return Err(TError::UnknownInfixOperator(op.to_string(), info)),
        };
        // TODO: Short circuiting of deps.
        children.extend(right.children);
        Ok(Tree {
            value: Code::new(s),
            children,
        })
    }

    fn handle_error(&mut self, _state: &mut State, expr: &Err) -> Res {
        Err(TError::FailedParse(expr.msg.clone(), expr.get_info()))
    }
}
