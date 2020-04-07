use super::ast::*;
use super::tree::*;
use super::cli_options::Options;
use super::errors::TError;

use std::collections::HashSet;

// Walks the AST compiling it to wasm.
pub struct Compiler {
    functions: Vec<Tree<Code>>,
    includes: HashSet<String>,
    pub flags: HashSet<String>,
}

#[derive(Clone)]
pub struct Code {
    // e.g. Some("int main", vec!["int argc", "char* argv[]"])
    label: Option<(String, Vec<String>)>,
    expr: String, // e.g. 3;
                  // Should print as
                  // int main(int argc, char* argv[]) {
                  //   ... // Lines from this nodes children
                  //   return 3;
                  // }
}

impl Code {
    fn new(expr: String) -> Code {
        Code { label: None, expr }
    }
    fn block(label: Option<(String, Vec<String>)>, expr: String) -> Code {
        Code { label, expr }
    }
}

pub fn make_name(def: Vec<ScopeName>) -> String {
    let def_n: Vec<String> = def[1..].iter().map(|n| n.clone().to_name()).collect();
    def_n.join("_")
}

fn pretty_print_block(src: Tree<Code>, indent: &str) -> String {
    let mut body = "".to_string();
    let next_indent = indent.to_string() + &"  ".to_string();
    for child in src.children.iter() {
        let contents = pretty_print_block(child.to_owned(), &next_indent);
        body = format!("{}{}", body, contents);
    }
    // Calculate the expression as well...
    // TODO: Consider if it is dropped (should it be stored? is it a side effect?)
    body = format!("{}{}{}", body, &next_indent, src.value.expr);
    let header = if let Some((label, args)) = src.value.label {
        body = format!("{{{}{}}}", body, indent);
        format!("{}{}({}) ", indent, label, args.join(", "))
    } else {
        indent.to_owned()
    };
    format!("{}{}", header, body)
}

type Res = Result<Tree<Code>, TError>;
type State = ();
type Out = (String, HashSet<String>);
impl Visitor<State, Tree<Code>, Out> for Compiler {
    fn new(_opts: &Options) -> Compiler {
        Compiler {
            functions: vec![],
            includes: HashSet::new(),
            flags: HashSet::new(),
        }
    }

    fn visit_root(&mut self, root: &Root) -> Result<Out, TError> {
        let child = self.visit(&mut (), &root.ast)?;

        // TODO(cypher1): Use a writer.
        let mut code = "".to_string();

        // #includes
        for inc in self.includes.iter() {
            code = format!("{}{}\n", code, inc);
        }

        // Forward declarations
        for func in self.functions.clone().iter() {
            if let Some(decl) = func.value.label.clone() {
                code = format!("{}{}({});\n", code, decl.0, decl.1.join(", "));
            }
        }

        let main_body = Tree {
            value: Code {
                expr: format!("return {};", child.value.expr),
                label: Some((
                    "int main".to_string(),
                    vec!["int argc".to_string(), "char* argv[]".to_string()],
                )),
            },
            children: child.children,
        };
        self.functions.push(main_body);

        // Definitions
        for func in self.functions.iter().clone() {
            let function = pretty_print_block(func.to_owned(), &"\n");
            code = format!("{}{}", code, function);
        }

        Ok((code, self.flags.clone()))
    }

    fn visit_sym(&mut self, _state: &mut State, expr: &Sym) -> Res {
        let name = make_name(expr.get_info().defined_at.unwrap());
        Ok(to_root(Code::new(name).clone()))
    }

    fn visit_prim(&mut self, _state: &mut State, expr: &Prim) -> Res {
        use Prim::*;
        match expr {
            I32(n, _) => Ok(to_root(Code::new(n.to_string()).clone())),
            Bool(true, _) => Ok(to_root(Code::new(1.to_string()).clone())),
            Bool(false, _) => Ok(to_root(Code::new(0.to_string()).clone())),
            _ => unimplemented!(),
        }
    }

    fn visit_apply(&mut self, state: &mut State, expr: &Apply) -> Res {
        let val = self.visit(state, &expr.inner)?;
        let args: Vec<Tree<Code>> = expr
            .args
            .iter()
            .map(|s| self.visit(state, &s.value).unwrap())
            .collect();
        let mut children = vec![];
        let mut arg_exprs = vec![];
        for arg in args.iter() {
            arg_exprs.push(arg.value.expr.clone());
            children.extend(arg.children.clone());
        }
        // TODO: require label is none.
        let arg_str = arg_exprs.join(", ");
        let with_args = format!("{}({})", val.value.expr, arg_str);
        Ok(Tree {
            value: Code::new(with_args),
            children,
        })
    }

    fn visit_let(&mut self, state: &mut State, expr: &Let) -> Res {
        let name = make_name(expr.get_info().defined_at.unwrap());
        let code = self.visit(state, &expr.value)?;
        if expr.is_function {
            let args: Vec<String> = expr
                .args
                .as_ref()
                .unwrap_or(&vec![])
                .iter()
                .map(|s| make_name(s.get_info().defined_at.unwrap()))
                .collect();
            let label = format!("int {}", name);
            let node = Code::block(Some((label, args)), format!("return {};", code.value.expr));
            return Ok(Tree {
                value: node,
                children: code.children,
            });
        }
        Ok(Tree {
            value: Code::new(format!("const int {} = {};", name, code.value.expr)),
            children: code.children,
        })
    }

    fn visit_un_op(&mut self, state: &mut State, expr: &UnOp) -> Res {
        let code = self.visit(state, &expr.inner)?;
        let info = expr.get_info();
        let res = match expr.name.as_str() {
            "+" => code.value.expr,
            "-" => format!("-({})", code.value.expr),
            "!" => format!("!({})", code.value.expr),
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
            "*" => format!("({}*{})", left.value.expr, right.value.expr),
            "+" => format!("({}+{})", left.value.expr, right.value.expr),
            "/" => format!("({}/{})", left.value.expr, right.value.expr), // TODO: require divisibility
            "-" => format!("({}-{})", left.value.expr, right.value.expr),
            "==" => format!("({}=={})", left.value.expr, right.value.expr),
            "!=" => format!("({}!={})", left.value.expr, right.value.expr),
            ">" => format!("({}>{})", left.value.expr, right.value.expr),
            "<" => format!("({}<{})", left.value.expr, right.value.expr),
            ">=" => format!("({}>={})", left.value.expr, right.value.expr),
            "<=" => format!("({}<={})", left.value.expr, right.value.expr),
            "^" => {
                self.includes.insert("#include <math.h>".to_string());
                self.flags.insert("-lm".to_string());
                format!("pow({}, {})", left.value.expr, right.value.expr)
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
        Err(TError::FailedParse(
            expr.msg.clone(),
            expr.get_info(),
        ))
    }
}
