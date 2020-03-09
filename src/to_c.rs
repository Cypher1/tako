use super::tree::*;
use super::ast::*;

#[derive(Debug, PartialEq)]
pub enum CompilerError {
    UnknownInfixOperator(String, Info),
    UnknownPrefixOperator(String, Info),
    FailedParse(String, Info),
}

// Walks the AST compiling it to wasm.
pub struct Compiler {
    forward_decls: String,
    functions: String,
}

impl Default for Compiler {
    fn default() -> Compiler {
        Compiler {
            forward_decls: "".to_string(),
            functions: "".to_string(),
        }
    }
}

pub fn make_name(def: Vec<ScopeName>) -> String {
    let def_n: Vec<String> = def[1..].into_iter().map(|n| n.clone().to_name()).collect();
    return format!("{}", def_n.join("_"));
}

fn build_src(src: &Tree<String>, indent: &String) -> (String, String) {
    let mut body = "".to_string();
    let next_indent = indent.clone()+&"  ".to_string();
    for child in src.children.iter() {
        let (dep, expr) = build_src(child, &next_indent);
        body = format!("{}{}{}{}", body, dep, indent, expr);
    }
    (body, src.value.clone())
}

type Res = Result<Tree<String>, CompilerError>;
type State = ();
impl Visitor<State, Tree<String>, String, CompilerError> for Compiler {
    fn visit_root(&mut self, root: &Root) -> Result<String, CompilerError> {
        let includes = "#include <stdio.h>\n#include <math.h>\n\n";
        let children = self.visit(&mut (), &root.ast)?;
        let (body, ret) = build_src(&children, &"\n".to_string());
        let main = format!("{}\nint main() {{\n  return {};\n}}", body, ret);
        Ok(format!(
                "{}{}{}{}",
                includes,
                self.forward_decls,
                self.functions,
                main
            )
        )
    }

    fn visit_sym(&mut self, _state: &mut State, expr: &Sym) -> Res {
        let name = make_name(expr.get_info().defined_at.unwrap());
        Ok(to_root(&name))
    }

    fn visit_prim(&mut self, _state: &mut State, expr: &Prim) -> Res {
        use Prim::*;
        match expr {
            I32(n, _) => Ok(to_root(&n.to_string())),
            Bool(true, _) => Ok(to_root(&"1".to_string())),
            Bool(false, _) => Ok(to_root(&"0".to_string())),
            _ => unimplemented!(),
        }
    }

    fn visit_apply(&mut self, state: &mut State, expr: &Apply) -> Res {
        let val = self.visit(state, &expr.inner)?;
        let args: Vec<Tree<String>> = expr.args
            .iter().map(|s| self.visit(state, &s.value).unwrap())
            .collect();
        let mut children = vec![];
        let mut arg_exprs = vec![];
        for arg in args.iter() {
            arg_exprs.push(arg.value.clone());
            children.extend(arg.children.clone());
        }
        let arg_str = arg_exprs.join(", ");
        Ok(to_root(&format!("{}({})", val, arg_str)))
    }

    fn visit_let(&mut self, state: &mut State, expr: &Let) -> Res {
        let name = make_name(expr.get_info().defined_at.unwrap());
        let val = self.visit(state, &expr.value)?;
        if expr.is_function {
            let emp = vec![];
            let args: Vec<String> = expr.args.as_ref()
                .unwrap_or(&emp).iter().map(|s| make_name(
                    s.get_info().defined_at.unwrap()
                ).clone())
                .collect();
            let arg_str = args.join(", ");
            self.forward_decls = format!("{}int {}({});\n", self.forward_decls, name, arg_str);
            self.functions = format!("{}int {}({}) {{\n  return {};\n}}\n", self.functions, name, arg_str, val);
            return Ok(to_root(&"".to_string()));
        }
        Ok(to_root(&format!("const int {} = {};", name, val)))
    }

    fn visit_un_op(&mut self, state: &mut State, expr: &UnOp) -> Res {
        let inner = self.visit(state, &expr.inner)?;
        let info = expr.get_info();
        match expr.name.as_str() {
            "+" => Ok(inner),
            "-" => Ok(to_root(&format!("-({})", inner))),
            "!" => Ok(to_root(&format!("!({})", inner))),
            op => Err(CompilerError::UnknownPrefixOperator(op.to_string(), info))
        }
    }
    fn visit_bin_op(&mut self, state: &mut State, expr: &BinOp) -> Res {
        let info = expr.get_info();
        let left = self.visit(state, &expr.left.clone())?;
        let right = self.visit(state, &expr.right.clone())?;
        // TODO: require 2 children
        let s = match expr.name.as_str() {
            "*" => format!("({}*{})", left, right),
            "+" => format!("({}+{})", left, right),
            "/" => format!("({}/{})", left, right), // TODO: require divisibility
            "-" => format!("({}-{})", left, right),
            "==" => format!("({}=={})", left, right),
            "!=" => format!("({}!={})", left, right),
            ">" => format!("({}>{})", left, right),
            "<" => format!("({}<{})", left, right),
            ">=" => format!("({}>={})", left, right),
            "<=" => format!("({}<={})", left, right),
            "^" => format!("pow({}, {})", left, right), // TODO: require pos pow
            ";" => {
                let mut children = vec![left];
                children.extend(right.children);
                return Ok(Tree {children, value: right.value});
            },
            op => return Err(CompilerError::UnknownInfixOperator(op.to_string(), info)),
        };
        Ok(to_root(&s))
    }

    fn handle_error(&mut self, _state: &mut State, expr: &Err) -> Res {
        Err(CompilerError::FailedParse(
            expr.msg.clone(),
            expr.get_info(),
        ))
    }
}
