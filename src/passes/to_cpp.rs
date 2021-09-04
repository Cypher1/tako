use crate::ast::{Abs, Apply, BinOp, HasInfo, Let, Path, PathRef, Sym, Symbol, UnOp, Visitor};
use crate::primitives::{Prim, Val};
use crate::symbol_table::Table;
use crate::{database::DBStorage, errors::TError};
use log::debug;
use std::collections::HashSet;

// Walks the AST compiling it to wasm.
#[derive(Default)]
pub struct CodeGenerator {
    functions: Vec<Code>,
    includes: HashSet<String>,
    pub flags: HashSet<String>,
}

#[derive(Clone, Debug)]
pub enum Code {
    Empty,
    Block(Vec<Code>),
    Struct(Vec<Code>),
    Expr(String),
    Statement(String),
    Template(String, Box<Code>),
    Assignment(String, Box<Code>),
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
        call: bool,
    },
}

impl Code {
    fn with_expr(self: Code, f: &dyn Fn(String) -> Code) -> Code {
        match self {
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

    fn merge(self: Code, other: Code) -> Code {
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

#[must_use]
pub fn make_name(def: PathRef) -> String {
    let def_n: Vec<String> = def.iter().map(|n| n.clone().to_name()).collect();
    def_n.join("_")
}

fn pretty_print_block(src: Code, indent: &str) -> String {
    let new_indent = indent.to_string() + "  ";
    // Calculate the expression as well...
    // TODO: Consider if it is dropped (should it be stored? is it a side effect?)
    match src {
        Code::Block(mut statements) => {
            let last = statements.pop().expect("Unexpected empty code block");
            statements.push(last.with_expr(&|exp| Code::Statement(format!("return {}", exp))));
            let body: Vec<String> = statements
                .iter()
                .map(|x| pretty_print_block(x.clone(), new_indent.as_str()))
                .collect();
            format!("{{{}{indent}}}", body.join(""), indent = indent,)
        }
        Code::Struct(vals) => {
            let body: Vec<String> = vals
                .iter()
                .map(|x| pretty_print_block(x.clone(), new_indent.as_str()))
                .collect();
            format!("{{{}{indent}}}", body.join(", "), indent = indent,)
        }
        Code::Expr(line) => line,
        Code::Statement(line) => format!("{}{};", indent, line),
        Code::Template(name, body) => format!(
            "template <{} {}>\n{}",
            "typename",
            name,
            pretty_print_block(*body, indent)
        ),
        Code::Assignment(name, value) => format!(
            "{}const auto {} = {};",
            indent,
            name,
            pretty_print_block(*value, indent)
        ),
        Code::Empty => "".to_string(),
        Code::If {
            condition,
            then,
            then_else,
        } => {
            let condition = pretty_print_block(*condition, indent);
            let body = pretty_print_block(*then, indent);
            let then_else = pretty_print_block(*then_else, indent);
            format!(
                "{indent}if({}) {} else {}",
                condition,
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
            call,
        } => {
            let inner = if let Code::Block(_) = *inner {
                *inner
            } else {
                // Auto wrap statements in blocks.
                Code::Block(vec![*inner])
            };
            let body = pretty_print_block(inner, indent);
            if lambda {
                let arg_str = if args.is_empty() {
                    "".to_string()
                } else {
                    format!(
                        "{new_indent}{}{indent}",
                        args.join(&(",".to_string() + &new_indent)),
                        indent = indent,
                        new_indent = new_indent
                    )
                };
                let out = format!("[&]({}) {}", arg_str, body);
                if call {
                    format!("({})()", out)
                } else {
                    out
                }
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
type State = Table;
type Out = (String, HashSet<String>);

impl CodeGenerator {
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

impl Visitor<State, Code, Out, Path> for CodeGenerator {
    fn visit_root(&mut self, storage: &mut DBStorage, module: &Path) -> Result<Out, TError> {
        let root = storage.look_up_definitions(module.clone())?;
        let mut main_info = root.ast.get_info();
        let mut main_at = module.clone();
        main_at.push(Symbol::new("main"));
        main_info.defined_at = Some(main_at);
        let main_let = Let {
            info: main_info,
            name: "main".to_string(),
            value: Box::new(root.ast.clone()),
            args: Some(vec![]),
        };
        let mut table = root.table; // TODO: Shouldn't be mut
        debug!("table {:?}", table);
        let main = match self.visit_let(storage, &mut table, &main_let)? {
            Code::Assignment(_, val) => match *val {
                Code::Func {
                    name: _,
                    args: _,
                    body,
                    lambda: _,
                    call: _,
                    return_type: _,
                } => Code::Func {
                    name: "main".to_string(),
                    args: vec!["int argc".to_string(), "char* argv[]".to_string()],
                    body,
                    lambda: false,
                    call: false,
                    return_type: "int".to_string(),
                },
                thing => panic!("main must be a Func {:?}", thing),
            },
            thing => panic!("main must be an Func {:?}", thing),
        };
        // TODO(cypher1): Use a writer.
        let mut code = "".to_string();

        // #includes
        let mut includes: Vec<&String> = self.includes.iter().collect();
        includes.sort();
        for inc in &includes {
            if inc.as_str() != "" {
                code = format!("{}{}\n", code, inc);
            }
        }
        // Forward declarations
        for func in &self.functions.clone() {
            match &func {
                Code::Func { name, args, .. } => {
                    code = format!("{}{}({});\n", code, name, args.join(", "),);
                }
                _ => panic!("Cannot create function from non-function"),
            }
        }

        self.functions.push(main);

        // Definitions
        for func in self.functions.iter().clone() {
            let function = pretty_print_block(func.clone(), "\n");
            code = format!("{}{}", code, function);
        }
        Ok((code + "\n", self.flags.clone()))
    }

    fn visit_sym(&mut self, storage: &mut DBStorage, _state: &mut State, expr: &Sym) -> Res {
        debug!(
            "to_c: visit {}, {:?}",
            expr.name,
            expr.get_info().defined_at
        );
        let name = make_name(
            &expr
                .get_info()
                .defined_at
                .expect("Could not find definition for symbol"),
        );
        if let Some(info) = storage.get_extern(&name) {
            self.includes.insert(info.cpp.includes.clone());
            self.flags.extend(info.cpp.flags.clone());
            // arg_processor
            return Ok(Code::Expr(info.cpp.code.clone()));
        }
        Ok(Code::Expr(name))
    }

    fn visit_val(&mut self, storage: &mut DBStorage, state: &mut State, expr: &Val) -> Res {
        use Val::{Lambda, PrimVal, Product, Struct, Union};
        match expr {
            Product(tys) => {
                if tys.is_empty() {
                    return Ok(Code::Expr("void".to_string()));
                }
                unimplemented!("unimplemented sum type in compilation to cpp")
            }
            Union(tys) => {
                if tys.is_empty() {
                    return Ok(Code::Expr("nullptr".to_string()));
                }
                unimplemented!("unimplemented sum type in compilation to cpp")
            }
            PrimVal(prim) => {
                use Prim::{Bool, BuiltIn, Str, Tag, I32};
                match prim {
                    I32(n) => Ok(Code::Expr(n.to_string())),
                    Bool(true) => Ok(Code::Expr(1.to_string())),
                    Bool(false) => Ok(Code::Expr(0.to_string())),
                    Str(s) => Ok(Code::Expr(format!("{:?}", s))),
                    BuiltIn(name) => {
                        unimplemented!("unimplemented BuiltIn to {} in compilation to cpp", &name)
                    }
                    Tag(bits) => {
                        unimplemented!("unimplemented Tag {:?} in compilation to cpp", &bits)
                    }
                }
            }
            Lambda(node) => self.visit(storage, state, node),
            Struct(vals) => {
                // TODO: Struct C++
                let mut val_code = vec![];
                for val in vals.iter() {
                    val_code.push(self.visit_val(storage, state, &val.1)?);
                }
                Ok(Code::Struct(val_code))
            }
            _ty => unimplemented!("unimplemented primitive type in compilation to cpp"),
        }
    }

    fn visit_apply(&mut self, storage: &mut DBStorage, state: &mut State, expr: &Apply) -> Res {
        debug!("apply here: {:?}", expr);
        // Build the 'struct' of args
        let mut args = vec![];
        for arg in &expr.args {
            // TODO: Include lambda head in values
            let val = self.visit_let(storage, state, arg)?;
            match val {
                Code::Assignment(_, val) => args.push(pretty_print_block(*val, "")),
                val => args.push(pretty_print_block(val, "")),
            };
        }
        let inner = self.visit(storage, state, &expr.inner)?;
        match inner {
            Code::Expr(expr) => {
                let with_args = format!("{}({})", expr, args.join(", "));
                Ok(Code::Expr(with_args))
            }
            _ => panic!("Don't know how to apply arguments to a block"),
        }
    }

    fn visit_abs(&mut self, storage: &mut DBStorage, state: &mut State, expr: &Abs) -> Res {
        debug!(
            "abs here: {:?}, {:?}",
            expr.get_info().defined_at,
            expr.name
        );
        let path = expr
            .get_info()
            .defined_at
            .expect("Could not find definition for abs");

        let name = make_name(&path);
        let body = self.visit(storage, state, &expr.value)?;
        Ok(Code::Template(name, Box::new(body)))
    }

    fn visit_let(&mut self, storage: &mut DBStorage, state: &mut State, expr: &Let) -> Res {
        debug!(
            "let here: {:?}, {:?}",
            expr.get_info().defined_at,
            expr.name
        );
        let path = expr
            .get_info()
            .defined_at
            .expect("Could not find definition for let");

        let uses = storage.find_symbol_uses(path.clone())?;
        if uses.is_empty() {
            return Ok(Code::Empty);
        }
        let name = make_name(&path);
        let body = self.visit(storage, state, &expr.value)?;
        if let Some(e_args) = &expr.args {
            let mut args = vec![];
            for arg in e_args.iter() {
                let path = arg
                    .get_info()
                    .defined_at
                    .expect("Could not find definition for let arg");
                let name = make_name(&path);
                args.push(format!("const auto {}", name));
            }

            return Ok(Code::Assignment(
                name.clone(),
                Box::new(Code::Func {
                    name,
                    args,
                    return_type: "int".to_string(), // TODO
                    body: Box::new(body),
                    lambda: true,
                    call: false,
                }),
            ));
        }
        let body = match body {
            Code::Statement(s) => Code::Statement(s),
            Code::Expr(s) => Code::Expr(s),
            body => Code::Func {
                name: "".to_string(),
                args: vec![],
                return_type: "int".to_string(), // TODO
                body: Box::new(body),
                lambda: true,
                call: true,
            },
        };
        Ok(Code::Assignment(name, Box::new(body)))
    }

    fn visit_un_op(&mut self, storage: &mut DBStorage, state: &mut State, expr: &UnOp) -> Res {
        let code = self.visit(storage, state, &expr.inner)?;
        let info = expr.get_info();
        let op = &expr.name;
        if let Some(info) = storage.get_extern(op) {
            self.includes.insert(info.cpp.includes.clone());
            self.flags.extend(info.cpp.flags.clone());
            let code = if info.cpp.arg_processor.as_str() == "" {
                code
            } else {
                self.build_call1(info.cpp.arg_processor.as_str(), code)
            };
            return Ok(self.build_call1(info.cpp.arg_joiner.as_str(), code));
        }
        Err(TError::UnknownPrefixOperator(op.to_string(), info))
    }

    fn visit_bin_op(&mut self, storage: &mut DBStorage, state: &mut State, expr: &BinOp) -> Res {
        let info = expr.get_info();
        let left = self.visit(storage, state, &expr.left.clone())?;
        let right = self.visit(storage, state, &expr.right.clone())?;
        // TODO: require 2 children
        // TODO: Short circuiting of dependencies.
        let op = expr.name.as_str();
        match op {
            "-|" => {
                // TODO: handle 'error' values more widely.
                let done = Code::If {
                    condition: Box::new(left),
                    then: Box::new(right),
                    then_else: Box::new(Code::Statement("throw 101".to_string())),
                };
                return Ok(done);
            }
            "," | ";" => {
                // TODO: handle 'error' values more widely.
                return Ok(left.merge(right));
            }
            _ => {}
        }
        if let Some(info) = storage.get_extern(op) {
            self.includes.insert(info.cpp.includes.clone());
            self.flags.extend(info.cpp.flags.clone());
            let (left, right) = if info.cpp.arg_processor.as_str() == "" {
                (left, right)
            } else {
                (
                    self.build_call1(info.cpp.arg_processor.as_str(), left),
                    self.build_call1(info.cpp.arg_processor.as_str(), right),
                )
            };
            return Ok(self.build_call2(
                info.cpp.code.as_str(),
                info.cpp.arg_joiner.as_str(),
                left,
                right,
            ));
        }
        Err(TError::UnknownInfixOperator(op.to_string(), info))
    }
}
