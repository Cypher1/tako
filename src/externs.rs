use derivative::Derivative;
use std::collections::HashMap;

use crate::ast::{Info, Prim::*};
use crate::database::Compiler;
use crate::interpreter::{prim_add_strs, prim_pow, Res};
use crate::types::{void, Type, Type::*, str_type, number_type};

use crate::dict;

pub type FuncImpl = Box<dyn Fn(&dyn Compiler, Vec<&dyn Fn() -> Res>, Info) -> Res>;

pub fn get_implementation(name: String) -> Option<FuncImpl> {
    match name.as_str() {
        "print" => Some(Box::new(|_, args, info| {
            let val = args[0]()?;
            match val {
                Str(s, _) => print!("{}", s),
                s => print!("{:?}", s),
            };
            Ok(I32(0, info))
        })),
        "eprint" => Some(Box::new(|_, args, info| {
            let val = args[0]()?;
            match val {
                Str(s, _) => eprint!("{}", s),
                s => eprint!("{:?}", s),
            };
            Ok(I32(0, info))
        })),
        "exit" => Some(Box::new(|_, args, _| {
            let val = args[0]()?;
            let code = match val {
                I32(n, _) => n,
                s => {eprint!("{:?}", s); 1},
            };
            std::process::exit(code);
        })),
        "++" => Some(Box::new(|_, args, info| {
            prim_add_strs(&args[0]()?, &args[1]()?, info)
        })),
        "^" => Some(Box::new(|_, args, info| {
            prim_pow(&args[0]()?, &args[1]()?, info)
        })),
        "argc" => Some(Box::new(|db, _, info| {
            Ok(I32(db.options().interpreter_args.len() as i32, info))
        })),
        "argv" => Some(Box::new(|db, args, info| {
            use crate::errors::TError;
            match args[0]()? {
                I32(ind, _) => Ok(Str(
                    db.options().interpreter_args[ind as usize].clone(),
                    info,
                )),
                value => Err(TError::TypeMismatch(
                    "Expected index to be of type i32".to_string(),
                    Box::new(value),
                    info,
                )),
            }
        })),
        _ => None,
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Extern {
    pub name: String,
    pub operator: Option<(i32, bool)>, // (binding power, is_right_assoc) if the extern is an operator
    pub ty: Type,
    pub cpp: LangImpl,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct LangImpl {
    pub code: String,
    pub arg_joiner: String,
    pub arg_processor: String,
    pub includes: String,
    pub flags: Vec<String>,
}

impl LangImpl {
    fn new(code: &str) -> LangImpl {
        LangImpl {
            code: code.to_string(),
            arg_joiner: "".to_string(),
            arg_processor: "".to_string(),
            includes: "".to_string(),
            flags: vec![],
        }
    }

    fn with_arg_joiner(mut self, arg_joiner: &str) -> LangImpl {
        self.arg_joiner = arg_joiner;
        self
    }

    fn with_arg_processor(mut self, arg_processor: &str) -> LangImpl {
        self.arg_processor = arg_processor;
        self
    }

    fn with_includes(mut self, includes: &str) -> LangImpl {
        self.includes = includes;
        self
    }

    fn with_flag(mut self, flag: &str) -> LangImpl {
        self.flags.push(flag.to_string());
        self
    }
}

pub fn get_externs() -> HashMap<String, Extern> {
    let mut externs = vec![
        Extern {
            name: "print".to_string(),
            operator: None,
            ty: Function {
                results: dict!{},
                arguments: dict!{"it" => str_type()},
                intros: dict!(),
                effects: vec!["stdout".to_string()],
            },
            cpp: LangImpl::new("std::cout < ")
                .with_includes("#include <iostream>")
        },
        Extern {
            name: "eprint".to_string(),
            operator: None,
            ty: Function {
                results: dict!{},
                arguments: dict!{"it" => str_type()},
                intros: dict!(),
                effects: vec!["stderr".to_string()],
            },
            cpp: LangImpl::new("std::cerr < ")
                .with_includes("#include <iostream>")
        },
        Extern {
            name: "exit".to_string(),
            operator: None,
            cpp_includes: "#include <stdlib.h>".to_string(),
            cpp_code: "[](const int code){exit(code);}".to_string(),
            cpp_arg_joiner: ", ".to_string(),
            cpp_arg_processor: "".to_string(),
            cpp_flags: vec![],
            ty: Function {
                results: dict!{"it" => void()},
                arguments: dict!{"it" => number_type()},
                intros: dict!(),
                effects: vec!["stderr".to_string()],
            },
        },
        Extern {
            name: "++".to_string(),
            operator: Some((48, false)),
            cpp_includes: "#include <string>
#include <sstream>
namespace std{
template <typename T>
string to_string(const T& t){
  stringstream out;
  out << t;
  return out.str();
}
string to_string(const bool& t){
  return t ? \"true\" : \"false\";
}
}"
            .to_string(),
            cpp_code: "".to_string(),
            cpp_arg_joiner: "+".to_string(),
            cpp_arg_processor: "std::to_string".to_string(),
            cpp_flags: vec![],
            ty: Function {
                intros: dict!("a" => Variable("Display".to_string()), "b" => Variable("Display".to_string())),
                results: dict!("it" => str_type()),
                arguments: dict!("left" => Variable("a".to_string()), "right" => Variable("b".to_string())),
                effects: vec![],
            },
        },
        Extern {
            name: "=".to_string(),
            operator: Some((40, true)),
            cpp_includes: "".to_string(),
            cpp_code: "".to_string(),
            cpp_arg_joiner: " = ".to_string(),
            cpp_arg_processor: "".to_string(),
            cpp_flags: vec![],
            ty: Function {
                intros: dict!("a" => Variable("Identifier".to_string()), "b" => Variable("Type".to_string())),
                results: dict!("it" => Variable("b".to_string())),
                arguments: dict!("left" => Variable("a".to_string()), "right" => Variable("b".to_string())),
                effects: vec![],
            },
        },
        Extern {
            name: "^".to_string(),
            operator: Some((90, true)),
            cpp_includes: "#include <cmath>".to_string(),
            cpp_code: "pow".to_string(),
            cpp_arg_joiner: ", ".to_string(),
            cpp_arg_processor: "".to_string(),
            cpp_flags: vec!["-lm".to_string()],
            ty: Function {
                intros: dict!("a" => Variable("Number".to_string()), "b" => Variable("Number".to_string())),
                results: dict!("it" => Variable("a".to_string())),
                arguments: dict!("left" => Variable("a".to_string()), "right" => Variable("b".to_string())),
                effects: vec![],
            },
        },
        Extern {
            name: "*".to_string(),
            operator: Some((80, false)),
            cpp_includes: "".to_string(),
            cpp_code: "".to_string(),
            cpp_arg_joiner: "*".to_string(),
            cpp_arg_processor: "".to_string(),
            cpp_flags: vec![],
            ty: Function {
                intros: dict!("a" => Variable("Number".to_string()), "b" => Variable("Number".to_string())),
                results: dict!("it" => Variable("a".to_string())),
                arguments: dict!("left" => Variable("a".to_string()), "right" => Variable("b".to_string())),
                effects: vec![],
            },
        },
        Extern {
            name: "%".to_string(),
            operator: Some((80, false)),
            cpp_includes: "".to_string(),
            cpp_code: "".to_string(),
            cpp_arg_joiner: "%".to_string(),
            cpp_arg_processor: "".to_string(),
            cpp_flags: vec![],
            ty: Function {
                intros: dict!("a" => Variable("Number".to_string()), "b" => Variable("Number".to_string())),
                results: dict!("it" => Variable("a".to_string())),
                arguments: dict!("left" => Variable("a".to_string()), "right" => Variable("b".to_string())),
                effects: vec![],
            },
        },
        Extern {
            name: "+".to_string(),
            operator: Some((70, false)),
            cpp_includes: "".to_string(),
            cpp_code: "".to_string(),
            cpp_arg_joiner: "+".to_string(),
            cpp_arg_processor: "".to_string(),
            cpp_flags: vec![],
            ty: Function {
                intros: dict!("a" => Variable("Number".to_string()), "b" => Variable("Number".to_string())),
                results: dict!("it" => Variable("a".to_string())),
                arguments: dict!("left" => Variable("a".to_string()), "right" => Variable("b".to_string())),
                effects: vec![],
            },
        },
        Extern {
            name: "/".to_string(),
            operator: Some((80, false)),
            cpp_includes: "".to_string(),
            cpp_code: "".to_string(),
            cpp_arg_joiner: "/".to_string(),
            cpp_arg_processor: "".to_string(),
            cpp_flags: vec![],
            ty: Function {
                intros: dict!("a" => Variable("Number".to_string()), "b" => Variable("Number".to_string())),
                results: dict!("it" => Variable("a".to_string())),
                arguments: dict!("left" => Variable("a".to_string()), "right" => Variable("b".to_string())),
                effects: vec![],
            },
        },
        Extern {
            name: "argc".to_string(),
            operator: None,
            cpp_includes: "".to_string(),
            cpp_code: "argc".to_string(),
            cpp_arg_joiner: ", ".to_string(),
            cpp_arg_processor: "".to_string(),
            cpp_flags: vec![],
            ty: number_type(),
        },
        Extern {
            name: "argv".to_string(),
            operator: None,
            cpp_includes: "".to_string(),
            cpp_code: "([&argv](const int x){return argv[x];})".to_string(),
            cpp_arg_joiner: ", ".to_string(),
            cpp_arg_processor: "".to_string(),
            cpp_flags: vec![],
            ty: Function {
                results: dict!("it" => str_type()),
                intros: dict!(),
                arguments: dict!("it" => number_type()),
                effects: vec![],
            },
        },
    ];
    let mut extern_map: HashMap<String, Extern> = HashMap::new();
    while let Some(extern_def) = externs.pop() {
        extern_map.insert(extern_def.name.clone(), extern_def);
    }
    extern_map
}
