use std::collections::HashMap;

use crate::ast::{Info, Prim::*};
use crate::database::Compiler;
use crate::errors::TError;
use crate::interpreter::{prim_add_strs, prim_pow, Res};
use crate::types::{
    bit_type, i32_type, number_type, string_type, type_type, unit_type, variable, void_type, Type, Type::*,
};

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
                s => {
                    eprint!("{:?}", s);
                    1
                }
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
        "argv" => Some(Box::new(|db, args, info| match args[0]()? {
            I32(ind, _) => Ok(Str(
                db.options().interpreter_args[ind as usize].clone(),
                info,
            )),
            value => Err(TError::TypeMismatch(
                "Expected index to be of type i32".to_string(),
                Box::new(value),
                info,
            )),
        })),
        "I32" => Some(Box::new(|_db, _, info| Ok(TypeValue(i32_type(), info)))),
        "Number" => Some(Box::new(|_db, _, info| Ok(TypeValue(number_type(), info)))),
        "String" => Some(Box::new(|_db, _, info| Ok(TypeValue(string_type(), info)))),
        "Bit" => Some(Box::new(|_db, _, info| Ok(TypeValue(bit_type(), info)))),
        "Unit" => Some(Box::new(|_db, _, info| Ok(TypeValue(unit_type(), info)))),
        "Void" => Some(Box::new(|_db, _, info| Ok(TypeValue(void_type(), info)))),
        "Type" => Some(Box::new(|_db, _, info| Ok(TypeValue(type_type(), info)))),
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

    fn operator(arg_joiner: &str) -> LangImpl {
        LangImpl {
            code: "".to_string(),
            arg_joiner: arg_joiner.to_string(),
            arg_processor: "".to_string(),
            includes: "".to_string(),
            flags: vec![],
        }
    }

    fn with_arg_joiner(mut self, arg_joiner: &str) -> LangImpl {
        self.arg_joiner = arg_joiner.to_string();
        self
    }

    fn with_arg_processor(mut self, arg_processor: &str) -> LangImpl {
        self.arg_processor = arg_processor.to_string();
        self
    }

    fn with_includes(mut self, includes: &str) -> LangImpl {
        self.includes = includes.to_string();
        self
    }

    fn with_flag(mut self, flag: &str) -> LangImpl {
        self.flags.push(flag.to_string());
        self
    }
}

pub fn get_externs(db: &dyn Compiler) -> Result<HashMap<String, Extern>, TError> {
    let module = vec![];

    let number_constraint = db.parse_str(module.clone(), "Number")?;
    let string_constraint = db.parse_str(module.clone(), "String")?;
    let unit_type = db.parse_str(module, "product()")?;

    eprintln!("num {}", number_constraint);
    eprintln!("str {}", string_constraint);
    eprintln!("unit {}", unit_type);

    let mut externs = vec![
        Extern {
            name: "argc".to_string(),
            operator: None,
            ty: i32_type(),
            cpp: LangImpl::new("argc"),
        },
        Extern {
            name: "argv".to_string(),
            operator: None,
            ty: Function {
                results: dict!("it" => string_type()),
                intros: dict!(),
                arguments: dict!("it" => i32_type()),
                effects: vec![],
            },
            cpp: LangImpl::new("([&argv](const int x){return argv[x];})"),
        },
        Extern {
            name: "eprint".to_string(),
            operator: None,
            ty: Function {
                results: dict! {},
                arguments: dict! {"it" => string_type()},
                intros: dict!(),
                effects: vec!["stderr".to_string()],
            },
            cpp: LangImpl::new("std::cerr << ").with_includes("#include <iostream>"),
        },
        Extern {
            name: "exit".to_string(),
            operator: None,
            ty: Function {
                results: dict! {"it" => void_type()},
                arguments: dict! {"it" => i32_type()},
                intros: dict!(),
                effects: vec!["stderr".to_string()],
            },
            cpp: LangImpl::new("[](const int code){exit(code);}")
                .with_includes("#include <stdlib.h>"),
        },
        Extern {
            name: "print".to_string(),
            operator: None,
            ty: Function {
                results: dict! {},
                arguments: dict! {"it" => string_type()},
                intros: dict!(),
                effects: vec!["stdout".to_string()],
            },
            cpp: LangImpl::new("std::cout << ").with_includes("#include <iostream>"),
        },
        Extern {
            name: "pointer".to_string(),
            operator: None,
            ty: Function {
                results: dict! {"it" => variable("a")},
                arguments: dict! {"it" => variable("Type")},
                intros: dict!("a" => variable("Type")),
                effects: vec![],
            },
            cpp: LangImpl::new("std::cout << ").with_includes("#include <iostream>"),
        },
        Extern {
            name: ";".to_string(),
            operator: Some((20, false)),
            ty: Function {
                intros: dict!("a" => variable("Type"), "b" => variable("Type")),
                results: dict!("it" => variable("b")),
                arguments: dict!("left" => variable("a"), "right" => variable("b")),
                effects: vec![],
            },
            cpp: LangImpl::operator(";"),
        },
        Extern {
            name: ",".to_string(),
            operator: Some((30, false)),
            ty: Function {
                intros: dict!("a" => variable("Type"), "b" => variable("Type"), "c" => variable("Type")),
                results: dict!("it" => variable("c")),
                arguments: dict!("left" => variable("a"), "right" => variable("b")),
                effects: vec![],
            },
            cpp: LangImpl::operator(", "),
        },
        Extern {
            name: "=".to_string(),
            operator: Some((40, true)),
            ty: Function {
                intros: dict!("a" => variable("Identifier"), "b" => variable("Type")),
                results: dict!("it" => variable("b")),
                arguments: dict!("left" => variable("a"), "right" => variable("b")),
                effects: vec![],
            },
            cpp: LangImpl::operator(" = "),
        },
        Extern {
            name: ":".to_string(),
            operator: Some((42, false)),
            ty: Function {
                intros: dict!("a" => variable("Type")),
                results: dict!("it" => variable("a")),
                arguments: dict!("left" => variable("a"), "right" => variable("Type")),
                effects: vec![],
            },
            cpp: LangImpl::operator(":"),
        },
        Extern {
            name: "?".to_string(),
            operator: Some((45, false)),
            ty: Function {
                intros: dict!("a" => variable("Type"), "b" => variable("Type")),
                results: dict!("it" => Union(set!(
                            variable("a"),
                            variable("b")
                    ))
                ),
                arguments: dict!("left" => variable("a"), "right" => variable("b")),
                effects: vec![],
            },
            cpp: LangImpl::operator("?"),
        },
        Extern {
            name: "-|".to_string(),
            operator: Some((47, false)),
            ty: Function {
                intros: dict!("a" => variable("Type")),
                results: dict!("it" => variable("a")),
                arguments: dict!("left" => variable("Type"), "right" => variable("a")),
                effects: vec![],
            },
            cpp: LangImpl::operator("-|"),
        },
        Extern {
            name: "|".to_string(),
            operator: Some((48, false)),
            ty: Function {
                intros: dict!("a" => variable("Type"), "b" => variable("Type")),
                results: dict!("it" => Union(set!(variable("a"), variable("b")))),
                arguments: dict!("left" => variable("a"), "right" => variable("b")),
                effects: vec![],
            },
            cpp: LangImpl::operator("|"),
        },
        Extern {
            name: "&".to_string(),
            operator: Some((48, false)),
            ty: Function {
                intros: dict!("a" => variable("Type"), "b" => variable("Type")),
                results: dict!("it" => Product(set!(variable("a"), variable("b")))),
                arguments: dict!("left" => variable("a"), "right" => variable("b")),
                effects: vec![],
            },
            cpp: LangImpl::operator("&"),
        },
        Extern {
            name: "++".to_string(),
            operator: Some((49, false)),
            ty: Function {
                intros: dict!("a" => variable("Display"), "b" => variable("Display")),
                results: dict!("it" => string_type()),
                arguments: dict!("left" => variable("a"), "right" => variable("b")),
                effects: vec![],
            },
            cpp: LangImpl::operator("+")
                .with_arg_processor("std::to_string")
                .with_includes(
                    "#include <string>
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
}",
                ),
        },
        Extern {
            name: "<".to_string(),
            operator: Some((50, false)),
            ty: Function {
                intros: dict!("a" => variable("Number"), "b" => variable("Number")),
                results: dict!("it" => bit_type()),
                arguments: dict!("left" => variable("a"), "right" => variable("b")),
                effects: vec![],
            },
            cpp: LangImpl::operator("<"),
        },
        Extern {
            name: "<=".to_string(),
            operator: Some((50, false)),
            ty: Function {
                intros: dict!("a" => variable("Number"), "b" => variable("Number")),
                results: dict!("it" => bit_type()),
                arguments: dict!("left" => variable("a"), "right" => variable("b")),
                effects: vec![],
            },
            cpp: LangImpl::operator("<="),
        },
        Extern {
            name: ">".to_string(),
            operator: Some((50, false)),
            ty: Function {
                intros: dict!("a" => variable("Number"), "b" => variable("Number")),
                results: dict!("it" => bit_type()),
                arguments: dict!("left" => variable("a"), "right" => variable("b")),
                effects: vec![],
            },
            cpp: LangImpl::operator(">"),
        },
        Extern {
            name: ">=".to_string(),
            operator: Some((50, false)),
            ty: Function {
                intros: dict!("a" => variable("Number"), "b" => variable("Number")),
                results: dict!("it" => bit_type()),
                arguments: dict!("left" => variable("a"), "right" => variable("b")),
                effects: vec![],
            },
            cpp: LangImpl::operator(">="),
        },
        Extern {
            name: "!=".to_string(),
            operator: Some((50, false)),
            ty: Function {
                intros: dict!("a" => variable("Type"), "b" => variable("Type")),
                results: dict!("it" => bit_type()),
                arguments: dict!("left" => variable("a"), "right" => variable("b")),
                effects: vec![],
            },
            cpp: LangImpl::operator("!="),
        },
        Extern {
            name: "==".to_string(),
            operator: Some((50, false)),
            ty: Function {
                intros: dict!("a" => variable("Type"), "b" => variable("Type")),
                results: dict!("it" => bit_type()),
                arguments: dict!("left" => variable("a"), "right" => variable("b")),
                effects: vec![],
            },
            cpp: LangImpl::operator("=="),
        },
        Extern {
            name: "||".to_string(),
            operator: Some((60, false)),
            ty: Function {
                intros: dict!(),
                results: dict!("it" => bit_type()),
                arguments: dict!("left" => bit_type(), "right" => bit_type()),
                effects: vec![],
            },
            cpp: LangImpl::operator("||"),
        },
        Extern {
            name: "&&".to_string(),
            operator: Some((60, false)),
            ty: Function {
                intros: dict!(),
                results: dict!("it" => bit_type()),
                arguments: dict!("left" => bit_type(), "right" => bit_type()),
                effects: vec![],
            },
            cpp: LangImpl::operator("&&"),
        },
        Extern {
            name: "!".to_string(),
            operator: Some((70, false)),
            ty: Function {
                intros: dict!(),
                results: dict!("it" => bit_type()),
                arguments: dict!("it" => bit_type()),
                effects: vec![],
            },
            cpp: LangImpl::operator("!"),
        },
        Extern {
            name: "-".to_string(),
            operator: Some((70, false)),
            ty: Function {
                intros: dict!("a" => variable("Number")),
                results: dict!("it" => variable("a")),
                arguments: dict!("it" => variable("a")),
                effects: vec![],
            },
            cpp: LangImpl::operator("-"),
        },
        Extern {
            name: "+".to_string(),
            operator: Some((70, false)),
            ty: Function {
                intros: dict!("a" => variable("Number"), "b" => variable("Number")),
                results: dict!("it" => variable("a")),
                arguments: dict!("left" => variable("a"), "right" => variable("b")),
                effects: vec![],
            },
            cpp: LangImpl::operator("+"),
        },
        Extern {
            name: "*".to_string(),
            operator: Some((80, false)),
            ty: Function {
                intros: dict!("a" => variable("Number"), "b" => variable("Number")),
                results: dict!("it" => variable("a")),
                arguments: dict!("left" => variable("a"), "right" => variable("b")),
                effects: vec![],
            },
            cpp: LangImpl::operator("*"),
        },
        Extern {
            name: "%".to_string(),
            operator: Some((80, false)),
            ty: Function {
                intros: dict!("a" => variable("Number"), "b" => variable("Number")),
                results: dict!("it" => variable("a")),
                arguments: dict!("left" => variable("a"), "right" => variable("b")),
                effects: vec![],
            },
            cpp: LangImpl::operator("%"),
        },
        Extern {
            name: "/".to_string(),
            operator: Some((80, false)),
            ty: Function {
                intros: dict!("a" => variable("Number"), "b" => variable("Number")),
                results: dict!("it" => variable("a")),
                arguments: dict!("left" => variable("a"), "right" => variable("b")),
                effects: vec![],
            },
            cpp: LangImpl::operator("/"),
        },
        Extern {
            name: "^".to_string(),
            operator: Some((90, true)),
            ty: Function {
                intros: dict!("a" => variable("Number"), "b" => variable("Number")),
                results: dict!("it" => variable("a")),
                arguments: dict!("left" => variable("a"), "right" => variable("b")),
                effects: vec![],
            },
            cpp: LangImpl::new("pow")
                .with_includes("#include <cmath>")
                .with_arg_joiner(", ")
                .with_flag("-lm"),
        },
        Extern {
            name: "Number".to_string(),
            operator: None,
            ty: variable("Type"),
            cpp: LangImpl::new("usize"),
        },
        Extern {
            name: "String".to_string(),
            operator: None,
            ty: variable("Type"),
            cpp: LangImpl::new("std::string").with_includes("#include <string>"),
        },
        Extern {
            name: "bit_type()".to_string(),
            operator: None,
            ty: variable("Type"),
            cpp: LangImpl::new("short"),
        },
        Extern {
            name: "Unit".to_string(),
            operator: None,
            ty: variable("Type"),
            cpp: LangImpl::new("void"),
        },
        Extern {
            name: "Void".to_string(),
            operator: None,
            ty: variable("Void"),
            cpp: LangImpl::new("/*void: should never happen*/ auto"),
        },
        Extern {
            name: "Type".to_string(),
            operator: None,
            ty: variable("Type"),
            cpp: LangImpl::new("auto"),
        },
    ];
    let mut extern_map: HashMap<String, Extern> = HashMap::new();
    while let Some(extern_def) = externs.pop() {
        extern_map.insert(extern_def.name.clone(), extern_def);
    }
    Ok(extern_map)
}
