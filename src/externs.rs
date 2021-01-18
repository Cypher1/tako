use std::collections::HashMap;

use crate::ast::{Node, Info, ToNode};
use crate::database::Compiler;
use crate::errors::TError;
use crate::interpreter::{prim_add_strs, prim_pow, Res};
use crate::primitives::{
    bit_type, i32_type, number_type, string_type, type_type, unit_type, variable, void_type, Prim::* };

pub type FuncImpl = Box<dyn Fn(&dyn Compiler, HashMap<String, Box<dyn Fn() -> Res>>, Info) -> Res>;

pub fn get_implementation(name: String) -> Option<FuncImpl> {
    match name.as_str() {
        "print" => Some(Box::new(|_, args, _info| {
            let val = args.get("it").unwrap()()?;
            match val {
                Str(s) => print!("{}", s),
                s => print!("{:?}", s),
            };
            Ok(I32(0))
        })),
        "eprint" => Some(Box::new(|_, args, _info| {
            let val = args.get("it").unwrap()()?;
            match val {
                Str(s) => eprint!("{}", s),
                s => eprint!("{:?}", s),
            };
            Ok(I32(0))
        })),
        "." => Some(Box::new(|_db, args, info| {
            let left = args.get("left").unwrap()()?;
            let right = args.get("right").unwrap()()?;
            use crate::ast::{Apply, Let};
            Ok(Lambda(Box::new(
                Apply {
                    inner: Box::new(right.to_node()),
                    args: vec![Let {
                        name: "it".to_string(),
                        value: Box::new(left.to_node()),
                        args: None,
                        info: info.clone(),
                    }],
                    info,
                }
                .to_node(),
            )))
        })),
        "exit" => Some(Box::new(|_, args, _| {
            let val = args.get("it").unwrap()()?;
            let code = match val {
                I32(n) => n,
                s => {
                    eprint!("{:?}", s);
                    1
                }
            };
            std::process::exit(code);
        })),
        "++" => Some(Box::new(|_, args, info| {
            prim_add_strs(
                &args.get("left").unwrap()()?,
                &args.get("right").unwrap()()?,
                info,
            )
        })),
        "^" => Some(Box::new(|_, args, info| {
            prim_pow(
                &args.get("left").unwrap()()?,
                &args.get("right").unwrap()()?,
                info,
            )
        })),

        "argc" => Some(Box::new(|db, _, _info| {
            Ok(I32(db.options().interpreter_args.len() as i32))
        })),
        "argv" => Some(Box::new(|db, args, info| {
            match args.get("it").unwrap()()? {
                I32(ind) => Ok(Str(db.options().interpreter_args[ind as usize].clone())),
                value => Err(TError::TypeMismatch(
                    "Expected index to be of type i32".to_string(),
                    Box::new(value),
                    info,
                )),
            }
        })),
        "I32" => Some(Box::new(|_db, _, _info| Ok(i32_type()))),
        "Number" => Some(Box::new(|_db, _, _info| Ok(number_type()))),
        "String" => Some(Box::new(|_db, _, _info| Ok(string_type()))),
        "Bit" => Some(Box::new(|_db, _, _info| Ok(bit_type()))),
        "Unit" => Some(Box::new(|_db, _, _info| Ok(unit_type()))),
        "Void" => Some(Box::new(|_db, _, _info| Ok(void_type()))),
        "Type" => Some(Box::new(|_db, _, _info| Ok(type_type()))),
        _ => None,
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Direction {
    Left,
    Right,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Semantic {
    Operator { binding: i32, assoc: Direction },
    Func,
}

fn operator(binding: i32, assoc: Direction) -> Semantic {
    Semantic::Operator { binding, assoc }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Extern {
    pub name: String,
    pub semantic: Semantic,
    pub ty: Node,
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

pub fn get_externs(_db: &dyn Compiler) -> Result<HashMap<String, Extern>, TError> {
    use Direction::*;
    use Semantic::Func;
    let mut externs = vec![
        Extern {
            name: "argc".to_string(),
            semantic: Func,
            ty: i32_type().to_node(),
            cpp: LangImpl::new("argc"),
        },
        Extern {
            name: "argv".to_string(),
            semantic: Func,
            ty: Function {
                results: Box::new(string_type()),
                intros: dict!(),
                arguments: Box::new(rec!("it" => i32_type())),
            }.to_node(),
            cpp: LangImpl::new("([&argv](const int x){return argv[x];})"),
        },
        Extern {
            name: "eprint".to_string(),
            semantic: Func,
            ty: Function {
                results: Box::new(WithEffect(
                    Box::new(unit_type()),
                    vec!["stderr".to_string()],
                )),
                arguments: Box::new(rec! {"it" => string_type()}),
                intros: dict!(),
            }.to_node(),
            cpp: LangImpl::new("std::cerr << ").with_includes("#include <iostream>"),
        },
        Extern {
            name: "exit".to_string(),
            semantic: Func,
            ty: Function {
                results: Box::new(void_type()),
                arguments: Box::new(rec! {"it" => i32_type()}),
                intros: dict!(),
            }.to_node(),
            cpp: LangImpl::new("[](const int code){exit(code);}")
                .with_includes("#include <stdlib.h>"),
        },
        Extern {
            name: "print".to_string(),
            semantic: Func,
            ty: Function {
                results: Box::new(WithEffect(
                    Box::new(unit_type()),
                    vec!["stdout".to_string()],
                )),
                arguments: Box::new(rec! {"it" => string_type()}),
                intros: dict!(),
            }.to_node(),
            cpp: LangImpl::new("std::cout << ").with_includes("#include <iostream>"),
        },
        Extern {
            name: "pointer".to_string(),
            semantic: Func,
            ty: Function {
                results: Box::new(variable("a")),
                arguments: Box::new(rec! {"it" => variable("Type")}),
                intros: dict!("a" => variable("Type")),
            }.to_node(),
            cpp: LangImpl::new("std::cout << ").with_includes("#include <iostream>"),
        },
        Extern {
            name: "struct".to_string(),
            semantic: Func,
            ty: Function {
                results: Box::new(variable("a")),
                arguments: Box::new(variable("a")),
                intros: dict!("a" => variable("Type")),
            }.to_node(),
            cpp: LangImpl::new("[](const int code){exit(code);}")
                .with_includes("#include <stdlib.h>"),
        },
        Extern {
            name: ";".to_string(),
            semantic: operator(20, Left),
            ty: Function {
                intros: dict!("a" => variable("Type"), "b" => variable("Type")),
                results: Box::new(variable("b")),
                arguments: Box::new(rec!("left" => variable("a"), "right" => variable("b"))),
            }.to_node(),
            cpp: LangImpl::operator(";"),
        },
        Extern {
            name: ",".to_string(),
            semantic: operator(30, Left),
            ty: Function {
                intros: dict!("a" => variable("Type"), "b" => variable("Type")),
                results: Box::new(Product(set!(variable("a"), variable("b")))),
                arguments: Box::new(rec!("left" => variable("a"), "right" => variable("b"))),
            }.to_node(),
            cpp: LangImpl::operator(", "),
        },
        Extern {
            name: "=".to_string(),
            semantic: operator(40, Right),
            ty: Function {
                intros: dict!("a" => variable("Identifier"), "b" => variable("Type")),
                results: Box::new(variable("b")),
                arguments: Box::new(rec!("left" => variable("a"), "right" => variable("b"))),
            }.to_node(),
            cpp: LangImpl::operator(" = "),
        },
        Extern {
            name: "?".to_string(),
            semantic: operator(45, Left),
            ty: Function {
                intros: dict!("a" => variable("Type"), "b" => variable("Type")),
                results: Box::new(Union(set!(variable("a"), variable("b")))),
                arguments: Box::new(rec!("left" => variable("a"), "right" => variable("b"))),
            }.to_node(),
            cpp: LangImpl::operator("?"),
        },
        Extern {
            name: "|-".to_string(),
            semantic: operator(46, Right),
            ty: Function {
                intros: dict!("a" => variable("Type")),
                results: Box::new(variable("a")),
                arguments: Box::new(rec!("left" => variable("Type"), "right" => variable("a"))),
            }.to_node(),
            cpp: LangImpl::operator("|-"),
        },
        Extern {
            name: "-|".to_string(),
            semantic: operator(47, Left),
            ty: Function {
                intros: dict!("a" => variable("Type")),
                results: Box::new(variable("a")),
                arguments: Box::new(rec!("left" => variable("Type"), "right" => variable("a"))),
            }.to_node(),
            cpp: LangImpl::operator("-|"),
        },
        Extern {
            name: "|".to_string(),
            semantic: operator(48, Left),
            ty: Function {
                intros: dict!("a" => variable("Type"), "b" => variable("Type")),
                results: Box::new(Union(set!(variable("a"), variable("b")))),
                arguments: Box::new(rec!("left" => variable("a"), "right" => variable("b"))),
            }.to_node(),
            cpp: LangImpl::operator("|"),
        },
        Extern {
            name: "&".to_string(),
            semantic: operator(48, Left),
            ty: Function {
                intros: dict!("a" => variable("Type"), "b" => variable("Type")),
                results: Box::new(Product(set!(variable("a"), variable("b")))),
                arguments: Box::new(rec!("left" => variable("a"), "right" => variable("b"))),
            }.to_node(),
            cpp: LangImpl::operator("&"),
        },
        Extern {
            name: "++".to_string(),
            semantic: operator(49, Left),
            ty: Function {
                intros: dict!("a" => variable("Display"), "b" => variable("Display")),
                results: Box::new(string_type()),
                arguments: Box::new(rec!("left" => variable("a"), "right" => variable("b"))),
            }.to_node(),
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
            name: "->".to_string(),
            semantic: operator(50, Left),
            ty: Function {
                intros: dict!("a" => variable("Type"), "b" => variable("Type")),
                results: Box::new(Function {
                    intros: dict!(),
                    results: Box::new(variable("b")),
                    arguments: Box::new(variable("a")),
                }),
                arguments: Box::new(rec!("left" => variable("a"), "right" => variable("b"))),
            }.to_node(),
            cpp: LangImpl::operator("???"),
        },
        Extern {
            name: "<".to_string(),
            semantic: operator(51, Left),
            ty: Function {
                intros: dict!("a" => variable("Number"), "b" => variable("Number")),
                results: Box::new(bit_type()),
                arguments: Box::new(rec!("left" => variable("a"), "right" => variable("b"))),
            }.to_node(),
            cpp: LangImpl::operator("<"),
        },
        Extern {
            name: "<=".to_string(),
            semantic: operator(51, Left),
            ty: Function {
                intros: dict!("a" => variable("Number"), "b" => variable("Number")),
                results: Box::new(bit_type()),
                arguments: Box::new(rec!("left" => variable("a"), "right" => variable("b"))),
            }.to_node(),
            cpp: LangImpl::operator("<="),
        },
        Extern {
            name: ">".to_string(),
            semantic: operator(51, Left),
            ty: Function {
                intros: dict!("a" => variable("Number"), "b" => variable("Number")),
                results: Box::new(bit_type()),
                arguments: Box::new(rec!("left" => variable("a"), "right" => variable("b"))),
            }.to_node(),
            cpp: LangImpl::operator(">"),
        },
        Extern {
            name: ">=".to_string(),
            semantic: operator(51, Left),
            ty: Function {
                intros: dict!("a" => variable("Number"), "b" => variable("Number")),
                results: Box::new(bit_type()),
                arguments: Box::new(rec!("left" => variable("a"), "right" => variable("b"))),
            }.to_node(),
            cpp: LangImpl::operator(">="),
        },
        Extern {
            name: "!=".to_string(),
            semantic: operator(51, Left),
            ty: Function {
                intros: dict!("a" => variable("Type"), "b" => variable("Type")),
                results: Box::new(bit_type()),
                arguments: Box::new(rec!("left" => variable("a"), "right" => variable("b"))),
            }.to_node(),
            cpp: LangImpl::operator("!="),
        },
        Extern {
            name: "==".to_string(),
            semantic: operator(51, Left),
            ty: Function {
                intros: dict!("a" => variable("Type"), "b" => variable("Type")),
                results: Box::new(bit_type()),
                arguments: Box::new(rec!("left" => variable("a"), "right" => variable("b"))),
            }.to_node(),
            cpp: LangImpl::operator("=="),
        },
        Extern {
            name: "||".to_string(),
            semantic: operator(60, Left),
            ty: Function {
                intros: dict!(),
                results: Box::new(bit_type()),
                arguments: Box::new(rec!("left" => bit_type(), "right" => bit_type())),
            }.to_node(),
            cpp: LangImpl::operator("||"),
        },
        Extern {
            name: "&&".to_string(),
            semantic: operator(60, Left),
            ty: Function {
                intros: dict!(),
                results: Box::new(bit_type()),
                arguments: Box::new(rec!("left" => bit_type(), "right" => bit_type())),
            }.to_node(),
            cpp: LangImpl::operator("&&"),
        },
        Extern {
            name: "!".to_string(),
            semantic: operator(70, Left),
            ty: Function {
                intros: dict!(),
                results: Box::new(bit_type()),
                arguments: Box::new(bit_type()),
            }.to_node(),
            cpp: LangImpl::operator("!"),
        },
        Extern {
            name: "...".to_string(),
            semantic: operator(70, Left),
            ty: Function {
                intros: dict!("a" => variable("Type")), // TODO: This should unpack a type with a set of named values and put them into scope.
                results: Box::new(variable("a")),
                arguments: Box::new(rec!("it" => variable("a"))),
            }.to_node(),
            cpp: LangImpl::operator("..."), // TODO: Implement
        },
        Extern {
            name: "-".to_string(),
            semantic: operator(70, Left),
            ty: Function {
                intros: dict!("a" => variable("Number")),
                results: Box::new(variable("a")),
                arguments: Box::new(rec!("it" => variable("a"))),
            }.to_node(),
            cpp: LangImpl::operator("-"),
        },
        Extern {
            name: "+".to_string(),
            semantic: operator(70, Left),
            ty: Function {
                intros: dict!("a" => variable("Number"), "b" => variable("Number")),
                results: Box::new(variable("a")),
                arguments: Box::new(rec!("left" => variable("a"), "right" => variable("b"))),
            }.to_node(),
            cpp: LangImpl::operator("+"),
        },
        Extern {
            name: "*".to_string(),
            semantic: operator(80, Left),
            ty: Function {
                intros: dict!("a" => variable("Number"), "b" => variable("Number")),
                results: Box::new(variable("a")),
                arguments: Box::new(rec!("left" => variable("a"), "right" => variable("b"))),
            }.to_node(),
            cpp: LangImpl::operator("*"),
        },
        Extern {
            name: "%".to_string(),
            semantic: operator(80, Left),
            ty: Function {
                intros: dict!("a" => variable("Number"), "b" => variable("Number")),
                results: Box::new(variable("a")),
                arguments: Box::new(rec!("left" => variable("a"), "right" => variable("b"))),
            }.to_node(),
            cpp: LangImpl::operator("%"),
        },
        Extern {
            name: "/".to_string(),
            semantic: operator(80, Left),
            ty: Function {
                intros: dict!("a" => variable("Number"), "b" => variable("Number")),
                results: Box::new(variable("a")),
                arguments: Box::new(rec!("left" => variable("a"), "right" => variable("b"))),
            }.to_node(),
            cpp: LangImpl::operator("/"),
        },
        Extern {
            name: "^".to_string(),
            semantic: operator(90, Right),
            ty: Function {
                intros: dict!("a" => variable("Number"), "b" => variable("Number")),
                results: Box::new(variable("a")),
                arguments: Box::new(rec!("left" => variable("a"), "right" => variable("b"))),
            }.to_node(),
            cpp: LangImpl::new("pow")
                .with_includes("#include <cmath>")
                .with_arg_joiner(", ")
                .with_flag("-lm"),
        },
        Extern {
            name: ".".to_string(),
            semantic: operator(100, Right),
            ty: Function {
                intros: dict!("a" => variable("Type"), "b" => variable("Type"), "c" => variable("Type")),
                results: Box::new(variable("c")),
                arguments: Box::new(
                    rec!("left" => variable("a"), "right" => Function{intros: dict!(), arguments: Box::new(rec!("it" => variable("a"))), results: Box::new(variable("c"))}),
                ),
            }.to_node(),
            cpp: LangImpl::new("[](const auto l, const auto r){return r(l);}"),
        },
        Extern {
            name: "I32".to_string(),
            semantic: Func,
            ty: variable("Type").to_node(),
            cpp: LangImpl::new("int32_t"),
        },
        Extern {
            name: "Number".to_string(),
            semantic: Func,
            ty: variable("Type").to_node(),
            cpp: LangImpl::new("usize"),
        },
        Extern {
            name: "String".to_string(),
            semantic: Func,
            ty: variable("Type").to_node(),
            cpp: LangImpl::new("std::string").with_includes("#include <string>"),
        },
        Extern {
            name: "Bit".to_string(),
            semantic: Func,
            ty: variable("Type").to_node(),
            cpp: LangImpl::new("short"),
        },
        Extern {
            name: "Unit".to_string(),
            semantic: Func,
            ty: variable("Type").to_node(),
            cpp: LangImpl::new("void"),
        },
        Extern {
            name: "Void".to_string(),
            semantic: Func,
            ty: variable("Void").to_node(),
            cpp: LangImpl::new("/*void: should never happen*/ auto"),
        },
        Extern {
            name: "Type".to_string(),
            semantic: Func,
            ty: variable("Type").to_node(),
            cpp: LangImpl::new("auto"),
        },
    ];
    let mut extern_map: HashMap<String, Extern> = HashMap::new();
    while let Some(extern_def) = externs.pop() {
        extern_map.insert(extern_def.name.clone(), extern_def);
    }
    Ok(extern_map)
}
