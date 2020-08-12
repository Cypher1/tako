use std::collections::HashMap;
use derivative::Derivative;

use crate::ast::Info;
use crate::ast::Prim::*;
use crate::database::Compiler;
use crate::interpreter::{prim_add_strs, prim_pow, Res};

pub type FuncImpl = Box<dyn Fn(&dyn Compiler, Vec<&dyn Fn() -> Res>, Info) -> Res>;

pub fn get_implementation(name: String) -> Option<FuncImpl> {
    match name.as_str() {
        "print" => Some(Box::new(|_, args, info| {
            let val = args[0]()?;
            match val {
                Str(s, _) => print!("{}", s),
                s => print!("{:?}", s)
            };
            Ok(I32(0, info.clone()))
        })),
        "++" => Some(Box::new(|_, args, info| prim_add_strs(&args[0]()?, &args[1]()?, info))),
        "^" => Some(Box::new(|_, args, info| prim_pow(&args[0]()?, &args[1]()?, info))),
        "argc" => Some(Box::new(|db, _, info| {
            Ok(I32(db.options().interpreter_args.len() as i32, info))
        })),
        "argv" => Some(Box::new(|db, args, info| {
            use crate::errors::TError;
            match args[0]()? {
                I32(ind, _) => Ok(Str(db.options().interpreter_args[ind as usize].clone(), info)),
                value => Err(TError::TypeMismatch(
                    "Expected index to be of type i32".to_string(),
                    Box::new(value),
                    info,
                )),
            }
        })),
        _ => None
    }
}

#[derive(Derivative)]
#[derivative(PartialEq, Eq, Clone, Debug)]
pub struct Extern {
    pub name: String,
    pub operator: Option<(i32, bool)>, // (binding power, is_right_assoc) if the extern is an operator
    pub cpp_includes: String,
    pub cpp_code: String,
    pub cpp_arg_processor: String,
    pub cpp_flags: String,
}

pub fn get_externs() -> HashMap<String, Extern> {
    let mut externs = vec![
        Extern {
            name: "print".to_string(),
            operator: None,
            cpp_includes: "#include <iostream>".to_string(),
            cpp_code: "std::cout << ".to_string(),
            cpp_arg_processor: "".to_string(),
            cpp_flags: "".to_string(),
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
            cpp_code: "+".to_string(),
            cpp_arg_processor: "std::to_string".to_string(),
            cpp_flags: "".to_string(),
        },
        Extern {
            name: "^".to_string(),
            operator: Some((90, true)),
            cpp_includes: "#include <cmath>".to_string(),
            cpp_code: "pow".to_string(),
            cpp_arg_processor: "".to_string(),
            cpp_flags: "-lm".to_string(),
        },
        Extern {
            name: "argc".to_string(),
            operator: None,
            cpp_includes: "".to_string(),
            cpp_code: "argc".to_string(),
            cpp_arg_processor: "".to_string(),
            cpp_flags: "".to_string(),
        },
        Extern {
            name: "argv".to_string(),
            operator: None,
            cpp_includes: "".to_string(),
            cpp_code: "([&argv](const int x){return argv[x];})".to_string(),
            cpp_arg_processor: "".to_string(),
            cpp_flags: "".to_string(),
        },
    ];
    let mut extern_map: HashMap<String, Extern> = HashMap::new();
    while let Some(extern_def) = externs.pop() {
        extern_map.insert(extern_def.name.clone(), extern_def);
    }
    extern_map
}
