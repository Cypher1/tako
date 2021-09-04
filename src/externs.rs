use std::collections::HashMap;
use std::convert::TryFrom;

use crate::ast::{Info, Node, ToNode};
use crate::database::DBStorage;
use crate::errors::TError;
use crate::primitives::{
    bit_type, boolean, builtin, i32_type, int32, never_type, number_type, string, string_type,
    type_type, unit_type, variable, Prim::{Bool, I32, Str}, Val, Val::{Function, Lambda, PrimVal, Product, Union, WithRequirement},
};

pub type Res = Result<Val, TError>;

pub fn prim_add(l: &Val, r: &Val, _info: &Info) -> Res {
    use crate::primitives::sum;
    match (l, r) {
        (PrimVal(Bool(l)), PrimVal(Bool(r))) => {
            Ok(int32(if *l { 1 } else { 0 } + if *r { 1 } else { 0 }))
        }
        (PrimVal(Bool(l)), PrimVal(I32(r))) => Ok(int32(r.wrapping_add(if *l { 1 } else { 0 }))),
        (PrimVal(Bool(l)), PrimVal(Str(r))) => Ok(PrimVal(Str(l.to_string() + &r.to_string()))),
        (PrimVal(I32(l)), PrimVal(Bool(r))) => Ok(int32(l.wrapping_add(if *r { 1 } else { 0 }))),
        (PrimVal(I32(l)), PrimVal(I32(r))) => Ok(int32(l.wrapping_add(*r))),
        (PrimVal(I32(l)), PrimVal(Str(r))) => Ok(PrimVal(Str(l.to_string() + &r.to_string()))),
        (PrimVal(Str(l)), PrimVal(Bool(r))) => Ok(PrimVal(Str(l.to_string() + &r.to_string()))),
        (PrimVal(Str(l)), PrimVal(I32(r))) => Ok(PrimVal(Str(l.to_string() + &r.to_string()))),
        (PrimVal(Str(l)), PrimVal(Str(r))) => Ok(PrimVal(Str(l.to_string() + &r.to_string()))),
        (l, r) => Ok(sum(vec![l.clone(), r.clone()])?),
        //(l, r) => Err(TError::TypeMismatch2(
        //"+".to_string(),
        //Box::new((*l).clone()),
        //Box::new((*r).clone()),
        //info,
        //)),
    }
}

pub fn prim_eq(l: &Val, r: &Val, info: &Info) -> Res {
    match (l, r) {
        (PrimVal(Bool(l)), PrimVal(Bool(r))) => Ok(boolean(*l == *r)),
        (PrimVal(I32(l)), PrimVal(I32(r))) => Ok(boolean(l == r)),
        (PrimVal(Str(l)), PrimVal(Str(r))) => Ok(boolean(l == r)),
        (l, r) => Err(TError::TypeMismatch2(
            "==".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info.clone(),
        )),
    }
}

pub fn prim_neq(l: &Val, r: &Val, info: &Info) -> Res {
    match (l, r) {
        (PrimVal(Bool(l)), PrimVal(Bool(r))) => Ok(boolean(*l != *r)),
        (PrimVal(I32(l)), PrimVal(I32(r))) => Ok(boolean(l != r)),
        (PrimVal(Str(l)), PrimVal(Str(r))) => Ok(boolean(l != r)),
        (l, r) => Err(TError::TypeMismatch2(
            "!=".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info.clone(),
        )),
    }
}

pub fn prim_gt(l: &Val, r: &Val, info: &Info) -> Res {
    match (l, r) {
        (PrimVal(Bool(l)), PrimVal(Bool(r))) => Ok(boolean(*l && !(*r))),
        (PrimVal(I32(l)), PrimVal(I32(r))) => Ok(boolean(l > r)),
        (PrimVal(Str(l)), PrimVal(Str(r))) => Ok(boolean(l > r)),
        (l, r) => Err(TError::TypeMismatch2(
            ">".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info.clone(),
        )),
    }
}

pub fn prim_gte(l: &Val, r: &Val, info: &Info) -> Res {
    match (l, r) {
        (PrimVal(Bool(l)), PrimVal(Bool(r))) => Ok(boolean(*l >= *r)),
        (PrimVal(I32(l)), PrimVal(I32(r))) => Ok(boolean(l >= r)),
        (PrimVal(Str(l)), PrimVal(Str(r))) => Ok(boolean(l >= r)),
        (l, r) => Err(TError::TypeMismatch2(
            ">=".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info.clone(),
        )),
    }
}

pub fn prim_sub(l: &Val, r: &Val, info: &Info) -> Res {
    match (l, r) {
        (PrimVal(I32(l)), PrimVal(Bool(r))) => Ok(int32(l - if *r { 1 } else { 0 })),
        (PrimVal(I32(l)), PrimVal(I32(r))) => Ok(int32(l - r)),
        (l, r) => Err(TError::TypeMismatch2(
            "-".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info.clone(),
        )),
    }
}

pub fn prim_mul(l: &Val, r: &Val, info: &Info) -> Res {
    use crate::primitives::record;
    let fail = || {
        Err(TError::TypeMismatch2(
            "*".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info.clone(),
        ))
    };
    match (l, r) {
        (PrimVal(Bool(l)), PrimVal(I32(r))) => Ok(int32(if *l { *r } else { 0 })),
        (PrimVal(Bool(l)), PrimVal(Str(r))) => Ok(string(if *l { r } else { "" })),
        (PrimVal(I32(l)), PrimVal(Bool(r))) => Ok(int32(if *r { *l } else { 0 })),
        (PrimVal(Str(l)), PrimVal(Bool(r))) => Ok(string(if *r { l } else { "" })),
        (PrimVal(Bool(_)), PrimVal(_)) | (PrimVal(_), PrimVal(Bool(_))) => fail(),
        (PrimVal(I32(l)), PrimVal(I32(r))) => Ok(int32(l.wrapping_mul(*r))),
        (l, r) => Ok(record(vec![l.clone(), r.clone()])?),
    }
}

pub fn prim_div(l: &Val, r: &Val, info: &Info) -> Res {
    match (l, r) {
        (PrimVal(I32(l)), PrimVal(I32(r))) => Ok(int32(l / r)),
        (l, r) => Err(TError::TypeMismatch2(
            "/".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info.clone(),
        )),
    }
}

pub fn prim_mod(l: &Val, r: &Val, info: &Info) -> Res {
    match (l, r) {
        (PrimVal(I32(l)), PrimVal(I32(r))) => Ok(int32(l % r)),
        (l, r) => Err(TError::TypeMismatch2(
            "%".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info.clone(),
        )),
    }
}

pub fn prim_and(l: &Val, r: &Val, info: &Info) -> Res {
    match (l, r) {
        (PrimVal(Bool(l)), PrimVal(Bool(r))) => Ok(boolean(*l && *r)),
        (l, r) => Err(TError::TypeMismatch2(
            "&&".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info.clone(),
        )),
    }
}

pub fn prim_or(l: &Val, r: &Val, info: &Info) -> Res {
    match (l, r) {
        (PrimVal(Bool(l)), PrimVal(Bool(r))) => Ok(boolean(*l || *r)),
        (l, r) => Err(TError::TypeMismatch2(
            "||".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info.clone(),
        )),
    }
}

pub fn prim_type_arrow(l: Val, r: Val, _info: &Info) -> Res {
    // TODO: add existential and forall quantification operators
    Ok(Val::Function {
        intros: dict!(),
        results: Box::new(r),
        arguments: Box::new(l),
    })
}

pub fn prim_type_and(l: Val, r: Val) -> Res {
    Ok(Val::Product(set!(l, r)))
}

pub fn prim_type_or(l: Val, r: Val, _info: &Info) -> Res {
    Ok(Val::Union(set!(l, r)))
}

pub fn prim_add_strs(l: &Val, r: &Val, _info: &Info) -> Res {
    let to_str = |v: &Val| {
        if let PrimVal(Str(s)) = v {
            s.to_string()
        } else {
            format!("{}", v)
        }
    };
    Ok(PrimVal(Str(format!("{}{}", to_str(l), to_str(r)))))
}

pub fn prim_pow(l: &Val, r: &Val, info: &Info) -> Res {
    match (l, r) {
        (PrimVal(I32(l)), PrimVal(Bool(r))) => Ok(int32(if *r { *l } else { 1 })),
        (PrimVal(I32(l)), PrimVal(I32(r))) => Ok(int32(i32::pow(*l, u32::try_from(*r).map_err(|_err|
        TError::TypeMismatch2(
            "^ (range error)".to_string(),
            Box::new(PrimVal(I32(*l))),
            Box::new(PrimVal(I32(*r))),
            info.clone(),
        ))?))), // TODO: require pos pow
        (l, r) => Err(TError::TypeMismatch2(
            "^".to_string(),
            Box::new((*l).clone()),
            Box::new((*r).clone()),
            info.clone(),
        )),
    }
}

pub type Args = HashMap<String, Box<dyn Fn() -> Res>>;
pub type FuncImpl = Box<dyn Fn(&DBStorage, Args, &Info) -> Res>;

fn get_symbol(args: &Args, sym: &str, info: &Info) -> Res {
    args.get(sym).map_or_else(
        || {
            Err(TError::UnknownSymbol(
                sym.to_string(),
                info.clone(),
                "".to_string(),
            ))
        },
        |val| val(),
    )
}

#[must_use]
pub fn get_implementation(name: &str) -> Option<FuncImpl> {
    match name {
        "print" => Some(Box::new(|_, args, info| {
            let val = get_symbol(&args, "it", info)?;
            match val {
                PrimVal(Str(s)) => print!("{}", s),
                s => print!("{:?}", s),
            };
            Ok(int32(0))
        })),
        "eprint" => Some(Box::new(|_, args, info| {
            let val = get_symbol(&args, "it", info)?;
            match val {
                PrimVal(Str(s)) => eprint!("{}", s),
                s => eprint!("{:?}", s),
            };
            Ok(int32(0))
        })),
        "struct" => Some(Box::new(|_, args, info| {
            use crate::ast::{BinOp, Sym};
            let mut sorted_args: Vec<_> = args.iter().collect();
            sorted_args.sort_by_key(|a| a.0);
            let mut requirements: Vec<Node> = vec![];
            for (name, arg) in &sorted_args {
                let arg = arg()?;
                requirements.push(
                    BinOp {
                        name: "-|".to_string(),
                        left: Box::new(
                            BinOp {
                                name: "==".to_string(),
                                left: Box::new(
                                    Sym {
                                        name: "it".to_string(),
                                        info: info.clone(),
                                    }
                                    .into_node(),
                                ),
                                right: Box::new(string(name).into_node()),
                                info: info.clone(),
                            }
                            .into_node(),
                        ),
                        right: Box::new(arg.into_node()),
                        info: info.clone(),
                    }
                    .into_node(),
                );
            }
            if let Some(first) = requirements.first() {
                let mut curr = first.clone(); // Nah don't clone
                for next in requirements[1..].iter() {
                    curr = BinOp {
                        name: "?".to_string(),
                        left: Box::new(curr),
                        right: Box::new(next.clone()),
                        info: info.clone(),
                    }
                    .into_node();
                }
                Ok(Lambda(Box::new(curr)))
            } else {
                Ok(Lambda(Box::new(Product(set![]).into_node())))
            }
        })),
        "." => Some(Box::new(|_db, args, info| {
            use crate::ast::{Apply, Let};

            let left = get_symbol(&args, "left", info)?;
            let right = get_symbol(&args, "right", info)?;
            Ok(Lambda(Box::new(
                Apply {
                    inner: Box::new(right.into_node()),
                    args: vec![Let {
                        name: "it".to_string(),
                        value: Box::new(left.into_node()),
                        args: None,
                        info: info.clone(),
                    }],
                    info: info.clone(),
                }
                .into_node(),
            )))
        })),
        "exit" => Some(Box::new(|_, args, info| {
            let val = get_symbol(&args, "it", info)?;
            let code = match val {
                PrimVal(I32(n)) => n,
                s => {
                    eprint!("{:?}", s);
                    1
                }
            };
            std::process::exit(code);
        })),
        "parse_i32" => Some(Box::new(|_, args, info| {
            let val = get_symbol(&args, "it", info)?;
            match val {
                PrimVal(Str(n)) => Ok(int32(n.parse::<i32>()?)),
                s => Err(TError::TypeMismatch(
                    "Expected parse_i32 argument to be a string encoded i32".to_string(),
                    Box::new(s),
                    info.clone(),
                )),
            }
        })),
        "++" => Some(Box::new(|_, args, info| {
            prim_add_strs(
                &get_symbol(&args, "left", info)?,
                &get_symbol(&args, "right", info)?,
                info,
            )
        })),
        "^" => Some(Box::new(|_, args, info| {
            prim_pow(
                &get_symbol(&args, "left", info)?,
                &get_symbol(&args, "right", info)?,
                info,
            )
        })),
        "argc" => Some(Box::new(|db, _, _info| {
            Ok(int32(
                i32::try_from(db.options.interpreter_args.len())
                    .expect("Too many interpreter arguments: length was not a valid i32"),
            ))
        })),
        "argv" => Some(Box::new(|db, args, info| {
            match get_symbol(&args, "it", info)? {
                PrimVal(I32(ind)) => Ok(string(
                    &db.options.interpreter_args[usize::try_from(ind)
                        .expect("Invalid interpreter argument index: index was not a valid usize")],
                )),
                value => Err(TError::TypeMismatch(
                    "Expected index to be of type i32".to_string(),
                    Box::new(value),
                    info.clone(),
                )),
            }
        })),
        "String" => Some(Box::new(|_db, _args, _info| Ok(string_type()))),
        "Int" => Some(Box::new(|_db, _args, _info| Ok(i32_type()))),
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
    pub value: Val,
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

lazy_static! {
    static ref EXTERN_MAP: HashMap<String, Extern> = {
        use Direction::{Left, Right};
        use Semantic::Func;
        let mut externs = vec![
            Extern {
                name: "argc".to_string(),
                value: builtin("argc"),
                semantic: Func,
                ty: i32_type().into_node(),
                cpp: LangImpl::new("[&argc](){return argc;}"),
            },
            Extern {
                name: "argv".to_string(),
                value: builtin("argv"),
                semantic: Func,
                ty: Function {
                    results: Box::new(string_type()),
                    intros: dict!(),
                    arguments: Box::new(rec!("it" => i32_type())),
                }.into_node(),
                cpp: LangImpl::new("([&argv](const int x){return argv[x];})"),
            },
            Extern {
                name: "eprint".to_string(),
                value: builtin("eprint"),
                semantic: Func,
                ty: Function {
                    results: Box::new(WithRequirement(
                        Box::new(unit_type()),
                        vec!["stderr".to_string()],
                    )),
                    arguments: Box::new(rec! {"it" => string_type()}),
                    intros: dict!(),
                }.into_node(),
                cpp: LangImpl::new("std::cerr << ").with_includes("#include <iostream>"),
            },
            Extern {
                name: "exit".to_string(),
                value: builtin("exit"),
                semantic: Func,
                ty: Function {
                    results: Box::new(never_type()),
                    arguments: Box::new(rec! {"it" => i32_type()}),
                    intros: dict!(),
                }.into_node(),
                cpp: LangImpl::new("[](const int code){exit(code);}")
                    .with_includes("#include <stdlib.h>"),
            },
            Extern {
                name: "parse_i32".to_string(),
                value: builtin("parse_i32"),
                semantic: Func,
                ty: Function {
                    results: Box::new(Union(set![i32_type(), never_type()])),
                    arguments: Box::new(rec! {"it" => string_type()}),
                    intros: dict!(),
                }.into_node(),
                cpp: LangImpl::new("std::stoi").with_includes("#include <string>"),
            },
            Extern {
                name: "print".to_string(),
                value: builtin("print"),
                semantic: Func,
                ty: Function {
                    results: Box::new(WithRequirement(
                        Box::new(unit_type()),
                        vec!["stdout".to_string()],
                    )),
                    arguments: Box::new(rec! {"it" => string_type()}),
                    intros: dict!(),
                }.into_node(),
                cpp: LangImpl::new("std::cout << ").with_includes("#include <iostream>"),
            },
            Extern {
                name: "pointer".to_string(),
                value: builtin("pointer"),
                semantic: Func,
                ty: Function {
                    results: Box::new(variable("a")),
                    arguments: Box::new(rec! {"it" => variable("Type")}),
                    intros: dict!("a" => variable("Type")),
                }.into_node(),
                cpp: LangImpl::new("std::cout << ").with_includes("#include <iostream>"),
            },
            Extern {
                name: "struct".to_string(),
                value: builtin("struct"),
                semantic: Func,
                ty: Function {
                    results: Box::new(variable("a")),
                    arguments: Box::new(variable("a")),
                    intros: dict!("a" => variable("Type")),
                }.into_node(),
                cpp: LangImpl::new("[](const int code){exit(code);}")
                    .with_includes("#include <stdlib.h>"),
            },
            Extern {
                name: ";".to_string(),
                value: builtin(";"),
                semantic: operator(20, Left),
                ty: Function {
                    intros: dict!("a" => variable("Type"), "b" => variable("Type")),
                    results: Box::new(variable("b")),
                    arguments: Box::new(rec!("left" => variable("a"), "right" => variable("b"))),
                }.into_node(),
                cpp: LangImpl::operator(";"),
            },
            Extern {
                name: ":".to_string(),
                value: builtin(":"),
                semantic: operator(21, Left),
                ty: Function {
                    intros: dict!("a" => variable("Type")),
                    results: Box::new(variable("b")),
                    arguments: Box::new(rec!("left" => variable("a"), "right" => variable("a"))),
                }.into_node(),
                cpp: LangImpl::operator(":"),
            },
            Extern {
                name: ",".to_string(),
                value: builtin(","),
                semantic: operator(30, Left),
                ty: Function {
                    intros: dict!("a" => variable("Type"), "b" => variable("Type")),
                    results: Box::new(Product(set!(variable("a"), variable("b")))),
                    arguments: Box::new(rec!("left" => variable("a"), "right" => variable("b"))),
                }.into_node(),
                cpp: LangImpl::operator(";"),
            },
            Extern {
                name: "=".to_string(),
                value: builtin("="),
                semantic: operator(40, Right),
                ty: Function {
                    intros: dict!("a" => variable("Identifier"), "b" => variable("Type")),
                    results: Box::new(variable("b")),
                    arguments: Box::new(rec!("left" => variable("a"), "right" => variable("b"))),
                }.into_node(),
                cpp: LangImpl::operator(" = "),
            },
            Extern {
                name: "?".to_string(),
                value: builtin("?"),
                semantic: operator(45, Left),
                ty: Function {
                    intros: dict!("a" => variable("Type"), "b" => variable("Type")),
                    results: Box::new(Union(set!(variable("a"), variable("b")))),
                    arguments: Box::new(rec!("left" => variable("a"), "right" => variable("b"))),
                }.into_node(),
                cpp: LangImpl::operator("?"),
            },
            Extern {
                name: "|-".to_string(),
                value: builtin("|-"),
                semantic: operator(46, Right),
                ty: Function {
                    intros: dict!("a" => variable("Type")),
                    results: Box::new(variable("a")),
                    arguments: Box::new(rec!("left" => variable("Type"), "right" => variable("a"))),
                }.into_node(),
                cpp: LangImpl::operator("|-"),
            },
            Extern {
                name: "-|".to_string(),
                value: builtin("-|"),
                semantic: operator(47, Left),
                ty: Function {
                    intros: dict!("a" => variable("Type")),
                    results: Box::new(variable("a")),
                    arguments: Box::new(rec!("left" => variable("Type"), "right" => variable("a"))),
                }.into_node(),
                cpp: LangImpl::operator("-|"),
            },
            Extern {
                name: "|".to_string(),
                value: builtin("|"),
                semantic: operator(48, Left),
                ty: Function {
                    intros: dict!("a" => variable("Type"), "b" => variable("Type")),
                    results: Box::new(Union(set!(variable("a"), variable("b")))),
                    arguments: Box::new(rec!("left" => variable("a"), "right" => variable("b"))),
                }.into_node(),
                cpp: LangImpl::operator("|"),
            },
            Extern {
                name: "&".to_string(),
                value: builtin("&"),
                semantic: operator(48, Left),
                ty: Function {
                    intros: dict!("a" => variable("Type"), "b" => variable("Type")),
                    results: Box::new(Product(set!(variable("a"), variable("b")))),
                    arguments: Box::new(rec!("left" => variable("a"), "right" => variable("b"))),
                }.into_node(),
                cpp: LangImpl::operator("&"),
            },
            Extern {
                name: "++".to_string(),
                value: builtin("++"),
                semantic: operator(49, Left),
                ty: Function {
                    intros: dict!("a" => variable("Display"), "b" => variable("Display")),
                    results: Box::new(string_type()),
                    arguments: Box::new(rec!("left" => variable("a"), "right" => variable("b"))),
                }.into_node(),
                cpp: LangImpl::operator("+")
                    .with_arg_processor("std::to_string")
                    .with_includes(
                        "\
#include <string>
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
                value: builtin("->"),
                semantic: operator(50, Left),
                ty: Function {
                    intros: dict!("a" => variable("Type"), "b" => variable("Type")),
                    results: Box::new(Function {
                        intros: dict!(),
                        results: Box::new(variable("b")),
                        arguments: Box::new(variable("a")),
                    }),
                    arguments: Box::new(rec!("left" => variable("a"), "right" => variable("b"))),
                }.into_node(),
                cpp: LangImpl::operator("???"),
            },
            Extern {
                name: "<".to_string(),
                value: builtin("<"),
                semantic: operator(51, Left),
                ty: Function {
                    intros: dict!("a" => variable("Number"), "b" => variable("Number")),
                    results: Box::new(bit_type()),
                    arguments: Box::new(rec!("left" => variable("a"), "right" => variable("b"))),
                }.into_node(),
                cpp: LangImpl::operator("<"),
            },
            Extern {
                name: "<=".to_string(),
                value: builtin("<="),
                semantic: operator(51, Left),
                ty: Function {
                    intros: dict!("a" => variable("Number"), "b" => variable("Number")),
                    results: Box::new(bit_type()),
                    arguments: Box::new(rec!("left" => variable("a"), "right" => variable("b"))),
                }.into_node(),
                cpp: LangImpl::operator("<="),
            },
            Extern {
                name: ">".to_string(),
                value: builtin(">"),
                semantic: operator(51, Left),
                ty: Function {
                    intros: dict!("a" => variable("Number"), "b" => variable("Number")),
                    results: Box::new(bit_type()),
                    arguments: Box::new(rec!("left" => variable("a"), "right" => variable("b"))),
                }.into_node(),
                cpp: LangImpl::operator(">"),
            },
            Extern {
                name: ">=".to_string(),
                value: builtin(">="),
                semantic: operator(51, Left),
                ty: Function {
                    intros: dict!("a" => variable("Number"), "b" => variable("Number")),
                    results: Box::new(bit_type()),
                    arguments: Box::new(rec!("left" => variable("a"), "right" => variable("b"))),
                }.into_node(),
                cpp: LangImpl::operator(">="),
            },
            Extern {
                name: "!=".to_string(),
                value: builtin("!="),
                semantic: operator(51, Left),
                ty: Function {
                    intros: dict!("a" => variable("Type"), "b" => variable("Type")),
                    results: Box::new(bit_type()),
                    arguments: Box::new(rec!("left" => variable("a"), "right" => variable("b"))),
                }.into_node(),
                cpp: LangImpl::operator("!="),
            },
            Extern {
                name: "==".to_string(),
                value: builtin("=="),
                semantic: operator(51, Left),
                ty: Function {
                    intros: dict!("a" => variable("Type"), "b" => variable("Type")),
                    results: Box::new(bit_type()),
                    arguments: Box::new(rec!("left" => variable("a"), "right" => variable("b"))),
                }.into_node(),
                cpp: LangImpl::operator("=="),
            },
            Extern {
                name: "||".to_string(),
                value: builtin("||"),
                semantic: operator(60, Left),
                ty: Function {
                    intros: dict!(),
                    results: Box::new(bit_type()),
                    arguments: Box::new(rec!("left" => bit_type(), "right" => bit_type())),
                }.into_node(),
                cpp: LangImpl::operator("||"),
            },
            Extern {
                name: "&&".to_string(),
                value: builtin("&&"),
                semantic: operator(60, Left),
                ty: Function {
                    intros: dict!(),
                    results: Box::new(bit_type()),
                    arguments: Box::new(rec!("left" => bit_type(), "right" => bit_type())),
                }.into_node(),
                cpp: LangImpl::operator("&&"),
            },
            Extern {
                name: "!".to_string(),
                value: builtin("!"),
                semantic: operator(70, Left),
                ty: Function {
                    intros: dict!(),
                    results: Box::new(bit_type()),
                    arguments: Box::new(bit_type()),
                }.into_node(),
                cpp: LangImpl::operator("!"),
            },
            Extern {
                name: "...".to_string(),
                value: builtin("..."),
                semantic: operator(70, Left),
                ty: Function {
                    intros: dict!("a" => variable("Type")), // TODO: This should unpack a type with a set of named values and put them into scope.
                    results: Box::new(variable("a")),
                    arguments: Box::new(rec!("it" => variable("a"))),
                }.into_node(),
                cpp: LangImpl::operator("..."), // TODO: Implement
            },
            Extern {
                name: "-".to_string(),
                value: builtin("-"),
                semantic: operator(70, Left),
                ty: Function {
                    intros: dict!("a" => variable("Number")),
                    results: Box::new(variable("a")),
                    arguments: Box::new(rec!("it" => variable("a"))),
                }.into_node(),
                cpp: LangImpl::operator("-"),
            },
            Extern {
                name: "+".to_string(),
                value: builtin("+"),
                semantic: operator(70, Left),
                ty: Function {
                    intros: dict!("a" => variable("Number"), "b" => variable("Number")),
                    results: Box::new(variable("a")),
                    arguments: Box::new(rec!("left" => variable("a"), "right" => variable("b"))),
                }.into_node(),
                cpp: LangImpl::operator("+"),
            },
            Extern {
                name: "*".to_string(),
                value: builtin("*"),
                semantic: operator(80, Left),
                ty: Function {
                    intros: dict!("a" => variable("Number"), "b" => variable("Number")),
                    results: Box::new(variable("a")),
                    arguments: Box::new(rec!("left" => variable("a"), "right" => variable("b"))),
                }.into_node(),
                cpp: LangImpl::operator("*"),
            },
            Extern {
                name: "%".to_string(),
                value: builtin("%"),
                semantic: operator(80, Left),
                ty: Function {
                    intros: dict!("a" => variable("Number"), "b" => variable("Number")),
                    results: Box::new(variable("a")),
                    arguments: Box::new(rec!("left" => variable("a"), "right" => variable("b"))),
                }.into_node(),
                cpp: LangImpl::operator("%"),
            },
            Extern {
                name: "/".to_string(),
                value: builtin("/"),
                semantic: operator(80, Left),
                ty: Function {
                    intros: dict!("a" => variable("Number"), "b" => variable("Number")),
                    results: Box::new(variable("a")),
                    arguments: Box::new(rec!("left" => variable("a"), "right" => variable("b"))),
                }.into_node(),
                cpp: LangImpl::operator("/"),
            },
            Extern {
                name: "^".to_string(),
                value: builtin("^"),
                semantic: operator(90, Right),
                ty: Function {
                    intros: dict!("a" => variable("Number"), "b" => variable("Number")),
                    results: Box::new(variable("a")),
                    arguments: Box::new(rec!("left" => variable("a"), "right" => variable("b"))),
                }.into_node(),
                cpp: LangImpl::new("pow")
                    .with_includes("#include <cmath>")
                    .with_arg_joiner(", ")
                    .with_flag("-lm"),
            },
            Extern {
                name: ".".to_string(),
                value: builtin("."),
                semantic: operator(100, Right),
                ty: Function {
                    intros: dict!("a" => variable("Type"), "b" => variable("Type"), "c" => variable("Type")),
                    results: Box::new(variable("c")),
                    arguments: Box::new(
                        rec!("left" => variable("a"), "right" => Function{intros: dict!(), arguments: Box::new(rec!("it" => variable("a"))), results: Box::new(variable("c"))}),
                    ),
                }.into_node(),
                cpp: LangImpl::new("[](const auto l, const auto r){return r(l);}"),
            },
            Extern {
                name: "I32".to_string(),
                value: i32_type(),
                semantic: Func,
                ty: variable("Type").into_node(),
                cpp: LangImpl::new("int32_t"),
            },
            Extern {
                name: "Number".to_string(),
                value: number_type(),
                semantic: Func,
                ty: variable("Type").into_node(),
                cpp: LangImpl::new("usize"),
            },
            Extern {
                name: "String".to_string(),
                value: string_type(),
                semantic: Func,
                ty: variable("Type").into_node(),
                cpp: LangImpl::new("std::string").with_includes("#include <string>"),
            },
            Extern {
                name: "Bit".to_string(),
                value: bit_type(),
                semantic: Func,
                ty: variable("Type").into_node(),
                cpp: LangImpl::new("short"),
            },
            Extern {
                name: "Unit".to_string(),
                value: unit_type(),
                semantic: Func,
                ty: variable("Type").into_node(),
                cpp: LangImpl::new("void"),
            },
            Extern {
                name: "Never".to_string(),
                value: never_type(),
                semantic: Func,
                ty: variable("Never").into_node(),
                cpp: LangImpl::new("/*Never: should never happen*/ auto"),
            },
            Extern {
                name: "Type".to_string(),
                value: type_type(),
                semantic: Func,
                ty: variable("Type").into_node(),
                cpp: LangImpl::new("auto"),
            },
        ];
        let mut extern_map: HashMap<String, Extern> = HashMap::new();
        while let Some(extern_def) = externs.pop() {
            extern_map.insert(extern_def.name.clone(), extern_def);
        }
        extern_map
    };
}

#[must_use]
pub fn get_externs() -> &'static HashMap<String, Extern> {
    &EXTERN_MAP
}
