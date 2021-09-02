use crate::ast::Info;
use crate::database::DBStorage;
use crate::errors::TError;
use crate::externs::*;
use crate::primitives::{Prim::*, Val};
// use crate::primitives::{
// boolean, int32, merge_vals, never_type, Frame, Prim::*, Val::*,
// };
use log::*;
use std::collections::HashMap;

pub type ImplFn<'a> =
    &'a mut dyn FnMut(&mut DBStorage, HashMap<String, Box<dyn Fn() -> Res>>, Info) -> Res;
pub type PureImplFn<'a> =
    &'a dyn Fn(&DBStorage, HashMap<String, Box<dyn Fn() -> Res>>, Info) -> Res;

use specs::Entity;

#[derive(PartialEq, Eq, Clone, PartialOrd, Ord, Hash, Debug)]
pub enum StackValue {
    // This is for storing a lazily evaluated reference (in the place of a pointer into the program's source)
    StaticReference(Entity),
    Value(Val),
}

pub struct Mem {
    stack: Vec<StackValue>,
    // heap: Vec<StackValue>,
}

pub enum MemoryReference {
    Stack(u32, u32),
    Heap(u32, u32),
    // TODO: Consider adding Registers to avoid hitting the stack for every argument
}

impl Default for Mem {
    fn default() -> Self {
        Self {
            stack: Vec::new(),
            // heap: Vec::new(),
        }
    }
}

pub struct Interpreter<'functions, 'storage> {
    // Might be worth merging these two
    pub default_impls: HashMap<String, ImplFn<'functions>>,
    memory: Mem,
    // mapping: HashMap<Path, MemoryReference>, // where each symbol is stored
    storage: &'storage mut DBStorage,
}

impl<'functions, 'storage> Interpreter<'functions, 'storage> {
    pub fn new(storage: &'storage mut DBStorage) -> Interpreter<'functions, 'storage> {
        Interpreter {
            default_impls: HashMap::new(),
            memory: Mem::default(),
            // mapping: HashMap::new(),
            storage,
        }
    }

    pub fn eval(&mut self, entry_point: Entity) -> Res {
        let mut code = vec![entry_point]; // Inject the symbol to be evaluated. This is likely to be 'main'.
        let mut function_stack = vec![]; // function stack for quick 'returning'
        loop {
            if let Some(curr) = code.pop() {
                let res = self.step(curr, &mut code, &mut function_stack)?;
                trace!("eval step: {:?}", &res);
                self.memory.stack.push(res); // Put the result back on the stack.
            } else {
                if let Some(StackValue::Value(last)) = self.memory.stack.pop() {
                    return Ok(last);
                }
                return Err(TError::StackInterpreterRanOutOfCode(Info::default()));
            }
        }
    }

    fn step(
        &mut self,
        curr: Entity,
        _code: &mut Vec<Entity>,
        _function_stack: &mut Vec<usize>,
    ) -> Result<StackValue, TError> {
        if let Some(value) = self.storage.get_known_value(&curr) {
            return Ok(StackValue::Value(value));
        }
        let arity = self.storage.arity(&curr)?;
        if self.memory.stack.len() < arity {
            return Err(TError::StackInterpreterRanOutOfArguments(
                curr,
                arity,
                self.memory.stack.clone(),
                Info::default(),
            ));
        }
        // Get the code for the entity

        // Push the code (in reverse) onto the 'code' stack
        // so that the first instruction is the 'last' (first to be executed)

        // To 'call'
        // function_stack.push(code.len()); // Place to 'ret' back to.
        // code.push(ent);
        Ok(StackValue::Value(Val::PrimVal(I32(0)))) // TODO: This is clearly wrong
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::database::{AstNode, AstTerm};
    use crate::location::Loc;
    use crate::primitives::{boolean, int32, number_type, string, string_type};

    fn get_db() -> DBStorage {
        DBStorage::default()
    }

    #[test]
    fn eval_num() {
        let mut storage = get_db();
        let filename = "test/file.tk";
        let module_name = storage.module_name(filename.to_owned());
        let entity = storage.store_node(
            AstNode {
                term: AstTerm::Value(int32(12)),
                loc: Loc::default(),
                ty: None,
            },
            &module_name,
        );
        assert_eq!(Interpreter::new(&mut storage).eval(entity), Ok(int32(12)));
    }

    fn eval_str(storage: &mut DBStorage, s: &str) -> Res {
        let filename = "test/file.tk";
        let module_name = storage.module_name(filename.to_owned());
        storage.set_file(filename, s.to_string());

        let _root = storage.look_up_definitions(module_name.clone())?;
        let root_entity = storage
            .path_to_entity
            .get(&module_name)
            .expect("Expected an entity for the program")
            .clone();
        Interpreter::new(storage).eval(root_entity)
    }

    #[allow(dead_code)]
    fn trace<T: std::fmt::Display, E>(t: Result<T, E>) -> Result<T, E> {
        match &t {
            Ok(t) => debug!(">> {}", &t),
            Err(_) => debug!(">> #error"),
        }
        t
    }

    #[test]
    fn parse_and_eval_bool() {
        let db = &mut get_db();
        assert_eq!(eval_str(db, "true"), Ok(boolean(true)));
    }
}

/*
    #[test]
    fn parse_and_eval_bool_and() {
        let db = &mut get_db();
        assert_eq!(eval_str(db, "true&&true"), Ok(boolean(true)));
        assert_eq!(eval_str(db, "false&&true"), Ok(boolean(false)));
        assert_eq!(eval_str(db, "true&&false"), Ok(boolean(false)));
        assert_eq!(eval_str(db, "false&&false"), Ok(boolean(false)));
    }

    #[test]
    fn parse_and_eval_bool_or() {
        let db = &mut get_db();
        assert_eq!(eval_str(db, "true||true"), Ok(boolean(true)));
        assert_eq!(eval_str(db, "false||true"), Ok(boolean(true)));
        assert_eq!(eval_str(db, "true||false"), Ok(boolean(true)));
        assert_eq!(eval_str(db, "false||false"), Ok(boolean(false)));
    }

    #[test]
    fn parse_and_eval_bool_eq() {
        let db = &mut get_db();
        assert_eq!(eval_str(db, "true==true"), Ok(boolean(true)));
        assert_eq!(eval_str(db, "false==true"), Ok(boolean(false)));
        assert_eq!(eval_str(db, "true==false"), Ok(boolean(false)));
        assert_eq!(eval_str(db, "false==false"), Ok(boolean(true)));
    }

    #[test]
    fn parse_and_eval_i32() {
        let db = &mut get_db();
        assert_eq!(eval_str(db, "32"), Ok(int32(32)));
    }

    #[test]
    fn parse_and_eval_i32_eq() {
        let db = &mut get_db();
        assert_eq!(eval_str(db, "0==0"), Ok(boolean(true)));
        assert_eq!(eval_str(db, "-1==1"), Ok(boolean(false)));
        assert_eq!(eval_str(db, "1==123"), Ok(boolean(false)));
        assert_eq!(eval_str(db, "1302==1302"), Ok(boolean(true)));
    }

    #[test]
    fn parse_and_eval_i32_pow() {
        let db = &mut get_db();
        assert_eq!(eval_str(db, "2^3"), Ok(int32(8)));
        assert_eq!(eval_str(db, "3^2"), Ok(int32(9)));
        assert_eq!(eval_str(db, "-4^2"), Ok(int32(-16)));
        assert_eq!(eval_str(db, "(-4)^2"), Ok(int32(16)));
        assert_eq!(eval_str(db, "2^3^2"), Ok(int32(512)));
    }

    #[test]
    fn parse_and_eval_str() {
        let db = &mut get_db();
        assert_eq!(eval_str(db, "\"32\""), Ok(string("32")));
    }

    #[test]
    fn parse_and_eval_let() {
        let db = &mut get_db();
        assert_eq!(eval_str(db, "x=3;x"), Ok(int32(3)));
    }

    #[test]
    fn parse_and_eval_let_with_args() {
        let db = &mut get_db();
        assert_eq!(eval_str(db, "x(it)=it*2;x(3)"), Ok(int32(6)));
    }

    #[test]
    fn parse_and_eval_i32_type() {
        let db = &mut get_db();
        assert_eq!(eval_str(db, "I32"), Ok(crate::primitives::i32_type()));
    }

    #[test]
    fn parse_and_eval_number_type() {
        let db = &mut get_db();
        assert_eq!(eval_str(db, "Number"), Ok(crate::primitives::number_type()));
    }

    #[test]
    fn parse_and_eval_string_type() {
        let db = &mut get_db();
        assert_eq!(eval_str(db, "String"), Ok(crate::primitives::string_type()));
    }

    #[test]
    fn parse_and_eval_string_or_number_type() {
        let db = &mut get_db();
        assert_eq!(
            eval_str(db, "String | Number"),
            Ok(Union(set![number_type(), string_type()]))
        );
    }

    #[test]
    fn parse_and_eval_string_and_number_type() {
        let db = &mut get_db();
        assert_eq!(
            eval_str(db, "String & Number"),
            Ok(Product(set![number_type(), string_type()]))
        );
    }

    #[test]
    fn parse_and_eval_tagged_string_or_number_type() {
        use crate::primitives::*;
        let db = &mut get_db();
        assert_eq!(
            eval_str(db, "String + I32"),
            sum(vec![string_type(), i32_type()])
        );
    }

    #[test]
    fn parse_and_eval_string_times_number_type() {
        use crate::primitives::*;
        let db = &mut get_db();
        assert_eq!(
            eval_str(db, "String * I32"),
            record(vec![string_type(), i32_type()])
        );
    }

    #[test]
    fn parse_and_eval_struct_x4_y5_access_x() {
        let db = &mut get_db();
        assert_eq!(eval_str(db, "struct(x=4, y=5)(\"x\")"), Ok(int32(4)));
    }

    #[test]
    fn parse_and_eval_struct_x4_y5_access_y() {
        let db = &mut get_db();
        assert_eq!(
            eval_str(db, "struct(x=4, y=\"Hi\")(\"y\")"),
            Ok(string("Hi"))
        );
    }

    #[test]
    fn parse_and_eval_struct_x4_y5() {
        let db = &mut get_db();
        assert_eq!(
            eval_str(db, "\"\"++struct(x=4, y=\"Hi\")"),
            Ok(string("(((it==\'x\')-|4)?((it==\'y\')-|\'Hi\'))")) // Ok(Str("struct(x=4, y=\"Hi\")".to_string()))
        );
    }

    #[test]
    fn parse_and_eval_struct_empty() {
        let db = &mut get_db();
        assert_eq!(
            eval_str(db, "struct()"),
            Ok(Lambda(Box::new(Product(set![]).into_node())))
        );
    }

    #[test]
    fn parse_and_eval_print() {
        use crate::primitives::Prim::BuiltIn;
        use crate::primitives::Val::PrimVal;
        let db = &mut get_db();
        assert_eq!(
            eval_str(db, "print"),
            Ok(PrimVal(BuiltIn("print".to_string())))
        );
    }

    #[test]
    fn tako_add_eq_rust_eq() {
        use rand::Rng;
        let db = &mut get_db();
        let mut rng = rand::thread_rng();
        for _ in 0..100 {
            let num1: i32 = rng.gen();
            let num2: i32 = rng.gen();
            let res = num1.wrapping_add(num2);
            info!("mul {:?} + {:?} => {:?}", num1, num2, res);
            assert_eq!(
                eval_str(db, &format!("mul(x, y)=x+y;mul(x= {}, y= {})", num1, num2)),
                Ok(int32(res))
            );
        }
    }

    #[test]
    fn tako_mul_eq_rust_eq() {
        use rand::Rng;
        let db = &mut get_db();
        let mut rng = rand::thread_rng();
        for _ in 0..100 {
            let num1: i32 = rng.gen();
            let num2: i32 = rng.gen();
            let res = num1.wrapping_mul(num2);
            info!("mul {:?} * {:?} => {:?}", num1, num2, res);
            assert_eq!(
                eval_str(db, &format!("mul(x, y)=x*y;mul(x= {}, y= {})", num1, num2)),
                Ok(int32(res))
            );
        }
    }
}
*/
