use crate::error::GraphErr;
use crate::node::NodeId;
use crate::graph::Graph;
use crate::value::{Value, Table};
use std::collections::{BTreeMap, HashSet};
use std::fmt::Write;

mod builder;
pub use builder::FunctionBuilder;

#[derive(Copy, Clone, Debug, Hash, Ord, Eq, PartialOrd, PartialEq)]
pub enum Op {
    // On Entities
    Accessor,
    Select,
    OrderBy,
    GroupBy,
    Unique,
    Index,
    Slice,
    Where,
    // On Prims
    Neg,
    Not,
    // On a vector:
    // A join is an entity...

    // OneWay ops...
    // On a vector or set of arguments:
    FloatSum,
    IntSum,
    ProductF,
    ProductI,
    StrConcat,
    VecConcat,
    // Fold(Function),
    // On an entity:
    Modulo,
    Eq,
    If,
    Substr,
    MakeMap, // Takes n pairs...???
    MakeTable, // Takes vector of values and makes table...
    Keys,    // Takes map and gets keys as vector
    Values,  // Takes map and gets values as vector
    Items,   // Takes map and gets keys and values as vector
    Over,
    SortedBy,
}

impl std::fmt::Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

fn sum(total: &mut i64, val: &Value) -> Result<(), GraphErr> {
    match val {
        Value::I64(i) => {
            *total += *i;
        }
        Value::Vec(vs) => {
            for v in vs {
                sum(total, v)?;
            }
        }
        _ => {
            return Err(GraphErr::TypeError(
                Op::IntSum,
                val.clone(),
                "number or set of numbers".to_string(),
            ));
        }
    }
    Ok(())
}

fn str_concat(total: &mut String, val: &Value) -> Result<(), GraphErr> {
    match val {
        Value::String(s) => {
            write!(total, "{s}")?;
        }
        Value::I64(i) => {
            write!(total, "{i}")?;
        }
        Value::Vec(vs) => {
            for v in vs {
                str_concat(total, v)?;
            }
        }
        _ => {
            return Err(GraphErr::TypeError(
                Op::IntSum,
                val.clone(),
                "number or set of numbers".to_string(),
            ));
        }
    }
    Ok(())
}

fn vec_concat(total: &mut Vec<Value>, val: &Value) -> Result<(), GraphErr> {
    match val {
        Value::Vec(vals) => {
            for val in vals {
                total.push(val.clone());
            }
        }
        val => total.push(val.clone()),
    }
    Ok(())
}

impl Op {
    pub fn compute(&self, graph: &Graph, args: &[Value]) -> Result<Value, GraphErr> {
        match self {
            Op::IntSum => {
                let mut s = 0;
                for arg in args {
                    sum(&mut s, arg)?;
                }
                Ok(Value::I64(s))
            }
            Op::StrConcat => {
                let mut s = "".to_string();
                for arg in args {
                    str_concat(&mut s, arg)?;
                }
                Ok(Value::String(s))
            }
            Op::VecConcat => {
                let mut s = vec![];
                for arg in args {
                    vec_concat(&mut s, arg)?;
                }
                Ok(Value::Vec(s))
            }
            Op::Not => {
                // TODO: Check args?
                let Value::Bool(arg) = args[0] else { todo!() };
                Ok(Value::Bool(!arg))
            }
            Op::Eq => {
                let mut eq = true;
                // TODO: Check args?
                let first = &args[0];
                for arg in &args[1..] {
                    if arg != first {
                        eq = false;
                        break;
                    }
                }
                Ok(Value::Bool(eq))
            }
            Op::MakeMap => {
                let mut map: BTreeMap::<Value, Value> = BTreeMap::new();
                // TODO: Check args?
                if args.len() % 2 != 0 {
                    todo!("Wrong number of arguments!?")
                }
                for kv in args.chunks(2) {
                    match kv {
                        [k, v] => {
                            map.insert((*k).clone(), (*v).clone());
                        }
                        _ => todo!(),
                    }
                }
                Ok(Value::Map(map))
            }
            Op::MakeTable => {
                // TODO: Check args?
                let Value::Vec(new_cols) = args[0].clone() else { todo!() };
                let mut columns = vec![];
                for col in new_cols {
                    if let Value::String(col) = col {
                        columns.push(col);
                    } else {
                        todo!();
                    }
                }
                let mut rows = vec![];
                let Value::Vec(new_rows) = args[1].clone() else { todo!() };
                for new_row in new_rows {
                    match new_row {
                        Value::Vec(new_row) => {
                            if new_row.len() == columns.len() {
                                rows.push(new_row);
                            } else {
                                todo!();
                            }
                        }
                        Value::Map(mut kvs) => {
                            let mut row = vec![];
                            for col in &columns {
                                let value = kvs.remove(&Value::String(col.to_string()));
                                match value {
                                    Some(value) => {
                                        row.push(value);
                                    }
                                    _ => todo!(),
                                }
                            }
                            rows.push(row);
                        }
                        _ => { todo!(); }
                    }
                }
                let table = Table {
                    rows,
                    columns,
                };
                Ok(Value::Table(table))
            }
            Op::Accessor => {
                // TODO: check num arguments.
                let mut value = args[0].clone();
                for key in &args[1..] {
                    value = match &value {
                        Value::Map(map) => {
                            if let Some(value) = map.get(key) {
                                value.clone()
                            } else {
                                Value::None
                            }
                        }
                        Value::Vec(vec) => {
                            match key {
                                Value::I64(i) => {
                                    if let Some(value) = vec.get(*i as usize) {
                                        value.clone()
                                    } else {
                                        Value::None
                                    }
                                }
                                _ => todo!(),
                            }
                        }
                        _ => {
                            eprintln!("WHAT");
                            todo!()
                        }
                    };
                }
                Ok(value)
            }
            Op::Keys => {
                // TODO: check num arguments.
                let mut keys = vec![];
                let map = match &args[0] {
                    Value::Map(map) => map,
                    _ => todo!(),
                };
                for (k, _) in map {
                    keys.push(k.clone());
                }
                Ok(Value::Vec(keys))
            }
            Op::Values => {
                // TODO: check num arguments.
                let mut values = vec![];
                let map = match &args[0] {
                    Value::Map(map) => map,
                    _ => todo!(),
                };
                for (_, v) in map {
                    values.push(v.clone());
                }
                Ok(Value::Vec(values))
            }
            Op::If => {
                // TODO: check num arguments.
                let Value::Bool(cond) = &args[0] else { todo!() };
                let v_then = &args[1];
                let v_else = &args[2];
                Ok(if *cond {
                    v_then.clone()
                } else {
                    v_else.clone()
                })
            }
            Op::Over => {
                let Value::Function(fun) = &args[0] else { todo!() };
                let Value::Vec(vec) = &args[1] else { todo!() };
                let mut new_vec = vec![];
                for val in vec {
                    new_vec.push(fun.compute(graph, &[val.clone()])?);
                }
                Ok(Value::Vec(new_vec))
            }
            Op::SortedBy => {
                let Value::Function(fun) = &args[0] else { todo!() };
                let Value::Vec(vec) = &args[1] else { todo!() };
                let mut new_vec = vec![];
                for val in vec {
                    new_vec.push((fun.compute(graph, &[val.clone()])?, val.clone()));
                }
                new_vec.sort();
                let new_vec = new_vec.into_iter().map(|(_key, val)| val).collect();
                Ok(Value::Vec(new_vec))
            }
            _ => todo!(),
        }
    }

    // pub fn reversed(&self, _value: Value) -> Result<Value, GraphErr> {
    // todo!()
    // }
}

type Index = u32;
type Count = u32;

#[derive(Clone, Debug, Hash, Ord, Eq, PartialOrd, PartialEq)]
pub enum FunctionOp {
    /* Constants */
    Value(Value),
    Reference(NodeId),
    Index(Index),
    Binding(Index),
    /* Function */
    MakeVec(Count), // Make a vector from the previous N arguments
    // Abstraction(Index),
    /* Computation */
    Apply(Index), // Apply prev to previous prev.
}

impl Into<FunctionOp> for NodeId {
    fn into(self: Self) -> FunctionOp {
        Reference(self)
    }
}

impl Into<FunctionOp> for &str {
    fn into(self: Self) -> FunctionOp {
        Value(self.into())
    }
}

impl Into<FunctionOp> for Value {
    fn into(self: Self) -> FunctionOp {
        Value(self)
    }
}

use FunctionOp::*;

#[derive(Default, Clone, Debug, Hash, Ord, Eq, PartialOrd, PartialEq)]
pub struct Function {
    // TODO: Introduce a simpler repr for single values?
    lambdas: Vec<FunctionOp>, // Root is last element.
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.print_range((self.lambdas.len() as Index) - 1, f)
    }
}

impl Into<Function> for Value {
    fn into(self) -> Function {
        // Costly representation?
        Function {
            lambdas: vec![Value(self)],
        }
    }
}

impl Function {
    fn print_range(&self, index: Index, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let fop = self
            .lambdas
            .get(index as usize)
            .expect("Expected index to be valid");
        // eprintln!("PRINTING: {index} {fop:?}");
        match fop {
            Value(value) => write!(f, "{value}"),
            Binding(index) => write!(f, "binding#{index}"),
            Index(index) => self.print_range(*index, f),
            Reference(id) => write!(f, "{id}"),
            // Abstraction(ind) => {
            // write!(f, "abs(")?;
            // self.print_range(*ind, f)?;
            // write!(f, ")")
            // }
            MakeVec(num) => {
                let mut first = true;
                for i in 0..(*num) {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    self.print_range(index - num + i, f)?;
                }
                Ok(())
            }
            Apply(function) => {
                self.print_range(*function, f)?;
                write!(f, "(")?;
                let i = index - 1;
                match self.lambdas.get(i as usize) {
                    Some(MakeVec(_)) => {}
                    _ => write!(f, "...")?,
                }
                self.print_range(i, f)?;
                write!(f, ")")
            }
        }
    }

    pub fn dependencies(&self) -> HashSet<NodeId> {
        let mut refs = HashSet::new();
        for op in &self.lambdas {
            if let FunctionOp::Reference(id) = op {
                refs.insert(*id);
            }
        }
        refs
    }

    pub fn new() -> Self {
        Self::default()
    }

    pub fn builder() -> FunctionBuilder {
        FunctionBuilder::default()
    }

    pub fn build(f: impl FnOnce(&mut FunctionBuilder)) -> Self {
        let mut s = Self::builder();
        f(&mut s);
        s.finalize()
    }

    pub fn source(&self) -> Result<NodeId, GraphErr> {
        todo!()
    }

    fn compute_at_index(
        &self,
        index: Index,
        graph: &Graph,
        bindings: &[Value],
    ) -> Result<Value, GraphErr> {
        let op = self.lambdas.get(index as usize).expect("Get valid index");
        match op {
            Value(value) => Ok(value.clone()),
            Reference(id) => graph.compute(*id, bindings),
            Binding(index) => Ok(bindings[*index as usize].clone()),
            Index(index) => self.compute_at_index(*index, graph, bindings),
            // Abstraction(id) => self.compute_at_index(*id, graph, bindings),
            MakeVec(num) => {
                let mut args = vec![];
                for i in 0..(*num) {
                    let val = self.compute_at_index(index - num + i, graph, bindings)?;
                    args.push(val);
                }
                Ok(Value::Vec(args))
            }
            Apply(function) => {
                let f = self.compute_at_index(*function, graph, bindings)?; // Get the op?
                let Value::Vec(args) = self.compute_at_index(index-1, graph, bindings)? else { todo!() }; // Get the args?
                match f {
                    Value::Op(op) => op.compute(graph, &args),
                    _ => todo!(),
                }
            }
        }
    }

    pub fn compute(&self, graph: &Graph, bindings: &[Value]) -> Result<Value, GraphErr> {
        self.compute_at_index((self.lambdas.len() - 1) as u32, graph, bindings)
    }

    pub fn has_setter(&self) -> bool {
        // TODO: Find simple cases that can be reversed!
        false
    }
    pub fn reversed(&self, _value: Value) -> Result<Value, GraphErr> {
        // TODO: Find simple cases that can be reversed!
        Err(GraphErr::FunctionCannotBeReversed(self.clone()))
    }
}
