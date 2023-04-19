#![allow(unused)]
use crate::function::{Op, Function};
use std::collections::BTreeMap;


#[derive(Clone, Debug, Hash, Ord, Eq, PartialOrd, PartialEq)]
pub struct Table {
    pub rows: Vec<Vec<Value>>,
    pub columns: Vec<String>,
    // TODO: Indexes?
}

impl std::fmt::Display for Table {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut widths = vec![];
        write!(f, "\n|")?;
        for (i, col) in self.columns.iter().enumerate() {
            let start_w = 2+col.chars().count();
            let mut w = start_w;
            for row in &self.rows {
                let item = &row[i];
                w = std::cmp::max(w, 2+format!("{}", item).chars().count());
            }
            widths.push(w);
            let gap = w-start_w;
            let r = gap/2;
            write!(f, "{} {col} {}|", " ".repeat(r), " ".repeat(gap-r))?;
        }
        write!(f, "\n|")?;
        for w in &widths { write!(f, "{}|", "-".repeat(*w)); }
        for row in &self.rows {
            write!(f, "\n|")?;
            for (i, item) in row.iter().enumerate() {
                let content = format!("{item}");
                let gap = widths[i]-content.chars().count();
                let r = gap/2;
                write!(f, "{}{content}{}|", " ".repeat(r), " ".repeat(gap-r))?;
            }
        }
        write!(f, "\n|")?;
        for w in &widths { write!(f, "{}|", "-".repeat(*w)); }
        Ok(())
    }
}

#[derive(Clone, Debug, Hash, Ord, Eq, PartialOrd, PartialEq)]
pub enum Value {
    None,
    Bool(bool),
    I64(i64),
    // F64(f64),
    String(String), // Todo: Cow
    Some(Box<Value>), // Needed??? Todo: Cow
    // Bytes, Bigint, Dates, URLs etc.
    // Containers...?
    Vec(Vec<Value>),
    // Set(Set<Value>),
    Map(BTreeMap<Value, Value>),
    Op(Op),
    Function(Function),
    Table(Table),
    // Hmmmmm....
    // Reference(NodeId),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            // Value::Placeholder => write!(f, "<unset>"),
            Value::None => write!(f, "None"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::I64(i) => write!(f, "{i}"),
            Value::String(s) => write!(f, "{s:?}"),
            Value::Vec(vs) => {
                write!(f, "[")?;
                let mut first = true;
                for v in vs {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{v}")?;
                }
                write!(f, "]")
            }
            Value::Some(v) => write!(f, "Some({v})"),
            Value::Map(ent) => {
                write!(f, "{{")?;
                let mut first = true;
                for (k, v) in ent {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{k}: {v}")?;
                }
                write!(f, "}}")
            }
            Value::Op(op) => write!(f, "{op}"),
            Value::Function(fun) => write!(f, "{fun}"),
            Value::Table(tab) => write!(f, "{tab}"),
        }
    }
}

impl Into<Value> for i64 {
    fn into(self) -> Value {
        Value::I64(self)
    }
}

impl Into<Value> for String {
    fn into(self) -> Value {
        Value::String(self)
    }
}

impl Into<Value> for &str {
    fn into(self) -> Value {
        Value::String(self.to_string())
    }
}

#[derive(Clone, Debug, Hash, Ord, Eq, PartialOrd, PartialEq)]
enum ModelType {
    // No constructable 'Bottom'.
    Empty,                                 // Nothing...
    Prim(Value),                           // Exactly x.
    Range(Value, Value),                   // x .. y.
    OneOf(Vec<ModelType>),                 // x OR y.
    AllOf(Vec<ModelType>),                 // x AND y.
    Keyed(Box<ModelType>, Box<ModelType>), // x: y.
    // Refinement?
    // Lambda?
    // Pi?
    // Sigma?
    Any, // Top. (default)
}
