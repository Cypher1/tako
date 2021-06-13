use crate::ast::{Info, Node};
use crate::errors::TError;
use bitvec::prelude::*;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;

// i32 here are sizes in bits, not bytes.
// This means that we don't need to have a separate systems for bit&byte layouts.
pub type Offset = usize;

// A list of types with an offset to get to the first bit (used for padding, frequently 0).
type Layout = Vec<Val>; // Use a deque
pub type TypeSet = BTreeSet<Val>;
type Pack = BTreeSet<(String, Val)>;
pub type Frame = HashMap<String, Val>;

#[derive(PartialEq, Eq, Clone, PartialOrd, Ord, Hash)]
pub enum Prim {
    Bool(bool),
    I32(i32),
    Str(String),
    BuiltIn(String),
    Tag(BitVec), // An identifying bit string (prefix).
}

impl std::fmt::Debug for Prim {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Prim::*;
        match self {
            Bool(b) => write!(f, "{:?}", b)?,
            I32(i) => write!(f, "{:?}", i)?,
            Str(s) => write!(f, "'{}'", s)?,
            BuiltIn(b) => write!(f, "BuiltIn#{}", b)?,
            Tag(bits) => {
                write!(f, "b")?;
                for bit in bits.iter() {
                    write!(f, "{}", if *bit { '1' } else { '0' })?
                }
            }
        }
        Ok(())
    }
}

#[derive(PartialEq, Eq, Clone, PartialOrd, Ord, Hash)]
pub enum Val {
    PrimVal(Prim),
    // Complex types
    Pointer(Offset, Box<Val>), // Defaults to 8 bytes (64 bit)
    Lambda(Box<Node>),
    Struct(Vec<(String, Val)>), // Should really just store values, but we can't do that yet.
    Union(TypeSet),
    Product(TypeSet),
    Padded(Offset, Box<Val>),
    Function {
        intros: Pack,
        arguments: Box<Val>,
        results: Box<Val>,
    },
    App {
        inner: Box<Val>,
        arguments: Box<Val>,
    },
    // The following should be eliminated during lowering
    WithRequirement(Box<Val>, Vec<String>),
    Variable(String),
    BitStr(Offset),
}

pub fn merge_vals(left: Vec<(String, Val)>, right: Vec<(String, Val)>) -> Vec<(String, Val)> {
    let mut names = HashSet::<String>::new();
    for pair in right.iter() {
        names.insert(pair.0.clone());
    }
    let mut items = vec![];
    for pair in left.iter() {
        if !names.contains(&pair.0) {
            items.push(pair.clone());
        }
    }
    for pair in right.iter() {
        items.push(pair.clone());
    }
    items
}

pub fn tag(bits: BitVec) -> Val {
    Val::PrimVal(Prim::Tag(bits))
}

pub fn boolean(b: bool) -> Val {
    Val::PrimVal(Prim::Bool(b))
}

pub fn string(s: &str) -> Val {
    Val::PrimVal(Prim::Str(s.to_string()))
}

pub fn int32(i: i32) -> Val {
    Val::PrimVal(Prim::I32(i))
}

pub fn builtin(name: &str) -> Val {
    Val::PrimVal(Prim::BuiltIn(name.to_string()))
}

impl Val {
    pub fn into_struct(self: Val) -> Vec<(String, Val)> {
        match self {
            Struct(vals) => vals,
            _ => vec![("it".to_string(), self)],
        }
    }

    pub fn merge(self: Val, other: Val) -> Val {
        use Val::*;
        match (self, other) {
            (Struct(vals), Struct(o_vals)) => Struct(merge_vals(vals, o_vals)),
            (Struct(vals), other) => Struct(merge_vals(vals, other.into_struct())),
            (vals, Struct(other)) => Struct(merge_vals(vals.into_struct(), other)),
            (thing, other) => rec!["left" => thing, "right" => other],
        }
    }

    pub fn unify(self: &Val, other: &Val, env: &mut Vec<Frame>) -> Result<Val, TError> {
        match (self, other) {
            (Variable(name), ty) => {
                // TODO check if already assigned (and if so unify again)
                // TODO handle unwrap
                env.last_mut().unwrap().insert(name.to_string(), ty.clone());
                Ok(ty.clone())
            }
            _ => Ok(self.clone()),
        }
    }

    pub fn access(self: &Val, name: &str) -> Val {
        match self {
            PrimVal(Prim::BuiltIn(name)) => {
                panic!("Built in {} does not currently support introspection", name)
            }
            BitStr(_) | PrimVal(_) => void_type(),
            Lambda(_) => void_type(),
            Struct(tys) => {
                for (param, ty) in tys.iter() {
                    if param == name {
                        return ty.clone();
                    }
                }
                void_type()
            }
            Union(_) => void_type(),   // TODO
            Product(_) => void_type(), // TODO
            Padded(_, ty) => ty.access(name),
            Pointer(_, ty) => ty.access(name),
            Function {
                intros: _,
                arguments,
                results,
            } => match name {
                "arguments" => *arguments.clone(),
                "results" => *results.clone(),
                _ => void_type(),
            },
            App {
                inner: _,
                arguments: _,
            } => void_type(), // TODO
            WithRequirement(ty, effs) => WithRequirement(Box::new(ty.access(name)), effs.to_vec()),
            Variable(var) => Variable(format!("{}.{}", var, name)),
        }
    }
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#?}", self)
    }
}

impl std::fmt::Debug for Val {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let types = vec![
            (string_type(), "String"),
            (number_type(), "Number"),
            (i32_type(), "I32"),
            (byte_type(), "Byte"),
            (bit_type(), "Bit"),
            (trit_type(), "Trit"),
            (quad_type(), "Quad"),
        ];
        for (ty, name) in types.iter() {
            if self == ty {
                return write!(f, "{}", name);
            }
        }
        match self {
            PrimVal(prim) => write!(f, "{:?}", prim),
            BitStr(ptr_size) => write!(f, "Pointer<{}b>Code", ptr_size),
            Lambda(val) => write!(f, "{}", val),
            Struct(vals) => {
                let mut out = f.debug_struct("");
                for val in vals.iter() {
                    out.field(&val.0, &val.1);
                }
                out.finish()
            }
            Union(s) => {
                if s.is_empty() {
                    write!(f, "Void")
                } else {
                    let mut out = f.debug_tuple("|");
                    for sty in s {
                        out.field(sty);
                    }
                    out.finish()
                }
            }
            Product(s) => {
                if s.is_empty() {
                    write!(f, "Unit")
                } else {
                    let mut out = f.debug_tuple("*");
                    for sty in s {
                        out.field(sty);
                    }
                    out.finish()
                }
            }
            Pointer(ptr_size, ty) => write!(f, "Pointer<{}b>{:#?}", ptr_size, ty),
            Padded(size, t) => write!(f, "pad_{}#{:#?}", size, t),
            Function {
                intros,
                results,
                arguments,
            } => {
                if !intros.is_empty() {
                    let ints: Vec<String> = intros
                        .iter()
                        .map(|(name, ty)| format!("{}: {:#?}", name, ty))
                        .collect();
                    write!(f, "{}|-", ints.join("|-"))?;
                }
                write!(f, "{:#?} -> {:#?}", arguments, results)
            }
            App { inner, arguments } => write!(f, "({:#?})({:#?})", inner, arguments),
            WithRequirement(ty, effs) => write!(f, "{:#?}+{}", ty, effs.join("+")),
            Variable(name) => write!(f, "{}", name),
        }
    }
}

use Val::*;

impl Val {
    pub fn ptr(self: Val) -> Val {
        Pointer(8 * byte_size(), Box::new(self))
    }
    pub fn padded(self: Val, size: Offset) -> Val {
        if size == 0 {
            return self;
        }
        if let Padded(n, t) = self {
            return Padded(n + size, t);
        }
        Padded(size, Box::new(self))
    }
}

#[allow(dead_code)]
pub fn card(ty: &Val) -> Result<Offset, TError> {
    use Prim::*;
    use Val::*;
    match ty {
        PrimVal(Tag(_bits)) => Ok(1),
        BitStr(_ptr_size) => Err(TError::StaticPointerCardinality(Info::default())),
        Union(s) => {
            let mut sum = 0;
            for sty in s {
                sum += card(&sty)?;
            }
            Ok(sum)
        }
        Product(s) => {
            let mut prod = 1;
            for sty in s {
                prod *= card(&sty)?;
            }
            Ok(prod)
        }
        Pointer(_ptr_size, t) => card(&t),
        Padded(_size, t) => card(&t),
        x => Err(TError::UnknownCardOfAbstractType(
            format!("{:#?}", x),
            Info::default(),
        )),
    }
}

// Calculates the memory needed for a new instance in bits.
pub fn size(ty: &Val) -> Result<Offset, TError> {
    use Prim::*;
    use Val::*;
    match ty {
        PrimVal(Tag(bits)) => Ok(bits.len()),
        BitStr(ptr_size) => Ok(*ptr_size),
        Union(s) => {
            let mut res = 0;
            for sty in s.iter() {
                // This includes padding in size.
                let c = size(&sty)?;
                if res <= c {
                    res = c;
                }
            }
            Ok(res)
        }
        Product(s) => {
            let mut res = 0;
            for sty in s.iter() {
                let c = size(&sty)?;
                if res <= c {
                    res = c;
                }
            }
            Ok(res)
        }
        Pointer(ptr_size, _t) => Ok(*ptr_size),
        Padded(bits, t) => Ok(bits + size(t)?),
        Variable(name) => Err(TError::UnknownSizeOfVariableType(
            name.clone(),
            Info::default(),
        )),
        x => Err(TError::UnknownSizeOfAbstractType(
            format!("{:#?}", x),
            Info::default(),
        )),
    }
}

fn num_bits(n: Offset) -> Offset {
    let mut k = 0;
    let mut p = 1;
    loop {
        if n <= p {
            return k;
        }
        k += 1;
        p *= 2;
    }
}

pub fn bits(mut n: Offset, len: Offset) -> BitVec {
    let mut v: BitVec = bitvec![0; len];
    for mut b in v.iter_mut().rev() {
        if n == 0 {
            break;
        }
        *b = n % 2 != 0;
        n /= 2;
    }
    v
}

pub fn record(values: Layout) -> Result<Val, TError> {
    let mut layout = set![];
    let mut off = 0;
    for val in values {
        // Detect nested records?
        // Work out the padding here
        let size = size(&val)?;
        layout.insert(val.padded(off));
        off += size;
    }
    let mut tys = set![];
    add_to_product(&mut tys, &layout);
    Ok(Product(tys))
}

pub fn add_to_product(tys: &mut TypeSet, values: &TypeSet) {
    for val in values {
        if let Product(vals) = val {
            add_to_product(tys, vals);
        } else {
            tys.insert(val.clone()); // hopeless
        }
    }
}

pub fn sum(values: Vec<Val>) -> Result<Val, TError> {
    let mut layout = set![];
    let tag_bits = num_bits(values.len() as Offset);
    for (count, val) in values.into_iter().enumerate() {
        let mut tagged = tag(bits(count, tag_bits));
        if val != unit_type() {
            tagged = record(vec![tagged, val])?;
        }
        layout.insert(tagged);
    }
    Ok(Union(layout))
}

pub fn void_type() -> Val {
    Union(set![])
}

pub fn unit_type() -> Val {
    Product(set![])
}

pub fn bit_type() -> Val {
    sum(vec![unit_type(), unit_type()]).expect("bit should be safe")
}

pub fn trit_type() -> Val {
    sum(vec![unit_type(), unit_type(), unit_type()]).expect("trit should be safe")
}

pub fn quad_type() -> Val {
    record(vec![bit_type(), bit_type()]).expect("quad should be safe")
}

pub fn byte_type() -> Val {
    record(vec![
        bit_type(),
        bit_type(),
        bit_type(),
        bit_type(),
        bit_type(),
        bit_type(),
        bit_type(),
        bit_type(),
    ])
    .expect("byte should be safe")
}

pub fn byte_size() -> Offset {
    8
}

pub fn char_type() -> Val {
    byte_type()
}

pub fn string_type() -> Val {
    char_type().ptr()
}

pub fn i32_type() -> Val {
    record(vec![byte_type(), byte_type(), byte_type(), byte_type()]).expect("i32 should be safe")
}

pub fn number_type() -> Val {
    variable("Number")
}

pub fn type_type() -> Val {
    variable("Type")
}

pub fn variable(name: &str) -> Val {
    Variable(name.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    type Res = Result<(), TError>;

    #[test]
    fn bits_zero_length() -> Res {
        assert_eq!(bits(0, 0), bits![]);
        assert_eq!(bits(1, 0), bits![]);
        Ok(())
    }

    #[test]
    fn bits_one_length() -> Res {
        assert_eq!(bits(0, 1), bits![0]);
        assert_eq!(bits(1, 1), bits![1]);
        assert_eq!(bits(2, 1), bits![0]);
        assert_eq!(bits(3, 1), bits![1]);
        Ok(())
    }

    #[test]
    fn bits_two_length() -> Res {
        assert_eq!(bits(0, 2), bits![0, 0]);
        assert_eq!(bits(1, 2), bits![0, 1]);
        assert_eq!(bits(2, 2), bits![1, 0]);
        assert_eq!(bits(3, 2), bits![1, 1]);
        assert_eq!(bits(4, 2), bits![0, 0]);
        Ok(())
    }

    #[test]
    fn bits_three_length() -> Res {
        assert_eq!(bits(0, 3), bits![0, 0, 0]);
        assert_eq!(bits(1, 3), bits![0, 0, 1]);
        assert_eq!(bits(2, 3), bits![0, 1, 0]);
        assert_eq!(bits(3, 3), bits![0, 1, 1]);
        assert_eq!(bits(4, 3), bits![1, 0, 0]);
        assert_eq!(bits(5, 3), bits![1, 0, 1]);
        assert_eq!(bits(6, 3), bits![1, 1, 0]);
        assert_eq!(bits(7, 3), bits![1, 1, 1]);
        assert_eq!(bits(8, 3), bits![0, 0, 0]);
        Ok(())
    }

    #[test]
    fn void() -> Res {
        assert_eq!(card(&void_type()), Ok(0));
        assert_eq!(size(&void_type()), Ok(0));
        Ok(())
    }
    #[test]
    fn unit() -> Res {
        assert_eq!(card(&unit_type()), Ok(1));
        assert_eq!(size(&unit_type()), Ok(0));
        Ok(())
    }
    #[test]
    fn tag1_type() -> Res {
        assert_eq!(card(&tag(bits(1, 1))), Ok(1));
        assert_eq!(size(&tag(bits(1, 1))), Ok(1));
        Ok(())
    }
    #[test]
    fn tag2_type() -> Res {
        assert_eq!(card(&tag(bits(0, 2))), Ok(1));
        assert_eq!(size(&tag(bits(0, 2))), Ok(2));
        assert_eq!(card(&tag(bits(1, 2))), Ok(1));
        assert_eq!(size(&tag(bits(1, 2))), Ok(2));
        assert_eq!(card(&tag(bits(2, 2))), Ok(1));
        assert_eq!(size(&tag(bits(2, 2))), Ok(2));
        assert_eq!(card(&tag(bits(3, 2))), Ok(1));
        assert_eq!(size(&tag(bits(3, 2))), Ok(2));
        Ok(())
    }
    #[test]
    fn tag4_type() -> Res {
        assert_eq!(card(&tag(bits(4, 3))), Ok(1));
        assert_eq!(size(&tag(bits(4, 3))), Ok(3));
        Ok(())
    }

    #[test]
    fn union_n_type() -> Res {
        let union2 = Union(set![unit_type(), unit_type()]);
        assert_eq!(card(&union2), Ok(1));
        assert_eq!(size(&union2), Ok(0));
        let union3 = Union(set![unit_type(), unit_type(), unit_type()]);
        assert_eq!(card(&union3), Ok(1));
        assert_eq!(size(&union3), Ok(0));
        Ok(())
    }
    #[test]
    fn bit() -> Res {
        let bitt = bit_type();
        assert_eq!(card(&bitt), Ok(2));
        assert_eq!(size(&bitt), Ok(1));
        Ok(())
    }
    #[test]
    fn trit() -> Res {
        let trit = trit_type();
        assert_eq!(card(&trit), Ok(3));
        assert_eq!(size(&trit), Ok(2));
        Ok(())
    }
    #[test]
    fn nested_quad_type() -> Res {
        let quad = record(vec![bit_type(), bit_type()])?;
        assert_eq!(card(&quad), Ok(4));
        assert_eq!(size(&quad), Ok(2));
        Ok(())
    }
    #[test]
    fn quad() -> Res {
        let quad = quad_type();
        assert_eq!(card(&quad), Ok(4));
        assert_eq!(size(&quad), Ok(2));
        Ok(())
    }
    #[test]
    fn pent_type() -> Res {
        let pent = sum(vec![
            unit_type(),
            unit_type(),
            unit_type(),
            unit_type(),
            unit_type(),
        ])?;
        assert_eq!(card(&pent), Ok(5));
        assert_eq!(size(&pent), Ok(3));
        Ok(())
    }
    #[test]
    fn pair_bool_ptrs() -> Res {
        let bool_ptr = Pointer(64, Box::new(bit_type()));
        let quad = record(vec![bool_ptr.clone(), bool_ptr])?;
        assert_eq!(card(&quad), Ok(4));
        assert_eq!(size(&quad), Ok(2 * 64));
        Ok(())
    }
    #[test]
    fn nested_nibble() -> Res {
        let quad = record(vec![bit_type(), bit_type()])?;
        let nibble = record(vec![quad.clone(), quad])?;
        assert_eq!(card(&nibble), Ok(16));
        assert_eq!(size(&nibble), Ok(4));
        Ok(())
    }
    #[test]
    fn padded_nibble() -> Res {
        let quad = record(vec![bit_type().padded(2), bit_type()])?;
        let nibble = record(vec![quad.clone(), quad])?;
        assert_eq!(card(&nibble), Ok(16));
        assert_eq!(size(&nibble), Ok(8));
        Ok(())
    }

    #[test]
    fn bool_and_fn() -> Res {
        let fn_ptr = BitStr(64);
        let closure = record(vec![bit_type(), fn_ptr])?;
        assert_eq!(size(&closure), Ok(65));
        Ok(())
    }

    #[test]
    fn bool_or_fn() -> Res {
        let fn_ptr = BitStr(64);
        let closure = sum(vec![bit_type(), fn_ptr])?;
        assert_eq!(size(&closure), Ok(65));
        Ok(())
    }
}
