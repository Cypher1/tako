use crate::ast::Info;
use crate::ast::Node;
use crate::errors::TError;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;

// i32 here are sizes in bits, not bytes.
// This means that we don't need to have a separate systems for bit&byte layouts.
pub type Offset = usize;

// A list of types with an offset to get to the first bit (used for padding, frequently 0).
type Layout = Vec<Val>;
type TypeSet = BTreeSet<Val>;
type Pack = BTreeSet<(String, Val)>;
pub type Frame = HashMap<String, Val>;

type BitString = Vec<bool>;
// TODO: Consider using https://docs.rs/bitvec/0.22.3/bitvec/ instead of a BitString

#[derive(PartialEq, Eq, Clone, PartialOrd, Ord, Debug, Hash)]
pub enum Prim {
    Bool(bool),
    I32(i32),
    Str(String),
    BuiltIn(String),
    Tag(BitString), // An identifying bit string (prefix).
    BitStr(Offset),
}

#[derive(PartialEq, Eq, Clone, PartialOrd, Ord, Debug, Hash)]
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

pub fn tag(bits: BitString) -> Val {
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
            PrimVal(_) => void_type(),
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
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let types = vec![
            (string_type(), "String"),
            (number_type(), "Number"),
            (i32_type(), "I32"),
            (byte_type(), "Byte"),
            (bit_type(), "Bit"),
        ];
        for (ty, name) in types.iter() {
            if self == ty {
                return write!(f, "{}", name);
            }
        }
        use Prim::*;
        match self {
            PrimVal(BuiltIn(name)) => write!(f, "{}", name),
            PrimVal(Bool(val)) => write!(f, "{}", val),
            PrimVal(I32(val)) => write!(f, "{}", val),
            PrimVal(Str(val)) => write!(f, "'{}'", val),
            PrimVal(Tag(bits)) => {
                let mut bit_str = "".to_string();
                for b in bits.iter() {
                    bit_str.push(if *b { '1' } else { '0' });
                }
                write!(f, "Tag({})", bit_str)
            }
            PrimVal(BitStr(ptr_size)) => write!(f, "*<{}b>Code", ptr_size),
            Lambda(val) => write!(f, "{}", val),
            Struct(vals) => {
                write!(f, "(").unwrap();
                let mut is_first = true;
                for val in vals.iter() {
                    if !is_first {
                        write!(f, ", ").unwrap();
                    }
                    write!(f, "{} = {}", val.0, &val.1).unwrap();
                    is_first = false;
                }
                write!(f, ")")
            }
            Union(s) => {
                if s.is_empty() {
                    write!(f, "Void")
                } else {
                    write!(f, "Union(")?;
                    let mut first = true;
                    for sty in s {
                        if !first {
                            write!(f, ", ")?;
                        }
                        first = false;
                        write!(f, "{}", sty)?;
                    }
                    write!(f, ")")
                }
            }
            Product(s) => {
                if s.is_empty() {
                    write!(f, "()")
                } else {
                    write!(f, "Product(")?;
                    let mut first = true;
                    for sty in s {
                        if !first {
                            write!(f, ", ")?;
                        }
                        first = false;
                        write!(f, "{}", sty)?;
                    }
                    write!(f, ")")
                }
            }
            Pointer(ptr_size, t) => write!(f, "*<{}b>{}", ptr_size, t),
            Padded(size, t) => write!(f, "Pad<{}b>{}", size, t),
            Function {
                intros,
                results,
                arguments,
            } => {
                if !intros.is_empty() {
                    let ints: Vec<String> = intros
                        .iter()
                        .map(|(name, ty)| format!("{}: {}", name, ty))
                        .collect();
                    write!(f, "{}|-", ints.join("|-"))?;
                }
                write!(f, "{} -> {}", arguments, results)
            }
            App { inner, arguments } => write!(f, "({})({})", inner, arguments),
            WithRequirement(ty, effs) => write!(f, "{}+{}", ty, effs.join("+")),
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
        PrimVal(BitStr(_ptr_size)) => Err(TError::StaticPointerCardinality(Info::default())),
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
        x => panic!(format!("unhandled: card of {:#?}", x)),
    }
}

// Calculates the memory needed for a new instance in bits.
pub fn size(ty: &Val) -> Result<Offset, TError> {
    use Prim::*;
    use Val::*;
    match ty {
        PrimVal(Tag(bits)) => Ok(bits.len()),
        PrimVal(BitStr(ptr_size)) => Ok(*ptr_size),
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
        x => panic!(format!("unhandled: size of {:#?}", x)),
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

pub fn bits(mut n: Offset, len: Offset) -> BitString {
    let mut v: BitString = vec![false; len];
    for ind in 0..len {
        if n == 0 {
            break;
        }
        v[len - 1 - ind] = if n % 2 == 0 { false } else { true };
        n /= 2;
    }
    v
}

pub fn record(values: Layout) -> Result<Val, TError> {
    let mut layout = set![];
    let mut off = 0;
    for val in values {
        // Work out the padding here
        let size = size(&val)?;
        layout.insert(val.padded(off));
        off += size;
    }
    Ok(Product(layout))
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

    #[test]
    fn bits_zero_length() {
        assert_eq!(bits(0, 0), vec![]);
        assert_eq!(bits(1, 0), vec![]);
    }

    #[test]
    fn bits_one_length() {
        assert_eq!(bits(0, 1), vec![false]);
        assert_eq!(bits(1, 1), vec![true]);
        assert_eq!(bits(2, 1), vec![false]);
        assert_eq!(bits(3, 1), vec![true]);
    }

    #[test]
    fn bits_two_length() {
        assert_eq!(bits(0, 2), vec![false, false]);
        assert_eq!(bits(1, 2), vec![false, true]);
        assert_eq!(bits(2, 2), vec![true, false]);
        assert_eq!(bits(3, 2), vec![true, true]);
        assert_eq!(bits(4, 2), vec![false, false]);
    }

    #[test]
    fn bits_three_length() {
        assert_eq!(bits(0, 3), vec![false, false, false]);
        assert_eq!(bits(1, 3), vec![false, false, true]);
        assert_eq!(bits(2, 3), vec![false, true, false]);
        assert_eq!(bits(3, 3), vec![false, true, true]);
        assert_eq!(bits(4, 3), vec![true, false, false]);
        assert_eq!(bits(5, 3), vec![true, false, true]);
        assert_eq!(bits(6, 3), vec![true, true, false]);
        assert_eq!(bits(7, 3), vec![true, true, true]);
        assert_eq!(bits(8, 3), vec![false, false, false]);
    }

    #[test]
    fn void() {
        assert_eq!(card(&void_type()), Ok(0));
        assert_eq!(size(&void_type()), Ok(0));
    }
    #[test]
    fn unit() {
        assert_eq!(card(&unit_type()), Ok(1));
        assert_eq!(size(&unit_type()), Ok(0));
    }
    #[test]
    fn tag1_type() {
        assert_eq!(card(&tag(bits(1, 1))), Ok(1));
        assert_eq!(size(&tag(bits(1, 1))), Ok(1));
    }
    #[test]
    fn tag2_type() {
        assert_eq!(card(&tag(bits(0, 2))), Ok(1));
        assert_eq!(size(&tag(bits(0, 2))), Ok(2));
        assert_eq!(card(&tag(bits(1, 2))), Ok(1));
        assert_eq!(size(&tag(bits(1, 2))), Ok(2));
        assert_eq!(card(&tag(bits(2, 2))), Ok(1));
        assert_eq!(size(&tag(bits(2, 2))), Ok(2));
        assert_eq!(card(&tag(bits(3, 2))), Ok(1));
        assert_eq!(size(&tag(bits(3, 2))), Ok(2));
    }
    #[test]
    fn tag4_type() {
        assert_eq!(card(&tag(bits(4, 3))), Ok(1));
        assert_eq!(size(&tag(bits(4, 3))), Ok(3));
    }

    #[test]
    fn union_n_type() {
        let union2 = Union(set![unit_type(), unit_type()]);
        assert_eq!(card(&union2), Ok(1));
        assert_eq!(size(&union2), Ok(0));
        let union3 = Union(set![unit_type(), unit_type(), unit_type()]);
        assert_eq!(card(&union3), Ok(1));
        assert_eq!(size(&union3), Ok(0));
    }
    #[test]
    fn bit() {
        let bitt = bit_type();
        assert_eq!(card(&bitt), Ok(2));
        assert_eq!(size(&bitt), Ok(1));
    }
    #[test]
    fn trit() {
        let trit = trit_type();
        assert_eq!(card(&trit), Ok(3));
        assert_eq!(size(&trit), Ok(2));
    }
    #[test]
    fn nested_quad_type() {
        let quad = record(vec![bit_type(), bit_type()]).unwrap();
        assert_eq!(card(&quad), Ok(4));
        assert_eq!(size(&quad), Ok(2));
    }
    #[test]
    fn quad_type() {
        let quad = sum(vec![unit_type(), unit_type(), unit_type(), unit_type()]).unwrap();
        assert_eq!(card(&quad), Ok(4));
        assert_eq!(size(&quad), Ok(2));
    }
    #[test]
    fn pent_type() {
        let pent = sum(vec![
            unit_type(),
            unit_type(),
            unit_type(),
            unit_type(),
            unit_type(),
        ])
        .unwrap();
        assert_eq!(card(&pent), Ok(5));
        assert_eq!(size(&pent), Ok(3));
    }
    #[test]
    fn pair_bool_ptrs() {
        let bool_ptr = Pointer(64, Box::new(bit_type()));
        let quad = record(vec![bool_ptr.clone(), bool_ptr]).unwrap();
        assert_eq!(card(&quad), Ok(4));
        assert_eq!(size(&quad), Ok(2 * 64));
    }
    #[test]
    fn nested_nibble() {
        let quad = record(vec![bit_type(), bit_type()]).unwrap();
        let nibble = record(vec![quad.clone(), quad]).unwrap();
        assert_eq!(card(&nibble), Ok(16));
        assert_eq!(size(&nibble), Ok(4));
    }
    #[test]
    fn padded_nibble() {
        let quad = record(vec![bit_type().padded(2), bit_type()]).unwrap();
        let nibble = record(vec![quad.clone(), quad]).unwrap();
        assert_eq!(card(&nibble), Ok(16));
        assert_eq!(size(&nibble), Ok(8));
    }

    #[test]
    fn bool_and_fn() {
        let fn_ptr = PrimVal(Prim::BitStr(64));
        let closure = record(vec![bit_type(), fn_ptr]).unwrap();
        assert_eq!(size(&closure), Ok(65));
    }

    #[test]
    fn bool_or_fn() {
        let fn_ptr = PrimVal(Prim::BitStr(64));
        let closure = sum(vec![bit_type(), fn_ptr]).unwrap();
        assert_eq!(size(&closure), Ok(65));
    }
}
