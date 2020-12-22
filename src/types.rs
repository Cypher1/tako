use crate::ast::Info;
use crate::errors::TError;
use std::collections::BTreeSet;
use std::fmt;

// i32 here are sizes in bits, not bytes.
// This means that we don't need to have a separate systems for bit&byte layouts.
pub type Offset = i32;

// A list of types with an offset to get to the first bit (used for padding, frequently 0).
type Layout = Vec<Type>;
type TypeSet = BTreeSet<Type>;
type Pack = BTreeSet<(String, Type)>;

#[derive(PartialEq, Eq, Clone, PartialOrd, Ord, Debug, Hash)]
pub enum Type {
    Union(TypeSet),
    Product(TypeSet),
    StaticPointer(Offset),
    Padded(Offset, Box<Type>),
    Tag(Offset, Offset), // A locally unique id and the number of bits needed for it, should be replaced with a bit pattern at compile time.
    Pointer(Offset, Box<Type>), // Defaults to 8 bytes (64 bit)
    Function {
        intros: Pack,
        arguments: Box<Type>,
        results: Box<Type>,
    },
    Apply {
        inner: Box<Type>,
        arguments: Box<Type>,
    },
    // The following should be eliminated during lowering
    Record(Pack),
    WithEffect(Box<Type>, Vec<String>),
    Variable(String),
}

impl fmt::Display for Type {
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
        match self {
            Union(s) => {
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
            Product(s) => {
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
            Pointer(ptr_size, t) => write!(f, "*<{}b>{}", ptr_size, t),
            Tag(tag, bits) => write!(f, "Tag<{}b>{}", bits, tag),
            Padded(size, t) => write!(f, "Pad<{}b>{}", size, t),
            StaticPointer(ptr_size) => write!(f, "*<{}b>Code", ptr_size),
            Record(pack) => {
                write!(f, "Record(")?;
                for (field, ty) in pack {
                    write!(f, "{}: {}", field, ty)?;
                }
                write!(f, ")")
            }
            Variable(name) => write!(f, "{}", name),
            Function {
                intros,
                arguments,
                results,
            } => {
                write!(f, "(")?;
                for (field, ty) in intros {
                    write!(f, "{}: {}. ", field, ty)?;
                }
                write!(f, "{}): {}", *arguments, *results)
            }
            Apply { inner, arguments } => write!(f, "({})({})", *inner, *arguments),
            WithEffect(ty, effects) => write!(f, "{} & {}", *ty, effects.join(" & ")),
        }
    }
}

use Type::*;

impl Type {
    pub fn ptr(self: Type) -> Type {
        Pointer(8 * byte_size(), Box::new(self))
    }
    pub fn padded(self: Type, size: Offset) -> Type {
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
pub fn card(ty: &Type) -> Result<Offset, TError> {
    use Type::*;
    match ty {
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
        Tag(_tag, _bits) => Ok(1),
        Padded(_size, t) => card(&t),
        StaticPointer(_ptr_size) => Err(TError::StaticPointerCardinality(Info::default())),
        x => panic!(format!("unhandled: card of {:#?}", x)),
    }
}

// Calculates the memory needed for a new instance in bits.
pub fn size(ty: &Type) -> Result<Offset, TError> {
    use Type::*;
    match ty {
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
        Tag(_tag, bits) => Ok(*bits),
        StaticPointer(ptr_size) => Ok(*ptr_size),
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

pub fn record(values: Layout) -> Result<Type, TError> {
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

pub fn sum(values: Vec<Type>) -> Result<Type, TError> {
    let mut layout = set![];
    let tag_bits = num_bits(values.len() as Offset);
    for (count, val) in values.into_iter().enumerate() {
        let mut tagged = Tag(count as i32, tag_bits);
        if val != unit_type() {
            tagged = record(vec![tagged, val])?;
        }
        layout.insert(tagged);
    }
    Ok(Union(layout))
}

pub fn void_type() -> Type {
    Union(set![])
}

pub fn unit_type() -> Type {
    Product(set![])
}

pub fn bit_type() -> Type {
    sum(vec![unit_type(), unit_type()]).expect("bit should be safe")
}

pub fn byte_type() -> Type {
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

pub fn char_type() -> Type {
    byte_type()
}

pub fn string_type() -> Type {
    char_type().ptr()
}

pub fn i32_type() -> Type {
    record(vec![byte_type(), byte_type(), byte_type(), byte_type()]).expect("i32 should be safe")
}

pub fn number_type() -> Type {
    variable("Number")
}

pub fn type_type() -> Type {
    variable("Type")
}

pub fn variable(name: &str) -> Type {
    Variable(name.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

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
        assert_eq!(card(&Tag(1, 1)), Ok(1));
        assert_eq!(size(&Tag(1, 1)), Ok(1));
    }
    #[test]
    fn tag2_type() {
        assert_eq!(card(&Tag(0, 2)), Ok(1));
        assert_eq!(size(&Tag(0, 2)), Ok(2));
        assert_eq!(card(&Tag(1, 2)), Ok(1));
        assert_eq!(size(&Tag(1, 2)), Ok(2));
        assert_eq!(card(&Tag(2, 2)), Ok(1));
        assert_eq!(size(&Tag(2, 2)), Ok(2));
        assert_eq!(card(&Tag(3, 2)), Ok(1));
        assert_eq!(size(&Tag(3, 2)), Ok(2));
    }
    #[test]
    fn tag4_type() {
        assert_eq!(card(&Tag(4, 3)), Ok(1));
        assert_eq!(size(&Tag(4, 3)), Ok(3));
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
    fn trit_type() {
        let trit = sum(vec![unit_type(), unit_type(), unit_type()]).unwrap();
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
        let fn_ptr = StaticPointer(64);
        let closure = record(vec![bit_type(), fn_ptr]).unwrap();
        assert_eq!(size(&closure), Ok(65));
    }

    #[test]
    fn bool_or_fn() {
        let fn_ptr = StaticPointer(64);
        let closure = sum(vec![bit_type(), fn_ptr]).unwrap();
        assert_eq!(size(&closure), Ok(65));
    }
}
