use std::collections::BTreeSet;
use crate::errors::TError;
use crate::ast::Info;

// i32 here are sizes in bits, not bytes.
// This means that we don't need to have a separate systems for bit&byte layouts.
type Offset = i32;

// A list of types with an offset to get to the first bit (used for padding, frequently 0).
type Layout = BTreeSet<(Type, Offset)>;
type Pack = BTreeSet<(String, Type)>;

#[derive(PartialEq, Eq, Clone, PartialOrd, Ord, Debug, Hash)]
pub enum Type {
    Union(Layout),
    Struct(Layout),
    StaticPointer(Offset),
    Tag(Offset), // A locally unique id, should be replaced with a bit pattern at compile time.
    Pointer(Offset, Box<Type>), // Defaults to 8 bytes (64 bit)
    Variable(String),
    Function {
        results: Pack,
        intros: Pack,
        arguments: Pack,
        effects: Vec<String>,
    },
    Apply {
        inner: Box<Type>,
        arguments: Pack,
    },
}

use Type::*;

impl Type {
    fn ptr(self: Type) -> Type {
        Pointer(8 * byte_size(), Box::new(self))
    }
}

fn product(values: Vec<Type>) -> Result<Type, TError> {
    let mut layout = set![];
    let mut off = 0;
    for val in values {
        let val_size = size(&val)?;
        layout.insert((val, off));
        off += val_size;
    }
    Ok(Struct(layout))
}

fn sum(values: Vec<Type>) -> Result<Type, TError> {
    let mut layout = set![];
    let mut count = 0;
    for val in values {
        let mut tagged = Tag(count);
        if val != unit() {
            tagged = product(vec![tagged, val])?;
        }
        layout.insert((tagged, 0));
        count += 1;
    }
    Ok(Union(layout))
}

pub fn void() -> Type {
    Union(set![])
}

pub fn unit() -> Type {
    Struct(set![])
}

pub fn bit() -> Type {
    sum(vec![unit(), unit()]).expect("bit should be safe")
}

pub fn byte_type() -> Type {
    product(vec![
        bit(),
        bit(),
        bit(),
        bit(),
        bit(),
        bit(),
        bit(),
        bit(),
    ]).expect("byte should be safe")
}

pub fn byte_size() -> Offset {
    8
}

pub fn char_type() -> Type {
    sum(vec![byte_type(), byte_type(), byte_type(), byte_type()]).expect("char should be safe")
}

pub fn str_type() -> Type {
    char_type().ptr()
}

pub fn number_type() -> Type {
    product(vec![byte_type(), byte_type(), byte_type(), byte_type()]).expect("number should be safe")
}

pub fn simplify(ty: &Type) -> Type {
    use Type::*;
    match ty {
        Union(values) => {
            if values.is_empty() {
                return void();
            }
            if values.len() == 1 {
                return values
                    .iter()
                    .next()
                    .expect("Expected single element set to have an element")
                    .0.clone();
            }
            let mut vals = set![];
            for (ty, off) in values {
                vals.insert((simplify(ty), *off));
            }
            return Union(vals);
        }
        Struct(values) => {
            if values.is_empty() {
                return unit();
            }
            if values.len() == 1 {
                return values
                    .iter()
                    .next()
                    .expect("Expected single element set to have an element")
                    .0.clone();
            }
            let mut vals = set![];
            for (ty, off) in values {
                vals.insert((simplify(&ty), *off));
            }
            return Struct(vals);
        }
        Pointer(ptr_size, t) => Pointer(*ptr_size, Box::new(simplify(t))),
        Tag(tag) => Tag(*tag),
        StaticPointer(ptr_size) => StaticPointer(*ptr_size),
        x => panic!(format!("unhandled: simplfy {:#?}", x))
    }
}

pub fn card(ty: &Type) -> Result<Offset, TError> {
    use Type::*;
    match ty {
        Union(s) => {
            let mut sum = 0;
            for sty in s {
                sum += card(&sty.0)?;
            }
            Ok(sum)
        }
        Struct(s) => {
            let mut prod = 1;
            for sty in s {
                prod *= card(&sty.0)?;
            }
            Ok(prod)
        }
        Pointer(_ptr_size, t) => card(&t),
        Tag(_tag) => Ok(1),
        StaticPointer(_ptr_size) => Err(TError::StaticPointerCardinality(Info::default())),
        x => panic!(format!("unhandled: card of {:#?}", x))
    }
}

fn num_bits(n: usize) -> usize {
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

// Calculates the memory needed for a new instance in bits.
pub fn size(ty: &Type) -> Result<Offset, TError> {
    use Type::*;
    match ty {
        Union(s) => {
            let mut res = 0;
            for sty in s.iter() {
                // This includes padding in size.
                let c = sty.1 + size(&sty.0)?;
                if res <= c {
                    res = c;
                }
            }
            Ok(res)
        }
        Struct(s) => {
            let mut res = 0;
            for sty in s.iter() {
                let c = sty.1 + size(&sty.0)?;
                if res <= c {
                    res = c;
                }
            }
            Ok(res)
        }
        Pointer(ptr_size, _t) => Ok(*ptr_size),
        Tag(tag) => Ok(num_bits(1+(*tag as usize)) as Offset),
        StaticPointer(ptr_size) => Ok(*ptr_size),
        Variable(name) => Err(TError::UnknownSizeOfVariableType(name.clone(), Info::default())),
        x => panic!(format!("unhandled: size of {:#?}", x))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn void_type() {
        assert_eq!(card(&void()), Ok(0));
        assert_eq!(size(&void()), Ok(0));
    }
    #[test]
    fn unit_type() {
        assert_eq!(card(&unit()), Ok(1));
        assert_eq!(size(&unit()), Ok(0));
    }
    #[test]
    fn tag1_type() {
        assert_eq!(card(&Tag(1)), Ok(1));
        assert_eq!(size(&Tag(1)), Ok(1));
    }
    #[test]
    fn tag2_type() {
        assert_eq!(card(&Tag(2)), Ok(1));
        assert_eq!(size(&Tag(2)), Ok(2));
    }
    #[test]
    fn tag4_type() {
        assert_eq!(card(&Tag(4)), Ok(1));
        assert_eq!(size(&Tag(4)), Ok(3));
    }

    #[test]
    fn union2_type() {
        let union2 = Union(set![(unit(), 0), (unit(), 0)]);
        assert_eq!(card(&union2), Ok(1));
        assert_eq!(size(&union2), Ok(0));
    }
    #[test]
    fn bit_type() {
        let bitt = bit();
        eprintln!(">> {:?}", bitt);
        assert_eq!(card(&bitt), Ok(2));
        assert_eq!(size(&bitt), Ok(1));
    }
    #[test]
    fn union3_type() {
        let union3 = Union(set![(unit(), 0), (unit(), 0), (unit(), 0)]);
        assert_eq!(card(&union3), Ok(1));
        assert_eq!(size(&union3), Ok(0));
    }
    #[test]
    fn trit_type() {
        let trit = sum(vec![unit(), unit(), unit()]).unwrap();
        assert_eq!(card(&trit), Ok(3));
        assert_eq!(size(&trit), Ok(2));
    }
    #[test]
    fn nested_quad_type() {
        let quad = product(vec![bit(), bit()]).unwrap();
        assert_eq!(card(&quad), Ok(4));
        assert_eq!(size(&quad), Ok(2));
    }
    #[test]
    fn quad_type() {
        let quad = sum(vec![unit(), unit(), unit(), unit()]).unwrap();
        assert_eq!(card(&quad), Ok(4));
        assert_eq!(size(&quad), Ok(2));
    }
    #[test]
    fn pent_type() {
        let pent = sum(vec![unit(), unit(), unit(), unit(), unit()]).unwrap();
        assert_eq!(card(&pent), Ok(5));
        assert_eq!(size(&pent), Ok(3));
    }
    #[test]
    fn pair_bool_ptrs() {
        let bool_ptr = Pointer(64, Box::new(bit()));
        let quad = product(vec![bool_ptr.clone(), bool_ptr]).unwrap();
        assert_eq!(card(&quad), Ok(4));
        assert_eq!(size(&quad), Ok(2 * 64));
    }
    #[test]
    fn padded_nibble() {
        let quad = product(vec![bit(), bit()]).unwrap();
        let nibble = product(vec![quad.clone(), quad]).unwrap();
        assert_eq!(card(&nibble), Ok(16));
        assert_eq!(size(&nibble), Ok(4));
    }

    #[test]
    fn bool_and_fn() {
        let fn_ptr = StaticPointer(64);
        let closure = product(vec![bit(), fn_ptr]).unwrap();
        assert_eq!(size(&closure), Ok(65));
    }

    #[test]
    fn bool_or_fn() {
        let fn_ptr = StaticPointer(64);
        let closure = sum(vec![bit(), fn_ptr]).unwrap();
        assert_eq!(size(&closure), Ok(65));
    }
}
