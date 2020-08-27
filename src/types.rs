use std::collections::BTreeSet;
use std::collections::HashMap;

// i32 here are sizes in bits, not bytes.
// This means that we don't need to have a separate systems for bit&byte layouts.
type Offset = i32;

// A list of types with an offset to get to the first bit (used for padding, frequently 0).
type Layout = BTreeSet<(DataType, Offset)>;

#[derive(PartialEq, Eq, Clone, PartialOrd, Ord, Debug, Hash)]
pub enum DataType {
    Union(Layout),
    Struct(Layout),
    StaticPointer(Offset),
    Tag(Offset), // A locally unique id, should be replaced with a bit pattern at compile time.
    Pointer(Offset, Box<DataType>), // Defaults to 8 bytes (64 bit)
}

impl DataType {
    fn ptr(self: DataType) -> DataType {
        Pointer(8 * byte_size(), Box::new(self))
    }
}

fn product(values: Vec<DataType>) -> DataType {
    let mut layout = set![];
    let mut off = 0;
    for val in values {
        let val_size = size(&val);
        layout.insert((val, off));
        off += val_size;
    }
    Struct(layout)
}

fn sum(values: Vec<DataType>) -> DataType {
    let mut layout = set![];
    let mut count = 0;
    for val in values {
        let mut tagged = Tag(count);
        if val != unit() {
            tagged = product(vec![tagged, val])
        }
        layout.insert((tagged, 0));
        count += 1;
    }
    Union(layout)
}

type Pack = HashMap<String, Type>;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Type {
    Value(DataType),
    Variable(String),
    Function {
        results: Pack,
        intros: Pack,
        arguments: Pack,
        effects: Vec<String>,
    },
    Apply {
        inner: DataType,
        arguments: Pack,
    },
}

use DataType::*;

pub fn void() -> DataType {
    Union(set![])
}

pub fn unit() -> DataType {
    Struct(set![])
}

pub fn bit() -> DataType {
    sum(vec![unit(), unit()])
}

pub fn byte_type() -> DataType {
    product(vec![
        bit(),
        bit(),
        bit(),
        bit(),
        bit(),
        bit(),
        bit(),
        bit(),
    ])
}

pub fn byte_size() -> Offset {
    8
}

pub fn char_type() -> DataType {
    sum(vec![byte_type(), byte_type(), byte_type(), byte_type()])
}

pub fn str_type() -> DataType {
    char_type().ptr()
}

pub fn number_type() -> DataType {
    product(vec![byte_type(), byte_type(), byte_type(), byte_type()])
}

pub fn simplify(ty: &DataType) -> DataType {
    use DataType::*;
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
    }
}

pub fn card(ty: &DataType) -> Offset {
    use DataType::*;
    match ty {
        Union(s) => s.iter().fold(0, |res, sty| res + card(&sty.0)),
        Struct(s) => s.iter().fold(1, |res, sty| res * card(&sty.0)),
        Pointer(_ptr_size, t) => card(&t),
        Tag(_tag) => 1,
        StaticPointer(_ptr_size) => {
            panic!("Pointers into static memory have effectively infinite cardinality")
        }
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
pub fn size(ty: &DataType) -> Offset {
    use DataType::*;
    match ty {
        Union(s) => {
            let mut res = 0;
            for sty in s.iter() {
                // This includes padding in size.
                let c = sty.1 + size(&sty.0);
                if res <= c {
                    res = c;
                }
            }
            res
        }
        Struct(s) => {
            let mut res = 0;
            for sty in s.iter() {
                let c = sty.1 + size(&sty.0);
                if res <= c {
                    res = c;
                }
            }
            res
        }
        Pointer(ptr_size, _t) => *ptr_size,
        Tag(tag) => num_bits(1+(*tag as usize)) as Offset,
        StaticPointer(ptr_size) => *ptr_size,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn void_type() {
        assert_eq!(card(&void()), 0);
        assert_eq!(size(&void()), 0);
    }
    #[test]
    fn unit_type() {
        assert_eq!(card(&unit()), 1);
        assert_eq!(size(&unit()), 0);
    }
    #[test]
    fn tag1_type() {
        assert_eq!(card(&Tag(1)), 1);
        assert_eq!(size(&Tag(1)), 1);
    }
    #[test]
    fn tag2_type() {
        assert_eq!(card(&Tag(2)), 1);
        assert_eq!(size(&Tag(2)), 2);
    }
    #[test]
    fn tag4_type() {
        assert_eq!(card(&Tag(4)), 1);
        assert_eq!(size(&Tag(4)), 3);
    }

    #[test]
    fn union2_type() {
        let union2 = Union(set![(unit(), 0), (unit(), 0)]);
        assert_eq!(card(&union2), 1);
        assert_eq!(size(&union2), 0);
    }
    #[test]
    fn bit_type() {
        let bitt = bit();
        eprintln!(">> {:?}", bitt);
        assert_eq!(card(&bitt), 2);
        assert_eq!(size(&bitt), 1);
    }
    #[test]
    fn union3_type() {
        let union3 = Union(set![(unit(), 0), (unit(), 0), (unit(), 0)]);
        assert_eq!(card(&union3), 1);
        assert_eq!(size(&union3), 0);
    }
    #[test]
    fn trit_type() {
        let trit = sum(vec![unit(), unit(), unit()]);
        assert_eq!(card(&trit), 3);
        assert_eq!(size(&trit), 2);
    }
    #[test]
    fn nested_quad_type() {
        let quad = product(vec![bit(), bit()]);
        assert_eq!(card(&quad), 4);
        assert_eq!(size(&quad), 2);
    }
    #[test]
    fn quad_type() {
        let quad = sum(vec![unit(), unit(), unit(), unit()]);
        assert_eq!(card(&quad), 4);
        assert_eq!(size(&quad), 2);
    }
    #[test]
    fn pent_type() {
        let pent = sum(vec![unit(), unit(), unit(), unit(), unit()]);
        assert_eq!(card(&pent), 5);
        assert_eq!(size(&pent), 3);
    }
    #[test]
    fn pair_bool_ptrs() {
        let bool_ptr = Pointer(64, Box::new(bit()));
        let quad = product(vec![bool_ptr.clone(), bool_ptr]);
        assert_eq!(card(&quad), 4);
        assert_eq!(size(&quad), 2 * 64);
    }
    #[test]
    fn padded_nibble() {
        let quad = product(vec![bit(), bit()]);
        let nibble = product(vec![quad.clone(), quad]);
        assert_eq!(card(&nibble), 16);
        assert_eq!(size(&nibble), 4);
    }

    #[test]
    fn bool_and_fn() {
        let fn_ptr = StaticPointer(64);
        let closure = product(vec![bit(), fn_ptr]);
        assert_eq!(size(&closure), 65);
    }

    #[test]
    fn bool_or_fn() {
        let fn_ptr = StaticPointer(64);
        let closure = sum(vec![bit(), fn_ptr]);
        assert_eq!(size(&closure), 65);
    }
}
