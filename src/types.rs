use std::collections::HashMap;

// A list of types with an offset to get to the first bit (used for padding, frequently 0).
type Layout = Vec<(DataType, usize)>;

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub enum DataType {
    Union(Layout),
    Struct(Layout),
    Static,
    Pointer(usize, Box<DataType>),
}

type Pack = HashMap<String, Type>;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Type {
    Value(DataType),
    Variable(String),
    Function{results: Pack, intros: Pack, arguments: Pack, effects: Vec<String>},
    Apply{inner: DataType, arguments: Pack},
}

use DataType::*;

pub fn void() -> DataType {
    Union(vec![])
}

pub fn unit() -> DataType {
    Struct(vec![])
}

pub fn bit() -> DataType {
    Union(vec![(unit(), 0), (unit(), 0)])
}

pub fn byte_type() -> DataType {
    Struct(vec![
        (bit(), 0),
        (bit(), 1),
        (bit(), 2),
        (bit(), 3),
        (bit(), 4),
        (bit(), 5),
        (bit(), 6),
        (bit(), 7),
    ])
}

pub fn byte_size() -> usize {
    8
}

pub fn machine_ptr_size() -> usize {
    8 * byte_size()
}

pub fn char_type() -> DataType {
    Struct(vec![
        (byte_type(), 0),
        (byte_type(), 8),
        (byte_type(), 16),
        (byte_type(), 24),
    ])
}

pub fn str_type() -> DataType {
    Pointer(machine_ptr_size(), Box::new(char_type()))
}

pub fn number_type() -> DataType {
    Struct(vec![(byte_type(), 0),(byte_type(), 8),(byte_type(), 16),(byte_type(), 24)])
}

pub fn card(ty: &DataType) -> usize {
    use DataType::*;
    match ty {
        Union(s) => s.iter().fold(0, |res, sty| res + card(&sty.0)),
        Struct(s) => s.iter().fold(1, |res, sty| res * card(&sty.0)),
        Pointer(_ptr_size, t) => card(&t),
        Static => panic!("Functions shouldnt be treated as cardinality"),
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
pub fn size(ty: &DataType) -> usize {
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
            num_bits(s.len()) + res
        }
        Struct(s) => {
            let mut res = 0;
            for sty in s.iter() {
                res += sty.1 + size(&sty.0);
            }
            res
        }
        Pointer(ptr_size, _t) => *ptr_size,
        Static => 0,
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
    fn bit_type() {
        let bitt = bit();
        let boolt = Union(vec![(unit(), 0), (unit(), 0)]);
        assert_eq!(boolt, bitt);
        assert_eq!(card(&bitt), 2);
        assert_eq!(card(&boolt), 2);
    }
    #[test]
    fn trit_type() {
        let trit = Union(vec![(unit(), 0), (unit(), 0), (unit(), 0)]);
        assert_eq!(card(&trit), 3);
        assert_eq!(size(&trit), 2);
    }
    #[test]
    fn quad_type() {
        let quad = Struct(vec![((bit()), 0), (bit(), 0)]);
        assert_eq!(card(&quad), 4);
        assert_eq!(size(&quad), 2);
    }
    #[test]
    fn pent_type() {
        let pent = Union(vec![
            (unit(), 0),
            (unit(), 0),
            (unit(), 0),
            (unit(), 0),
            (unit(), 0),
        ]);
        assert_eq!(card(&pent), 5);
        assert_eq!(size(&pent), 3);
    }
    #[test]
    fn pair_bool_ptrs() {
        let bool_ptr = Pointer(64, Box::new(bit()));
        let quad = Struct(vec![(bool_ptr.clone(), 0), (bool_ptr, 0)]);
        assert_eq!(card(&quad), 4);
        assert_eq!(size(&quad), 2 * 64);
    }
    #[test]
    fn padded_nibble() {
        let quad = Struct(vec![(bit(), 0), (bit(), 0)]);
        let nibble = Struct(vec![(quad.clone(), 2), (quad, 2)]);
        assert_eq!(card(&nibble), 16);
        assert_eq!(size(&nibble), 8);
    }

    #[test]
    fn bool_and_fn() {
        let fn_ptr = Pointer(64, Box::new(Static));
        let closure = Struct(vec![(bit(), 7), (fn_ptr, 0)]);
        assert_eq!(size(&closure), 72);
    }
}
