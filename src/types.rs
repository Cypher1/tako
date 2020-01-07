// A list of types with an offset to get to the first bit (used for padding, frequently 0).
type Layout = Vec<(Box<DataType>, usize)>;

#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Clone)]
pub enum DataType {
    Union(Layout),
    Struct(Layout),
    Static,
    Pointer(usize, Box<DataType>),
}

pub fn card(ty: DataType) -> usize {
    use DataType::*;
    match ty {
        Union(s) => s.iter().fold(0, |res, sty| {
            let t = sty.0.clone();
            res + card(*t)
        }),
        Struct(s) => s.iter().fold(1, |res, sty| {
            let t = sty.0.clone();
            res * card(*t)
        }),
        Pointer(_ptr_size, t) => card(*t),
        Static => panic!("Functions shouldnt be treated as cardinality")
    }
}

fn num_bits(n: usize) -> usize {
    let mut k = 0;
    let mut p = 1;
    loop {
        if n <= p {
            return k;
        }
        k+=1;
        p*=2;
    }
}

// Calculates the memory needed for a new instance in bits.
pub fn size(ty: DataType) -> usize {
    use DataType::*;
    match ty {
        Union(s) => num_bits(s.len())+s.iter().fold(0, |res, sty| {
            let t = sty.0.clone();
            let offset = sty.1.clone();
            // This includes padding in size.
            let c = offset+size(*t);
            if res > c { res } else { c }
        }),
        Struct(s) => s.iter().fold(0, |res, sty| {
            let t = sty.0.clone();
            let offset = sty.1.clone();
            res + offset + size(*t)
        }),
        Pointer(ptr_size, _t) => ptr_size,
        Static => 0,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::DataType::*;

    #[test]
    fn cardinality_void() {
        let void = Union(vec![]);
        assert_eq!(card(void), 0);
    }
    #[test]
    fn cardinality_unit() {
        let unit = Struct(vec![]);
        assert_eq!(card(unit), 1);
    }
    #[test]
    fn cardinality_bool() {
        let unit = Struct(vec![]);
        let boolt = Union(vec![(Box::new(unit.clone()), 0), (Box::new(unit), 0)]);

        assert_eq!(card(boolt), 2);
    }
    #[test]
    fn cardinality_trit() {
        let unit = Struct(vec![]);
        let trit = Union(vec![
            (Box::new(unit.clone()), 0),
            (Box::new(unit.clone()), 0),
            (Box::new(unit), 0)
        ]);

        assert_eq!(card(trit), 3);
    }
    #[test]
    fn cardinality_quad() {
        let unit = Struct(vec![]);
        let boolt = Union(vec![(Box::new(unit.clone()), 0), (Box::new(unit), 0)]);
        let quad = Struct(vec![(Box::new(boolt.clone()), 0), (Box::new(boolt), 0)]);

        assert_eq!(card(quad), 4);
    }
    #[test]
    fn cardinality_pent() {
        let unit = Struct(vec![]);
        let trit = Union(vec![
            (Box::new(unit.clone()), 0),
            (Box::new(unit.clone()), 0),
            (Box::new(unit.clone()), 0),
            (Box::new(unit.clone()), 0),
            (Box::new(unit), 0)
        ]);

        assert_eq!(card(trit), 5);
    }
    #[test]
    fn cardinality_pair_bool_ptrs() {
        let unit = Struct(vec![]);
        let boolt = Union(vec![(Box::new(unit.clone()), 0), (Box::new(unit), 0)]);
        let bool_ptr = Pointer(64, Box::new(boolt.clone()));
        let quad = Struct(vec![(Box::new(bool_ptr.clone()), 0), (Box::new(bool_ptr), 0)]);

        assert_eq!(card(quad), 4);
    }

    #[test]
    fn size_void() {
        let void = Union(vec![]);
        assert_eq!(size(void), 0);
    }
    #[test]
    fn size_unit() {
        let unit = Struct(vec![]);
        assert_eq!(size(unit), 0);
    }
    #[test]
    fn size_bool() {
        let unit = Struct(vec![]);
        let boolt = Union(vec![(Box::new(unit.clone()), 0), (Box::new(unit), 0)]);

        assert_eq!(size(boolt), 1);
    }
    #[test]
    fn size_trit() {
        let unit = Struct(vec![]);
        let trit = Union(vec![
            (Box::new(unit.clone()), 0),
            (Box::new(unit.clone()), 0),
            (Box::new(unit), 0)
        ]);

        assert_eq!(size(trit), 2);
    }
    #[test]
    fn size_quad() {
        let unit = Struct(vec![]);
        let boolt = Union(vec![(Box::new(unit.clone()), 0), (Box::new(unit), 0)]);
        let quad = Struct(vec![(Box::new(boolt.clone()), 0), (Box::new(boolt), 0)]);

        assert_eq!(size(quad), 2);
    }
    #[test]
    fn size_pent() {
        let unit = Struct(vec![]);
        let trit = Union(vec![
            (Box::new(unit.clone()), 0),
            (Box::new(unit.clone()), 0),
            (Box::new(unit.clone()), 0),
            (Box::new(unit.clone()), 0),
            (Box::new(unit), 0)
        ]);

        assert_eq!(size(trit), 3);
    }
    #[test]
    fn size_pair_bool_ptrs32() {
        let unit = Struct(vec![]);
        let boolt = Union(vec![(Box::new(unit.clone()), 0), (Box::new(unit), 0)]);
        let bool_ptr = Pointer(32, Box::new(boolt.clone()));
        let quad = Struct(vec![(Box::new(bool_ptr.clone()), 0), (Box::new(bool_ptr), 0)]);

        assert_eq!(size(quad), 2*32);
    }
    #[test]
    fn size_pair_bool_ptrs64() {
        let unit = Struct(vec![]);
        let boolt = Union(vec![(Box::new(unit.clone()), 0), (Box::new(unit), 0)]);
        let bool_ptr = Pointer(64, Box::new(boolt.clone()));
        let quad = Struct(vec![(Box::new(bool_ptr.clone()), 0), (Box::new(bool_ptr), 0)]);

        assert_eq!(size(quad), 2*64);
    }

    #[test]
    fn size_padded_nibble() {
        let unit = Struct(vec![]);
        let boolt = Union(vec![(Box::new(unit.clone()), 0), (Box::new(unit), 0)]);
        let quad = Struct(vec![(Box::new(boolt.clone()), 0), (Box::new(boolt), 0)]);
        let nibble = Struct(vec![(Box::new(quad.clone()), 2), (Box::new(quad), 2)]);

        assert_eq!(size(nibble), 8);
    }

    #[test]
    fn size_bool_and_fn() {
        let unit = Struct(vec![]);
        let boolt = Union(vec![(Box::new(unit.clone()), 0), (Box::new(unit), 0)]);
        let fn_ptr = Pointer(64, Box::new(Static));
        let closure = Struct(vec![(Box::new(boolt), 7), (Box::new(fn_ptr), 0)]);

        assert_eq!(size(closure), 72);
    }
}
