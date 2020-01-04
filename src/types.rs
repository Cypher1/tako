
type Layout = Vec<(Box<Type>, i32)>;

#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Clone)]
pub enum Type {
    Variable(String),
    TypeOr(Layout),
    TypeAnd(Layout),
    TypeImplication(Layout, Layout),
}

pub fn card(ty: Type) -> Result<i32, Vec<String>> {
    use Type::*;
    match ty {
        Variable(s) => Err(vec![s]),
        TypeOr(s) => s.iter().fold(Ok(0), |res, sty| {
            let t = sty.0.clone();
            match (res, card(*t)) {
                (Ok(res), Ok(c)) => Ok(res + c),
                (res, Ok(_)) => res,
                (Ok(_), res) => res,
                (Err(res), Err(c)) => Err([&res[..], &c[..]].concat()
),
            }
        }),
        TypeAnd(s) => s.iter().fold(Ok(1), |res, sty| {
            let t = sty.0.clone();
            match (res, card(*t)) {
                (Ok(res), Ok(c)) => Ok(res * c),
                (res, Ok(_)) => res,
                (Ok(_), res) => res,
                (Err(res), Err(c)) => Err([&res[..], &c[..]].concat()
),
            }
        }),
        TypeImplication(a, s) => unimplemented!(),
    }
}

pub fn card_to_bin(ty: Type) -> i32 {
    unimplemented!();
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::Type::*;

    #[test]
    fn cardinality_void_is_0() {
        let void = TypeOr(vec![]);
        assert_eq!(card(void), Ok(0));
    }
    #[test]
    fn cardinality_unit_is_1() {
        let unit = TypeAnd(vec![]);
        assert_eq!(card(unit), Ok(1));
    }
    #[test]
    fn cardinality_bool_is_2() {
        let unit = TypeAnd(vec![]);
        let boolt = TypeOr(vec![(Box::new(unit.clone()), 0), (Box::new(unit), 1)]);

        assert_eq!(card(boolt), Ok(2));
    }
    #[test]
    fn cardinality_trit_is_3() {
        let unit = TypeAnd(vec![]);
        let trit = TypeOr(vec![
            (Box::new(unit.clone()), 0),
            (Box::new(unit.clone()), 1),
            (Box::new(unit), 2)
        ]);

        assert_eq!(card(trit), Ok(3));
    }
    #[test]
    fn cardinality_nibble_is_4() {
        let unit = TypeAnd(vec![]);
        let boolt = TypeOr(vec![(Box::new(unit.clone()), 0), (Box::new(unit), 1)]);
        let nibble = TypeAnd(vec![(Box::new(boolt.clone()), 0), (Box::new(boolt), 1)]);

        assert_eq!(card(nibble), Ok(4));
    }
}
