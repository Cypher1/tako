use std::collections::HashSet;

// From https://riptutorial.com/rust/example/4149/create-a-hashset-macro
macro_rules! set {
    ( $( $x:expr ),* ) => {  // Match zero or more comma delimited items
        {
            let mut temp_set = HashSet::new();  // Create a mutable HashSet
            $(
                temp_set.insert($x); // Insert each item matched into the HashSet
            )*
            temp_set // Return the populated HashSet
        }
    };
}

fn merge<K: Eq + std::hash::Hash>(set1: HashSet<K>, set2: HashSet<K>) -> HashSet<K> {
    set1.into_iter().chain(set2).collect()
}

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

pub fn card(ty: Type) -> Result<i32, HashSet<String>> {
    use Type::*;
    match ty {
        Variable(s) => Err(set![s]),
        TypeOr(s) => s.iter().fold(Ok(0), |res, sty| {
            let t = sty.0.clone();
            match (res, card(*t)) {
                (Ok(res), Ok(c)) => Ok(res + c),
                (res, Ok(_)) => res,
                (Ok(_), res) => res,
                (Err(res), Err(c)) => Err(merge(res, c)),
            }
        }),
        TypeAnd(s) => s.iter().fold(Ok(1), |res, sty| {
            let t = sty.0.clone();
            match (res, card(*t)) {
                (Ok(res), Ok(c)) => Ok(res * c),
                (res, Ok(_)) => res,
                (Ok(_), res) => res,
                (Err(res), Err(c)) => Err(merge(res, c)),
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
    use std::collections::HashSet;

    #[test]
    fn cardinality_var_is_err() {
        let var = Variable("x".to_string());
        assert_eq!(card(var), Err(set!["x".to_string()]));
    }
    #[test]
    fn cardinality_x_or_y() {
        let var_x = Box::new(Variable("x".to_string()));
        let var_y = Box::new(Variable("y".to_string()));
        let or_var = TypeOr(vec![(var_x, 0), (var_y, 1)]);
        assert_eq!(card(or_var), Err(set![
                "x".to_string(),
                "y".to_string()]));
    }
    #[test]
    fn cardinality_y_and_x() {
        let var_x = Box::new(Variable("x".to_string()));
        let var_y = Box::new(Variable("y".to_string()));
        let or_var = TypeAnd(vec![(var_y, 0), (var_x, 1)]);
        assert_eq!(card(or_var), Err(set![
                "x".to_string(),
                "y".to_string()]));
    }
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
