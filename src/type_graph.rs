use crate::ast::*;
use crate::errors::TError;
use crate::primitives::Val;
use std::collections::HashMap;

type Id = i32; // TODO: Make this a vec of Id so it can be treated as a stack

#[derive(Default, Debug, Clone)]
pub struct TypeGraph {
    // Map from paths to Ids (to avoid mapping from Paths to other things)
    pub symbols: HashMap<Path, Id>,

    pub types: HashMap<Id, Val>,

    // A counter used for generating new type variables... (probably a bad idea)
    pub counter: Id,
}

impl TypeGraph {
    pub fn get_new_id(&mut self) -> Id {
        let curr = self.counter;
        self.counter += 1;
        curr
    }

    fn id_for_path(&mut self, path: &Path) -> Option<&Id> {
        self.symbols.get(path).clone()
    }

    pub fn get_id_for_path(&mut self, path: &Path) -> Id {
        let id = self.symbols.get(path).clone();
        if let Some(id) = id {
            id.clone()
        } else {
            let new = self.get_new_id();
            self.symbols.insert(path.clone(), new.clone());
            new
        }
    }

    pub fn get_type(&self, path: &Path) -> Option<Val> {
        let id = self.symbols.get(path); // TODO: Use get_id_for_path?
        if let Some(id) = id {
            self.types.get(&id).cloned()
        } else {
            None
        }
    }

    pub fn normalize(&mut self, v: Val) -> Result<Val, TError> {
        use crate::primitives::{void_type, Val::*};
        Ok(match v {
            // Lambda(_),
            // Function {
            // intros: Pack,
            // arguments: Box<Val>,
            // results: Box<Val>,
            // },
            // App {
            // inner: Box<Val>,
            // arguments: Box<Val>,
            // },
            Struct(tys) => {
                let mut voided = false;
                let mut new_tys = vec![];
                for (name, ty) in tys.iter() {
                    let ty = self.normalize(ty.clone())?;
                    if ty == void_type() {
                        voided = true;
                        break;
                    }
                    new_tys.push((name.clone(), ty));
                }
                if voided {
                    void_type()
                } else {
                    Struct(new_tys)
                }
            }
            Union(tys) => {
                let mut new_tys = set![];
                for ty in tys.iter().cloned() {
                    let ty = self.normalize(ty)?;
                    if ty != void_type() {
                        match ty {
                            Union(tys) => new_tys = new_tys.union(&tys).cloned().collect(),
                            ty => {
                                new_tys.insert(ty.clone());
                            }
                        }
                    }
                }
                if new_tys.len() == 1 {
                    new_tys
                        .iter()
                        .next()
                        .expect("set with len 1 should always have a value")
                        .clone()
                } else {
                    Union(new_tys)
                }
            }
            Product(tys) => {
                let mut new_tys = set![];
                let mut voided = false;
                for ty in tys.iter() {
                    let ty = self.normalize(ty.clone())?;
                    if ty == void_type() {
                        voided = true;
                        break;
                    }
                    // Check for void overlap
                    match ty {
                        Product(tys) => new_tys = new_tys.union(&tys).cloned().collect(),
                        _ => {
                            new_tys.insert(ty);
                        }
                    }
                }
                if voided {
                    void_type()
                } else if new_tys.len() == 1 {
                    new_tys
                        .iter()
                        .next()
                        .expect("set with len 1 should always have a value")
                        .clone()
                } else {
                    Product(new_tys)
                }
            }
            Padded(n, t) => match self.normalize(*t)? {
                Padded(k, u) => Padded(n + k, u),
                Union(tys) => self.normalize(Union(
                    tys.iter()
                        .map(|ty| Padded(n, Box::new(ty.clone())))
                        .collect()
                ))?,
                Product(tys) => self.normalize(Product(
                    tys.iter()
                        .map(|ty| Padded(n, Box::new(ty.clone())))
                        .collect()
                ))?,
                ty => Padded(n, Box::new(ty)),
            },
            // WithRequirement(Box<Val>, Vec<String>),
            // Variable(String),
            _ => v,
        })
    }

    pub fn unify(&mut self, from: &Val, to: &Val) -> Result<Val, TError> {
        let from = self.normalize(from.clone())?;
        let to = self.normalize(to.clone())?;
        let v = self.unify_impl(&from, &to)?;
        let v = self.normalize(v)?;
        use crate::primitives::void_type;
        if from == to && from != v {
            panic!("wtf\n{}\n{}\n{}", &from, &to, &v);
        }
        if v != void_type() {
            eprintln!("unified:\n     {}\nwith {}\n to {}", &from, &to, &v);
        } else {
            eprintln!("couldnt not unify:\n     {}\nwith {}", &from, &to);
        }
        Ok(v)
    }

    fn unify_impl(&mut self, from: &Val, to: &Val) -> Result<Val, TError> {
        use crate::primitives::{void_type, Val::*};
        Ok(match (from, to) {
            (Struct(s), Struct(t)) => {
                let mut names = set![];
                let mut s_tys = map![];
                for (name, ty) in s.iter() {
                    names.insert(name.clone());
                    s_tys.insert(name.clone(), ty.clone());
                }
                let mut t_tys = map![];
                for (name, ty) in t.iter() {
                    names.insert(name.clone());
                    t_tys.insert(name.clone(), ty.clone());
                }
                let mut vals = vec![];
                for name in names.iter() {
                    match (s_tys.get(name), t_tys.get(name)) {
                        (None, None) => panic!("name must be in type"),
                        (None, Some(t)) | (Some(t), None) => {
                            vals.push((name.clone(), t.clone()));
                        }
                        (Some(s), Some(t)) => {
                            vals.push((name.clone(), self.unify(&s, &t)?));
                        }
                    }
                }
                Struct(vals)
            }
            (t, Union(tys)) | (Union(tys), t) => {
                let mut new_tys = set![];
                for ty in tys.iter() {
                    let ty = self.unify(ty, &t)?;
                    if ty != void_type() {
                        new_tys.insert(ty);
                    }
                }
                if new_tys.len() == 1 {
                    new_tys
                        .iter()
                        .next()
                        .expect("set with len 1 should always have a value")
                        .clone()
                } else {
                    Union(new_tys)
                }
            }
            (Product(s), ty) | (ty, Product(s)) => {
                let mut new_tys = set![]; // union find it?
                for t in s.iter() {
                    let ty = self.unify(ty, &t)?;
                    if ty == void_type() {
                        return Ok(void_type());
                    }
                    match ty {
                        Product(tys) => {
                            new_tys = new_tys.union(&tys).cloned().collect();
                        }
                        ty => {
                            new_tys.insert(ty.clone());
                        }
                    }
                }
                if new_tys.len() == 1 {
                    new_tys
                        .iter()
                        .next()
                        .expect("set with len 1 should always have a value")
                        .clone()
                } else {
                    Product(new_tys)
                }
            }
            (Variable(s), Variable(t)) => {
                if s == t {
                    Variable(s.to_string())
                } else {
                    // set s to t?
                    // set t to s?
                    Product(set![Variable(s.to_string()), Variable(t.to_string())])
                }
            }
            (s, Variable(_t)) | (Variable(_t), s) => {
                // set t to s
                s.clone()
            }
            (Padded(k, u), Padded(j, v)) => {
                if k == j {
                    Padded(*k, Box::new(self.unify(u, v)?))
                } else if k > j {
                    Padded(*j, Box::new(self.unify(&Padded(k - j, u.clone()), v)?))
                } else {
                    Padded(*k, Box::new(self.unify(u, &Padded(j - k, v.clone()))?))
                }
            }
            (t, Padded(0, u)) | (Padded(0, u), t) => self.unify(t, u)?,
            (t, Padded(k, u)) | (Padded(k, u), t) => {
                // actually we need to check if these overlap...
                Product(set!(
                        t.clone(),
                        Padded(*k, u.clone())
                ))
            }
            (PrimVal(p), PrimVal(q)) => {
                if p == q {
                    PrimVal(p.clone())
                } else {
                    void_type()
                }
            }
            (PrimVal(_), _) | (_, PrimVal(_)) => void_type(),
            (Pointer(k, t1), Pointer(j, t2)) => {
                if k == j {
                    Pointer(*k, Box::new(self.unify(t1, t2)?))
                } else {
                    void_type()
                }
            }
            (_, Pointer(_, _)) | (Pointer(_, _), _) => void_type(),
            (_, Lambda(_)) | (Lambda(_), _) => void_type(), // TODO: Work out what this means
            _ => void_type(),
        })
    }

    pub fn is_assignable_to(&mut self, from: &Val, to: &Val) -> Result<bool, TError> {
        // To be assignable from and to must unify to the same type as
        // from (i.e. we can't need more from 'from' than it already providee).
        let from = self.normalize(from.clone())?;
        let to = self.normalize(to.clone())?;
        let unified = self.unify(&from, &to)?;
        eprintln!("{}. {}. -> {}", &from, &to, &unified);
        Ok(unified == from)
    }

    fn require_assignable_for_id(&mut self, id: &Id, ty: &Val) -> Result<(), TError> {
        // TODO: Merge with existing types
        let curr_ty = self.types.get(id).cloned();
        let m = if let Some(curr_ty) = curr_ty {
            self.unify(&curr_ty, ty)?
        } else {
            ty.clone()
        };

        // Check overlap?

        self.types.insert(id.clone(), m);
        Ok(())
    }

    pub fn require_assignable(&mut self, path: &Path, ty: &Val) -> Result<(), TError> {
        let id = self.get_id_for_path(path);
        self.require_assignable_for_id(&id, ty)
    }
}

#[cfg(test)]
mod tests {
    use super::TypeGraph;
    use crate::ast::{Path, Symbol::*};
    use crate::errors::TError;
    use crate::primitives::{bit_type, byte_type, string_type, i32_type, number_type, variable, void_type, Val::*, boolean, int32, string};

    fn assert_eqs<T: Eq + Clone + std::fmt::Display + std::fmt::Debug>(a: T, b: T) {
        assert_eq!(
            format!("{}", a.clone()),
            format!("{}", b.clone()),
            "non-equal string representations"
        );
        assert_eq!(a, b, "non-equal objects with same string representations");
    }

    fn test_path() -> Path {
        vec![
            Named("root".to_string(), None),
            Named("file".to_string(), Some("tk".to_string())),
            Named("main".to_string(), None),
        ]
    }

    #[test]
    fn adding_type_gets_a_new_id() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        tgb.require_assignable(&path, &i32_type())?;
        let id = tgb.id_for_path(&path);
        assert_ne!(id, None, "Should get a new Id for this path");
        Ok(())
    }

    #[test]
    fn adding_a_second_type_gets_the_same_id() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        tgb.require_assignable(&path, &i32_type())?;
        let id = tgb.id_for_path(&path).cloned();
        tgb.require_assignable(&path, &number_type())?;
        let id2 = tgb.id_for_path(&path).cloned();
        assert_eq!(id, id2, "Should get the same Id for this path");
        Ok(())
    }

    #[test]
    fn round_trips_type_single_i32() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        tgb.require_assignable(&path, &i32_type())?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eqs(ty, i32_type());

        Ok(())
    }

    #[test]
    fn unifies_equal_bools_in_types() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        tgb.require_assignable(&path, &boolean(true))?;
        tgb.require_assignable(&path, &boolean(true))?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eqs(ty, boolean(true));
        Ok(())
    }

    #[test]
    fn unifies_non_equal_bools_in_types_to_void() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        tgb.require_assignable(&path, &boolean(true))?;
        tgb.require_assignable(&path, &boolean(false))?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eqs(ty, void_type());
        Ok(())
    }

    #[test]
    fn unifies_equal_i32_in_types() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        tgb.require_assignable(&path, &int32(4))?;
        tgb.require_assignable(&path, &int32(4))?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eqs(ty, int32(4));
        Ok(())
    }

    #[test]
    fn unifies_non_equal_i32_in_types_to_void() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        tgb.require_assignable(&path, &int32(4))?;
        tgb.require_assignable(&path, &int32(5))?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eqs(ty, void_type());
        Ok(())
    }

    #[test]
    fn unifies_equal_strs_in_types_to_void() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        tgb.require_assignable(&path, &string("Woo"))?;
        tgb.require_assignable(&path, &string("Woo"))?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eqs(ty, string("Woo"));
        Ok(())
    }

    #[test]
    fn unifies_non_equal_str_in_types_to_void() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        tgb.require_assignable(&path, &string("Yes"))?;
        tgb.require_assignable(&path, &string("No"))?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eqs(ty, void_type());
        Ok(())
    }

    #[test]
    fn unifies_equal_padded_bools_in_types() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        let a = Padded(10, Box::new(boolean(true)));
        tgb.require_assignable(&path, &a)?;
        tgb.require_assignable(&path, &a)?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eqs(ty, a);
        Ok(())
    }

    #[test]
    fn unifies_equivalent_nonequal_padded_bools_in_types() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        let a = Padded(5, Box::new(Padded(5, Box::new(boolean(true)))));
        let b = Padded(10, Box::new(boolean(true)));
        tgb.require_assignable(&path, &a)?;
        tgb.require_assignable(&path, &b)?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eqs(ty, b);
        Ok(())
    }

    #[test]
    fn unifies_overlapping_padded_bools() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        let a = Padded(5, Box::new(Padded(5, Box::new(boolean(true)))));
        let b = Padded(11, Box::new(boolean(true)));
        tgb.require_assignable(&path, &a)?;
        tgb.require_assignable(&path, &b)?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eqs(ty, Product(set!(
            Padded(10, Box::new(boolean(true))),
            Padded(11, Box::new(boolean(true)))
        )));
        Ok(())
    }

    #[test]
    fn unifies_non_equivalent_padded_bools_in_types_to_void() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        let a = Padded(5, Box::new(Padded(6, Box::new(boolean(false)))));
        let b = Padded(11, Box::new(boolean(true)));
        tgb.require_assignable(&path, &a)?;
        tgb.require_assignable(&path, &b)?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eqs(ty, void_type());
        Ok(())
    }

    #[test]
    fn unifies_non_equal_i32_and_str_in_types_to_void() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        tgb.require_assignable(&path, &int32(4))?;
        tgb.require_assignable(&path, &string("No"))?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eqs(ty, void_type());
        Ok(())
    }

    #[test]
    fn unifies_equivalent_unions() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        let a = Union(set!(boolean(false), boolean(true)));
        let b = Union(set!(boolean(true), boolean(false)));
        tgb.require_assignable(&path, &a)?;
        tgb.require_assignable(&path, &b)?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eqs(ty, a);
        Ok(())
    }

    #[test]
    fn unifies_unions_with_values() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        let a = Union(set!(boolean(false)));
        let b = boolean(false);
        tgb.require_assignable(&path, &a)?;
        tgb.require_assignable(&path, &b)?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eqs(ty, b);
        Ok(())
    }

    #[test]
    fn unifies_overlapping_unions_to_overlap() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        let a = Union(set!(int32(3), int32(4), int32(5)));
        let b = Union(set!(int32(1), int32(2), int32(3)));
        tgb.require_assignable(&path, &a)?;
        tgb.require_assignable(&path, &b)?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eqs(ty, int32(3));
        Ok(())
    }

    #[test]
    fn unifies_unions_of_unions() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        let a = Union(set!(int32(3), Union(set![int32(4), int32(5)])));
        let b = Union(set!(Union(set![int32(1), int32(2)]), int32(3)));
        tgb.require_assignable(&path, &a)?;
        tgb.require_assignable(&path, &b)?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eqs(ty, int32(3));
        Ok(())
    }

    #[test]
    fn unifies_non_equivalent_unions_to_void() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        let a = Union(set!(int32(3)));
        let b = Union(set!(boolean(true), boolean(false)));
        tgb.require_assignable(&path, &a)?;
        tgb.require_assignable(&path, &b)?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eqs(ty, void_type());
        Ok(())
    }

    #[test]
    fn unifies_products_with_values() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        let a = Product(set!(boolean(false)));
        let b = boolean(false);
        tgb.require_assignable(&path, &a)?;
        tgb.require_assignable(&path, &b)?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eqs(ty, b);
        Ok(())
    }

    #[test]
    fn unifies_overlapping_products_to_overlap() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        let a = Product(set!(variable("a"), variable("b")));
        let b = Product(set!(variable("b"), variable("c")));
        let res = Product(set!(variable("a"), variable("b"), variable("c")));
        tgb.require_assignable(&path, &a)?;
        tgb.require_assignable(&path, &b)?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eqs(ty, res);
        Ok(())
    }

    #[test]
    fn unifies_products_of_products() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        let a = Product(set!(variable("a"), variable("b")));
        let b = Product(set!(
            Product(set!(variable("b"), variable("c"))),
            variable("c")
        ));
        let res = Product(set!(variable("a"), variable("b"), variable("c")));
        tgb.require_assignable(&path, &a)?;
        tgb.require_assignable(&path, &b)?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eqs(ty, res);
        Ok(())
    }

    #[test]
    fn unifies_unions_to_union_products() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        let a = Union(set!(variable("a"), variable("b")));
        let b = Union(set!(variable("c"), variable("d")));
        let res = Union(set!(
                Product(set!(variable("a"), variable("c"))),
                Product(set!(variable("a"), variable("d"))),
                Product(set!(variable("b"), variable("c"))),
                Product(set!(variable("b"), variable("d")))
        ));
        tgb.require_assignable(&path, &a)?;
        tgb.require_assignable(&path, &b)?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eqs(ty, res);
        Ok(())
    }

    #[test]
    fn unifies_non_equivalent_products_to_void() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        let a = Product(set!(int32(3)));
        let b = Product(set!(boolean(true), boolean(false)));
        tgb.require_assignable(&path, &a)?;
        tgb.require_assignable(&path, &b)?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eqs(ty, void_type());
        Ok(())
    }

    #[test]
    fn unifies_equivalent_structs() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        let a = rec!["left" => variable("a"), "right" => variable("b")];
        let b = rec!["left" => variable("a"), "right" => variable("b")];
        tgb.require_assignable(&path, &a)?;
        tgb.require_assignable(&path, &b)?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eqs(ty, a);
        Ok(())
    }

    #[test]
    fn unifies_structs() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        let a = rec!["left" => variable("a"), "right" => variable("b")];
        let b = rec!["left" => variable("c"), "right" => variable("b")];
        let res =
            rec!["left" => Product(set![variable("a"), variable("c")]), "right" => variable("b")];
        tgb.require_assignable(&path, &a)?;
        tgb.require_assignable(&path, &b)?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eqs(ty, res);
        Ok(())
    }

    #[test]
    fn unifies_products_of_structs() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        let a = Product(set![
            rec!["right" => variable("b")],
            rec!["left" => variable("a")]
        ]);
        let b = rec!["left" => variable("a"), "right" => variable("b")];
        tgb.require_assignable(&path, &a)?;
        tgb.require_assignable(&path, &b)?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eqs(ty, b);
        Ok(())
    }

    #[test]
    fn unifies_non_equivalent_structs_to_void() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        let a = rec!["left" => int32(1), "right" => variable("b")];
        let b = rec!["left" => int32(2), "right" => variable("b")];
        tgb.require_assignable(&path, &a)?;
        tgb.require_assignable(&path, &b)?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eqs(ty, void_type());
        Ok(())
    }

    #[test]
    fn assignment_to_equal_type_bit() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        assert!(tgb.is_assignable_to(&bit_type(), &bit_type())?);
        Ok(())
    }

    // #[test]
    fn assignment_to_equal_type_byte() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        assert!(tgb.is_assignable_to(&byte_type(), &byte_type())?);
        Ok(())
    }

    // #[test]
    fn assignment_to_equal_type_str() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        assert!(tgb.is_assignable_to(&string_type(), &string_type())?);
        Ok(())
    }

    // #[test]
    fn assignment_to_equal_type_i32() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        assert!(tgb.is_assignable_to(&i32_type(), &i32_type())?);
        Ok(())
    }

    // #[test]
    fn unifies_variables_in_types_ensuring_assignment() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        tgb.require_assignable(&path, &variable("a"))?;
        tgb.require_assignable(&path, &i32_type())?;

        let ty = tgb.get_type(&path).unwrap();
        assert!(tgb.is_assignable_to(&ty, &i32_type())?);
        assert!(tgb.is_assignable_to(&ty, &variable("a"))?);
        Ok(())
    }
}
