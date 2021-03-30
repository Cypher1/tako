use crate::ast::*;
use crate::errors::TError;
use crate::primitives::Prim;
use std::collections::HashMap;

type Id = i32; // TODO: Make this a vec of Id so it can be treated as a stack

#[derive(Default, Debug, Clone)]
pub struct TypeGraph {
    // Map from paths to Ids (to avoid mapping from Paths to other things)
    pub symbols: HashMap<Path, Id>,

    pub types: HashMap<Id, Prim>,

    // A counter used for generating new type variables... (probably a bad idea)
    pub counter: Id,
}

impl TypeGraph {
    fn get_new_id(&mut self) -> Id {
        let curr = self.counter;
        self.counter +=1;
        curr
    }

    fn id_for_path(&mut self, path: &Path) -> Option<&Id> {
        self.symbols.get(path).clone()
    }

    fn get_id_for_path(&mut self, path: &Path) -> Id {
        let id = self.symbols.get(path).clone();
        if let Some(id) = id {
            id.clone()
        } else {
            let new = self.get_new_id();
            self.symbols.insert(path.clone(), new.clone());
            new
        }
    }

    fn get_type(&self, path: &Path) -> Option<Prim> {
        let id = self.symbols.get(path); // TODO: Use get_id_for_path?
        if let Some(id) = id {
            self.types.get(&id).cloned()
        } else {
            None
        }
    }

    fn unify(&mut self, l: &Prim, r: &Prim) -> Result<Prim, TError> {
        use crate::primitives::{Prim::*, void_type};
        Ok(match (l, r) {
            (Struct(s), Struct(t)) => {
                let mut tys = map![];
                for (name, ty) in s.iter() {
                    tys.insert(name.clone(), ty.clone());
                }
                for (name, ty_new) in t.iter() {
                    let ty = if let Some(ty) = tys.get(name) {
                        self.unify(&ty, ty_new)?
                    } else {
                        ty_new.clone()
                    };
                    tys.insert(name.clone(), ty);
                }
                let mut vals = vec![];
                for (name, ty) in tys.iter() {
                    vals.push((name.clone(), ty.clone()));
                }
                Struct(vals)
            },
            (t, Union(tys)) | (Union(tys), t) => {
                let mut new_tys = set![];
                for ty in tys.iter() {
                    let ty = self.unify(ty, &t)?;
                    new_tys.insert(ty);
                }
                Union(new_tys)
            },
            (t, Product(tys)) | (Product(tys), t) => {
                let mut new_tys = set![];
                for ty in tys.iter() {
                    let ty = self.unify(ty, &t)?;
                    new_tys.insert(ty);
                }
                Product(new_tys)
            },
            (Padded(k, t1), Padded(j, t2)) => {
                if k == j {
                    Padded(*k, Box::new(self.unify(t1, t2)?))
                } else {
                    void_type()
                }
            },
            (_, Padded(_, _)) | (Padded(_, _), _) => void_type(),
            (Tag(k, u), Tag(j, v)) => {
                if k == j && u == v {
                    Tag(*k, *u)
                } else {
                    void_type()
                }
            },
            (_, Tag(_, _)) | (Tag(_, _), _) => void_type(),
            (Pointer(k, t1), Pointer(j, t2)) => {
                if k == j {
                    Pointer(*k, Box::new(self.unify(t1, t2)?))
                } else {
                    void_type()
                }
            },
            (_, Pointer(_, _)) | (Pointer(_, _), _) => void_type(),

            (Bool(b1), Bool(b2)) => if b1 == b2 { Bool(*b1) } else { void_type() },
            (Bool(_), _) | (_, Bool(_)) => void_type(),
            (I32(i1), I32(i2)) => if i1 == i2 { I32(*i1) } else { void_type() },
            (I32(_), _) | (_, I32(_)) => void_type(),
            (Str(s1), Str(s2)) => if s1 == s2 { Str(s1.clone()) } else { void_type() },
            (Str(_), _) | (_, Str(_)) => void_type(),
            (BuiltIn(b1), BuiltIn(b2)) => if b1 == b2 { BuiltIn(b1.clone()) } else { void_type() },
            (BuiltIn(_), _) | (_, BuiltIn(_)) => void_type(),
            (StaticPointer(p1), StaticPointer(p2)) => if p1 == p2 { StaticPointer(*p1) } else { void_type() },
            (StaticPointer(_), _) | (_, StaticPointer(_)) => void_type(),
            (_, Lambda(_)) | (Lambda(_), _) => void_type(), // TODO: Work out what this means
            _ => void_type()
        })
    }

    fn restrict_type_for_id(&mut self, id: &Id, ty: &Prim) -> Result<(), TError> {
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

    pub fn restrict_type(&mut self, path: &Path, ty: &Prim) -> Result<(), TError> {
        let id = self.get_id_for_path(path);
        self.restrict_type_for_id(&id, ty)
    }
}

#[cfg(test)]
mod tests {
    use super::TypeGraph;
    use crate::ast::{Path, Symbol::*};
    use crate::primitives::{i32_type, variable, number_type, void_type, Prim::*};
    use crate::errors::TError;

    fn assert_eqs<T: Eq + Clone + std::fmt::Display + std::fmt::Debug>(l: T, r: T) {
        assert_eq!(format!("{}", l.clone()), format!("{}", r.clone()), "non-equal string representations");
        assert_eq!(l, r, "non-equal objects with same string representations");
    }

    fn test_path() -> Path {
        vec![
            Named("root".to_string(), None),
            Named("file".to_string(), Some("tk".to_string())),
            Named("main".to_string(), None)
        ]
    }

    #[test]
    fn adding_type_gets_a_new_id() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        tgb.restrict_type(&path, &i32_type())?;
        let id = tgb.id_for_path(&path);
        assert_ne!(id, None, "Should get a new Id for this path");
        Ok(())
    }

    #[test]
    fn adding_a_second_type_gets_the_same_id() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        tgb.restrict_type(&path, &i32_type())?;
        let id = tgb.id_for_path(&path).cloned();
        tgb.restrict_type(&path, &number_type())?;
        let id2 = tgb.id_for_path(&path).cloned();
        assert_eq!(id, id2, "Should get the same Id for this path");
        Ok(())
    }

    #[test]
    fn round_trips_type_single_i32() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        tgb.restrict_type(&path, &i32_type())?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eqs(ty, i32_type());

        Ok(())
    }

    #[test]
    fn merges_types_for_same_path() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        tgb.restrict_type(&path, &i32_type())?;
        tgb.restrict_type(&path, &number_type())?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eqs(ty, Product(set![i32_type(), number_type()]));
        Ok(())
    }

    #[test]
    fn unifies_equal_bools_in_types() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        tgb.restrict_type(&path, &Bool(true))?;
        tgb.restrict_type(&path, &Bool(true))?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eqs(ty, Bool(true));
        Ok(())
    }

    #[test]
    fn unifies_non_equal_bools_in_types_to_void() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        tgb.restrict_type(&path, &Bool(true))?;
        tgb.restrict_type(&path, &Bool(false))?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eqs(ty, void_type());
        Ok(())
    }

    #[test]
    fn unifies_equal_i32_in_types() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        tgb.restrict_type(&path, &I32(4))?;
        tgb.restrict_type(&path, &I32(4))?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eqs(ty, I32(4));
        Ok(())
    }

    #[test]
    fn unifies_non_equal_i32_in_types_to_void() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        tgb.restrict_type(&path, &I32(4))?;
        tgb.restrict_type(&path, &I32(5))?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eqs(ty, void_type());
        Ok(())
    }

    #[test]
    fn unifies_equal_strs_in_types_to_void() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        tgb.restrict_type(&path, &Str("Woo".to_string()))?;
        tgb.restrict_type(&path, &Str("Woo".to_string()))?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eqs(ty, Str("Woo".to_string()));
        Ok(())
    }

    #[test]
    fn unifies_non_equal_str_in_types_to_void() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        tgb.restrict_type(&path, &Str("Yes".to_string()))?;
        tgb.restrict_type(&path, &Str("No".to_string()))?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eqs(ty, void_type());
        Ok(())
    }

    #[test]
    fn unifies_non_equal_i32_and_str_in_types_to_void() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        tgb.restrict_type(&path, &I32(4))?;
        tgb.restrict_type(&path, &Str("No".to_string()))?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eqs(ty, void_type());
        Ok(())
    }

    #[test]
    fn unifies_variables_in_types() -> Result<(), TError> {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        tgb.restrict_type(&path, &variable("a"))?;
        tgb.restrict_type(&path, &i32_type())?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eqs(ty, i32_type());
        let var_ty_a = tgb.get_type(&path).unwrap();
        assert_eqs(var_ty_a, i32_type());
        Ok(())
    }
}
