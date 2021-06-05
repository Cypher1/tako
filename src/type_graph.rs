use crate::ast::*;
use crate::errors::TError;
use crate::primitives::{TypeSet, Val};
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

fn reduce_common_padding(tys: TypeSet, builder: fn(TypeSet) -> Val) -> Val {
    use crate::primitives::Val::Padded;
    let mut common_padding = None;
    for ty in tys.iter() {
        common_padding = Some(if let Padded(k, _ty) = ty {
            std::cmp::min(*k, common_padding.unwrap_or(*k))
        } else {
            0
        });
    }
    let common_padding = common_padding.unwrap_or(0);
    if common_padding > 0 {
        let mut unpadded_tys = set![];
        for ty in tys.iter().cloned() {
            if let Padded(k, ty) = ty {
                let k = k - common_padding;
                unpadded_tys.insert((*ty).padded(k));
            }
        }
        return builder(unpadded_tys).padded(common_padding);
    }
    builder(tys)
}

fn only_item_or_build(vals: TypeSet, builder: fn(TypeSet) -> Val) -> Val {
    if vals.is_empty() {
        builder(vals) // skip reductions automatically.
    } else if vals.len() == 1 {
        vals.iter()
            .next()
            .expect("set with len 1 should always have a value")
            .clone()
    } else {
        reduce_common_padding(vals, builder)
    }
}

fn factor_out(val: Val, reduction: &Val) -> Val {
    use crate::primitives::Val::*;
    let factor_tys = |tys: TypeSet, builder: fn(TypeSet) -> Val| {
        let mut new_tys = set![];
        for ty in tys.iter().cloned() {
            new_tys.insert(factor_out(ty, reduction));
        }
        new_tys.remove(reduction);
        only_item_or_build(new_tys, builder)
    };
    match val {
        Product(tys) => factor_tys(tys, Product),
        Union(tys) => factor_tys(tys, Union),
        Padded(k, ty) => match reduction {
            Padded(j, reduction) => {
                if k <= *j {
                    factor_out(*ty, &reduction.clone().padded(j - k))
                } else {
                    *ty
                }
            }
            _ => *ty,
        }
        .padded(k),
        t => match reduction {
            Product(reductions) => {
                let mut t = t;
                for reduction in reductions.iter() {
                    t = factor_out(t, reduction);
                }
                t
            }
            _ => t,
        },
    }
}

fn cancel_neighbours(mut tys: TypeSet) -> TypeSet {
    for ty in tys.clone().iter() {
        let mut simpl_tys = set![];
        for other in tys.iter().cloned() {
            simpl_tys.insert(factor_out(other, &ty));
        }
        tys = simpl_tys;
    }
    tys
}

impl TypeGraph {
    pub fn get_new_id(&mut self) -> Id {
        let curr = self.counter;
        self.counter += 1;
        curr
    }

    pub fn get_id_for_path(&mut self, path: &Path) -> Id {
        let id = self.symbols.get(path);
        if let Some(id) = id {
            *id
        } else {
            let new = self.get_new_id();
            self.symbols.insert(path.clone(), new);
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

    pub fn normalize(&mut self, value: Val) -> Result<Val, TError> {
        use crate::primitives::{void_type, Val::*};
        Ok(match value {
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
                let mut new_tys = vec![];
                for (name, ty) in tys.iter() {
                    let ty = self.normalize(ty.clone())?;
                    if ty == void_type() {
                        return Ok(void_type());
                    }
                    new_tys.push((name.clone(), ty));
                }
                Struct(new_tys)
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
                // Cancel all the neighbours (e.g. (a*b)|(c*b) => (a|c)*b
                only_item_or_build(cancel_neighbours(new_tys), Union)
            }
            Product(tys) => {
                let mut new_tys = set![];
                for ty in tys.iter() {
                    let ty = self.normalize(ty.clone())?;
                    if ty == void_type() {
                        return Ok(void_type());
                    }
                    // Check for void overlap
                    match ty {
                        Product(tys) => new_tys = new_tys.union(&tys).cloned().collect(),
                        _ => {
                            new_tys.insert(ty);
                        }
                    }
                }
                // Cancel all the neighbours (e.g. (a|b)*b => (a*b)|b
                only_item_or_build(cancel_neighbours(new_tys), Product)
            }
            Padded(n, inner) => match self.normalize(*inner)? {
                Padded(k, inner) => inner.padded(n + k),
                Union(tys) => {
                    self.normalize(Union(tys.iter().map(|ty| ty.clone().padded(n)).collect()))?
                }
                Product(tys) => {
                    self.normalize(Product(tys.iter().map(|ty| ty.clone().padded(n)).collect()))?
                }
                ty => ty.padded(n),
            },
            // WithRequirement(Box<Val>, Vec<String>),
            // Variable(String),
            _ => value,
        })
    }

    pub fn unify(&mut self, from: &Val, to: &Val) -> Result<Val, TError> {
        let from = self.normalize(from.clone())?;
        let to = self.normalize(to.clone())?;
        let v = self.unify_impl(&from, &to)?;
        let v = self.normalize(v)?;
        // use crate::primitives::void_type;
        if from == to && from != v {
            panic!("wtf\n{}\n{}\n{:#?}", &from, &to, &v);
        }
        // if v != void_type() {
        // eprintln!("unified:\n     {}\nwith {}\n to {}", &from, &to, &v);
        // } else {
        // eprintln!("couldnt not unify:\n     {}\nwith {}", &from, &to);
        // }
        Ok(v)
    }

    fn unify_impl(&mut self, from: &Val, to: &Val) -> Result<Val, TError> {
        use crate::primitives::{void_type, Val::*, Prim::Tag};
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
                only_item_or_build(new_tys, Union)
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
                only_item_or_build(new_tys, Product)
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
                let min_pad = std::cmp::min(*k, *j);
                let max_pad = std::cmp::max(*k, *j);
                self.unify(
                    &u.clone().padded(max_pad - j),
                    &v.clone().padded(max_pad - k),
                )?
                .padded(min_pad)
            }
            (t, Padded(k, u)) | (Padded(k, u), t) => {
                // actually we need to check if these overlap...
                only_item_or_build(set![t.clone(), Padded(*k, u.clone())], Product)
            }
            (PrimVal(Tag(p)), PrimVal(Tag(q))) => {
                let (shorter, longer) = if p.len() < q.len() { (p, q) } else { (q, p) };
                if longer[0..shorter.len()] == shorter[0..] {
                    PrimVal(Tag(longer.clone()))
                } else {
                    void_type()
                }
            }
            (PrimVal(p), q) | (q, PrimVal(p)) => {
                if PrimVal(p.clone()) == *q {
                    PrimVal(p.clone())
                } else {
                    void_type()
                }
            }
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
        self.types.insert(*id, m);
        Ok(())
    }

    pub fn require_assignable(&mut self, path: &Path, ty: &Val) -> Result<(), TError> {
        let id = self.get_id_for_path(path);
        self.require_assignable_for_id(&id, ty)
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use super::{Id, TypeGraph};
    use crate::ast::{Path, Symbol::*};
    use crate::errors::TError;
    use crate::primitives::{
        bit_type, boolean, byte_type, i32_type, int32, number_type, quad_type, record, string,
        string_type, sum, trit_type, variable, void_type, Val::*,
    };

    impl TypeGraph {
        fn id_for_path(&mut self, path: &Path) -> Option<&Id> {
            self.symbols.get(path).clone()
        }
    }

    fn test_path() -> Path {
        vec![
            Named("root".to_string(), None),
            Named("file".to_string(), Some("tk".to_string())),
            Named("main".to_string(), None),
        ]
    }

    type Test = Result<(), TError>;

    #[test]
    fn adding_type_gets_a_new_id() -> Test {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        tgb.require_assignable(&path, &i32_type())?;
        let id = tgb.id_for_path(&path);
        assert_ne!(id, None, "Should get a new Id for this path");
        Ok(())
    }

    #[test]
    fn adding_a_second_type_gets_the_same_id() -> Test {
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
    fn round_trips_type_single_i32() -> Test {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        tgb.require_assignable(&path, &i32_type())?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eq!(ty, i32_type());

        Ok(())
    }

    #[test]
    fn normalize_type_bit_to_type_bit() -> Test {
        let mut tgb = TypeGraph::default();
        assert_eq!(tgb.normalize(bit_type())?, tgb.normalize(bit_type())?);
        assert_eq!(tgb.normalize(bit_type())?, bit_type());
        Ok(())
    }

    #[test]
    fn normalize_type_trit_to_type_trit() -> Test {
        let mut tgb = TypeGraph::default();
        assert_eq!(tgb.normalize(trit_type())?, trit_type());
        Ok(())
    }

    #[test]
    fn normalize_type_pair_bit_to_pair_bit() -> Test {
        let pair_bit = || record(vec![bit_type(), bit_type()]).unwrap();
        let mut tgb = TypeGraph::default();
        assert_eq!(tgb.normalize(pair_bit())?, pair_bit());
        Ok(())
    }

    #[test]
    fn normalize_type_byte_to_type_byte() -> Test {
        let mut tgb = TypeGraph::default();
        assert_eq!(tgb.normalize(byte_type())?, tgb.normalize(byte_type())?);
        assert_eq!(tgb.normalize(byte_type())?, byte_type());
        Ok(())
    }

    #[test]
    fn normalize_type_bit_trit_to_bit_trit() -> Test {
        let bit_trit = || record(vec![bit_type(), trit_type()]).unwrap();
        let mut tgb = TypeGraph::default();
        assert_eq!(tgb.normalize(bit_trit())?, bit_trit());
        Ok(())
    }

    #[test]
    fn normalize_type_trit_bit_to_trit_bit() -> Test {
        let trit_bit = || record(vec![bit_type(), trit_type()]).unwrap();
        let mut tgb = TypeGraph::default();
        assert_eq!(tgb.normalize(trit_bit())?, trit_bit());
        Ok(())
    }

    #[test]
    fn normalize_type_trit_xor_bit_to_trit_xor_bit() -> Test {
        let trit_xor_bit = || sum(vec![bit_type(), trit_type()]).unwrap();
        let mut tgb = TypeGraph::default();
        assert_eq!(tgb.normalize(trit_xor_bit())?, trit_xor_bit());
        Ok(())
    }

    // #[test]
    fn normalize_type_trit_or_bit_to_simple_trit() -> Test {
        let trit_or_bit = || Union(set![bit_type(), trit_type()]);
        let mut tgb = TypeGraph::default();
        assert_eq!(tgb.normalize(trit_or_bit())?, trit_type());
        Ok(())
    }

    // #[test]
    fn normalize_type_trit_or_quad_to_simple_quad() -> Test {
        let trit_or_quad = || Union(set![quad_type(), trit_type()]);
        let mut tgb = TypeGraph::default();
        assert_eq!(tgb.normalize(trit_or_quad())?, quad_type());
        Ok(())
    }

    #[test]
    fn normalize_type_quad_to_quad() -> Test {
        let mut tgb = TypeGraph::default();
        assert_eq!(tgb.normalize(quad_type())?, quad_type());
        Ok(())
    }

    #[test]
    fn normalize_type_nested_quad_to_quad() -> Test {
        let nested_quad = record(vec![bit_type(), bit_type()]).unwrap();
        let mut tgb = TypeGraph::default();
        assert_eq!(tgb.normalize(nested_quad)?, quad_type());
        Ok(())
    }

    #[test]
    fn unifies_equal_bools_in_types() -> Test {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        tgb.require_assignable(&path, &boolean(true))?;
        tgb.require_assignable(&path, &boolean(true))?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eq!(ty, boolean(true));
        Ok(())
    }

    #[test]
    fn unifies_non_equal_bools_in_types_to_void() -> Test {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        tgb.require_assignable(&path, &boolean(true))?;
        tgb.require_assignable(&path, &boolean(false))?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eq!(ty, void_type());
        Ok(())
    }

    #[test]
    fn unifies_tags_to_shared() -> Test {
        use crate::primitives::{tag, bits};
        let mut tgb = TypeGraph::default();
        let path = test_path();
        // value must start with b11
        tgb.require_assignable(&path, &tag(bits(3, 2)))?;
        // value must start with b11
        tgb.require_assignable(&path, &tag(bits(3, 1)))?;

        let ty = tgb.get_type(&path).unwrap();
        // therefore value must start with b11
        assert_eq!(ty, tag(bits(3, 2)));
        Ok(())
    }

    #[test]
    fn unifies_equal_tags_to_tag() -> Test {
        use crate::primitives::{tag, bits};
        let mut tgb = TypeGraph::default();
        let path = test_path();
        // value must start with b11
        tgb.require_assignable(&path, &tag(bits(3, 2)))?;
        // value must start with b11
        tgb.require_assignable(&path, &tag(bits(3, 2)))?;

        let ty = tgb.get_type(&path).unwrap();
        // therefore value must start with b11
        assert_eq!(ty, tag(bits(3, 2)));
        Ok(())
    }

    #[test]
    fn unifies_non_equal_tags_to_void() -> Test {
        use crate::primitives::{tag, bits};
        let mut tgb = TypeGraph::default();
        let path = test_path();
        // value must start with b11
        tgb.require_assignable(&path, &tag(bits(3, 2)))?;
        // value must start with b00
        tgb.require_assignable(&path, &tag(bits(0, 2)))?;

        let ty = tgb.get_type(&path).unwrap();
        // therefore there's no valid value
        assert_eq!(ty, void_type());
        Ok(())
    }

    #[test]
    fn unifies_trit_or_bit_in_types() -> Test {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        let trit_or_bit = || Union(set![bit_type(), trit_type()]);
        tgb.require_assignable(&path, &trit_or_bit())?;
        tgb.require_assignable(&path, &trit_or_bit())?;

        let norm = tgb.normalize(trit_or_bit())?;
        let ty = tgb.get_type(&path).unwrap();
        assert_eq!(ty, norm);
        Ok(())
    }

    #[test]
    fn unifies_equal_i32_in_types() -> Test {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        tgb.require_assignable(&path, &int32(4))?;
        tgb.require_assignable(&path, &int32(4))?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eq!(ty, int32(4));
        Ok(())
    }

    #[test]
    fn unifies_non_equal_i32_in_types_to_void() -> Test {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        tgb.require_assignable(&path, &int32(4))?;
        tgb.require_assignable(&path, &int32(5))?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eq!(ty, void_type());
        Ok(())
    }

    #[test]
    fn unifies_equal_strs_in_types_to_void() -> Test {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        tgb.require_assignable(&path, &string("Woo"))?;
        tgb.require_assignable(&path, &string("Woo"))?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eq!(ty, string("Woo"));
        Ok(())
    }

    #[test]
    fn unifies_non_equal_str_in_types_to_void() -> Test {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        tgb.require_assignable(&path, &string("Yes"))?;
        tgb.require_assignable(&path, &string("No"))?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eq!(ty, void_type());
        Ok(())
    }

    #[test]
    fn unifies_equal_padded_bools_in_types() -> Test {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        let a = Padded(10, Box::new(boolean(true)));
        tgb.require_assignable(&path, &a)?;
        tgb.require_assignable(&path, &a)?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eq!(ty, a);
        Ok(())
    }

    #[test]
    fn unifies_equivalent_nonequal_padded_bools_in_types() -> Test {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        let a = Padded(5, Box::new(Padded(5, Box::new(boolean(true)))));
        let b = Padded(10, Box::new(boolean(true)));
        tgb.require_assignable(&path, &a)?;
        tgb.require_assignable(&path, &b)?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eq!(ty, b);
        Ok(())
    }

    #[test]
    fn unifies_overlapping_padded_bools() -> Test {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        let a = Padded(5, Box::new(boolean(true).padded(5)));
        let b = boolean(true).padded(11);
        tgb.require_assignable(&path, &a)?;
        tgb.require_assignable(&path, &b)?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eq!(
            ty,
            Product(set!(boolean(true), boolean(true).padded(1))).padded(10),
        );
        Ok(())
    }

    #[test]
    fn unifies_non_equivalent_padded_bools_in_types_to_void() -> Test {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        let a = Padded(5, Box::new(boolean(false).padded(6))); // Keep the second padding explicit to test normalization.
        let b = boolean(true).padded(11);
        tgb.require_assignable(&path, &a)?;
        tgb.require_assignable(&path, &b)?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eq!(ty, void_type());
        Ok(())
    }

    #[test]
    fn unifies_non_equal_i32_and_str_in_types_to_void() -> Test {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        tgb.require_assignable(&path, &int32(4))?;
        tgb.require_assignable(&path, &string("No"))?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eq!(ty, void_type());
        Ok(())
    }

    #[test]
    fn unifies_equivalent_unions() -> Test {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        let a = Union(set!(boolean(false), boolean(true)));
        let b = Union(set!(boolean(true), boolean(false)));
        tgb.require_assignable(&path, &a)?;
        tgb.require_assignable(&path, &b)?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eq!(ty, a);
        Ok(())
    }

    #[test]
    fn unifies_unions_with_values() -> Test {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        let a = Union(set!(boolean(false)));
        let b = boolean(false);
        tgb.require_assignable(&path, &a)?;
        tgb.require_assignable(&path, &b)?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eq!(ty, b);
        Ok(())
    }

    #[test]
    fn unifies_overlapping_unions_to_overlap() -> Test {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        let a = Union(set!(int32(3), int32(4), int32(5)));
        let b = Union(set!(int32(1), int32(2), int32(3)));
        tgb.require_assignable(&path, &a)?;
        tgb.require_assignable(&path, &b)?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eq!(ty, int32(3));
        Ok(())
    }

    #[test]
    fn unifies_unions_of_unions() -> Test {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        let a = Union(set!(int32(3), Union(set![int32(4), int32(5)])));
        let b = Union(set!(Union(set![int32(1), int32(2)]), int32(3)));
        tgb.require_assignable(&path, &a)?;
        tgb.require_assignable(&path, &b)?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eq!(ty, int32(3));
        Ok(())
    }

    #[test]
    fn unifies_non_equivalent_unions_to_void() -> Test {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        let a = Union(set!(int32(3)));
        let b = Union(set!(boolean(true), boolean(false)));
        tgb.require_assignable(&path, &a)?;
        tgb.require_assignable(&path, &b)?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eq!(ty, void_type());
        Ok(())
    }

    #[test]
    fn unifies_products_with_values() -> Test {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        let a = Product(set!(boolean(false)));
        let b = boolean(false);
        tgb.require_assignable(&path, &a)?;
        tgb.require_assignable(&path, &b)?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eq!(ty, b);
        Ok(())
    }

    #[test]
    fn unifies_overlapping_products_to_overlap() -> Test {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        let a = Product(set!(variable("a"), variable("b")));
        let b = Product(set!(variable("b"), variable("c")));
        let res = Product(set!(variable("a"), variable("b"), variable("c")));
        tgb.require_assignable(&path, &a)?;
        tgb.require_assignable(&path, &b)?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eq!(ty, res);
        Ok(())
    }

    #[test]
    fn unifies_products_of_products() -> Test {
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
        assert_eq!(ty, res);
        Ok(())
    }

    #[test]
    fn unifies_unions_to_union_products() -> Test {
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
        assert_eq!(ty, res);
        Ok(())
    }

    #[test]
    fn unifies_non_equivalent_products_to_void() -> Test {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        let a = Product(set!(int32(3)));
        let b = Product(set!(boolean(true), boolean(false)));
        tgb.require_assignable(&path, &a)?;
        tgb.require_assignable(&path, &b)?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eq!(ty, void_type());
        Ok(())
    }

    #[test]
    fn unifies_equivalent_structs() -> Test {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        let a = rec!["left" => variable("a"), "right" => variable("b")];
        let b = rec!["left" => variable("a"), "right" => variable("b")];
        tgb.require_assignable(&path, &a)?;
        tgb.require_assignable(&path, &b)?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eq!(ty, a);
        Ok(())
    }

    #[test]
    fn unifies_structs() -> Test {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        let a = rec!["left" => variable("a"), "right" => variable("b")];
        let b = rec!["left" => variable("c"), "right" => variable("b")];
        let res =
            rec!["left" => Product(set![variable("a"), variable("c")]), "right" => variable("b")];
        tgb.require_assignable(&path, &a)?;
        tgb.require_assignable(&path, &b)?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eq!(ty, res);
        Ok(())
    }

    #[test]
    fn unifies_products_of_structs() -> Test {
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
        assert_eq!(ty, b);
        Ok(())
    }

    #[test]
    fn unifies_non_equivalent_structs_to_void() -> Test {
        let mut tgb = TypeGraph::default();
        let path = test_path();
        let a = rec!["left" => int32(1), "right" => variable("b")];
        let b = rec!["left" => int32(2), "right" => variable("b")];
        tgb.require_assignable(&path, &a)?;
        tgb.require_assignable(&path, &b)?;

        let ty = tgb.get_type(&path).unwrap();
        assert_eq!(ty, void_type());
        Ok(())
    }

    #[test]
    fn assignment_to_equal_type_bit() -> Test {
        let mut tgb = TypeGraph::default();
        assert!(tgb.is_assignable_to(&bit_type(), &bit_type())?);
        Ok(())
    }

    #[test]
    fn assignment_to_equal_type_pair_bit() -> Test {
        let pair_bit = || record(vec![bit_type(), bit_type()]).unwrap();
        let mut tgb = TypeGraph::default();
        assert!(tgb.is_assignable_to(&pair_bit(), &pair_bit())?);
        Ok(())
    }

    #[test]
    fn assignment_to_equal_type_byte() -> Test {
        let mut tgb = TypeGraph::default();
        assert!(tgb.is_assignable_to(&byte_type(), &byte_type())?);
        Ok(())
    }

    #[test]
    fn assignment_to_equal_type_str() -> Test {
        let mut tgb = TypeGraph::default();
        assert!(tgb.is_assignable_to(&string_type(), &string_type())?);
        Ok(())
    }

    #[test]
    fn assignment_to_equal_type_i32() -> Test {
        let mut tgb = TypeGraph::default();
        assert!(tgb.is_assignable_to(&i32_type(), &i32_type())?);
        Ok(())
    }

    #[test]
    fn unifies_variables_in_types_ensuring_assignment() -> Test {
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