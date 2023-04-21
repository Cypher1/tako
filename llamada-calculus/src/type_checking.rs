use crate::base_types::Never;
use crate::expr_result::{EvalInfo, ValueInfo};
use crate::reprs::compact_numerals::{NumExt, NumTyInfo, Prim, PrimType};
use crate::visitors::compact_to_church::map_to_nodes;
use crate::visitors::number_nodes;
use crate::{derive_expr_from, DenseRepr, Evaluable, Expr, ExprResult, Term, Visitor};
use std::collections::HashMap;

type VarId = u32;

pub trait TypeChecker<E: Expr> {
    type Type;
    type Value;
    type Error;
    type R;
    fn unknown(&mut self) -> Self::Type;
    fn var(&mut self, id: VarId) -> Self::Type;
    fn app(&mut self, inner: Self::Type, arg: Self::Type) -> Self::Type;
    fn abs(&mut self, arg: Self::Type, inner: Self::Type) -> Self::Type;
    fn get_type(&mut self, value: &Self::Value) -> Self::Type;

    fn check(&mut self, input: &E) -> Result<Self::R, Self::Error>;
    fn propagate(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }
}

pub struct Hindley<TyRepr, Ty: Default = TyRepr> {
    terms: DenseRepr<TyRepr, usize>,
    ty_graph: HashMap<usize, Ty>, // From node number to index in `terms`.
    var_count: VarId,
}

impl<TyRepr: Evaluable + Clone + std::fmt::Display, Ty: Default> Default for Hindley<TyRepr, Ty> {
    fn default() -> Self {
        // TODO: Reduce duplication
        Self {
            terms: DenseRepr::new(Term::Var(0), 0),
            // ty_graph: HashMap::new(),
            ty_graph: HashMap::new(),
            var_count: 1,
        }
    }
}

impl<
        TypeRepr: Clone,
        Ty: Default + Clone,
        Value,
        Error,
        Over: Expr<Meta = usize, Value = Value>,
    > Visitor<Error, Over> for Hindley<TypeRepr, Ty>
where
    Hindley<TypeRepr, Ty>: TypeChecker<Over, Type = Ty, Value = Value>,
{
    type Type = <Self as TypeChecker<Over>>::Type;
    fn start_value(&mut self) -> Self::Type {
        self.unknown()
    }

    fn on_var(&mut self, _ctx: &Over, _var: usize, meta: &usize) -> Result<Self::Type, Error> {
        let ty = self.unknown();
        self.ty_graph.insert(*meta, ty.clone());
        Ok(ty)
    }

    fn on_abs(
        &mut self,
        ctx: &Over,
        _arg_meta: &Option<Over::Index>, // TODO: ???
        inner: &Over::Index,
        meta: &Over::Meta,
    ) -> Result<Self::Type, Error> {
        let inner_ty = self.on_id(ctx, inner)?;
        let arg_ty = self.unknown();
        // TODO: Require arg to be the right type.
        let ty = self.abs(arg_ty, inner_ty);
        self.ty_graph.insert(*meta, ty.clone());
        Ok(ty)
    }

    fn on_app(
        &mut self,
        ctx: &Over,
        inner: &Over::Index,
        arg: &Over::Index,
        meta: &Over::Meta,
    ) -> Result<Self::Type, Error> {
        let inner_ty = self.on_id(ctx, inner)?;
        let arg_ty = self.on_id(ctx, arg)?;
        let ty = self.app(inner_ty, arg_ty);
        self.ty_graph.insert(*meta, ty.clone());
        Ok(ty)
    }

    fn on_ext(
        &mut self,
        _ctx: &Over,
        ext: &<Over as Expr>::Value,
        meta: &<Over as Expr>::Meta,
    ) -> Result<Self::Type, Error> {
        let ty = self.get_type(ext);
        self.ty_graph.insert(*meta, ty.clone());
        Ok(ty)
    }
}

// #[derive(Debug, Clone, PartialEq, Eq, Hash)]
type MonoType = PrimType;

impl std::fmt::Display for MonoType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct PolyType {
    mono: MonoType,
    arity: usize,
    // requirements: Vec<()>,
}

impl Evaluable for PolyType {
    fn info(&self) -> Option<EvalInfo> {
        Some(EvalInfo::new(self.arity))
    }
}

impl std::fmt::Display for PolyType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let Self { mono, arity } = self;
        for i in 0..*arity {
            write!(f, "x{i} => ")?;
        }
        write!(f, "{mono}")
    }
}

impl Default for PolyType {
    fn default() -> Self {
        Self::top()
    }
}
impl PolyType {
    pub fn top() -> Self {
        Self {
            mono: PrimType::Type,
            arity: 0,
            // TODO:
        }
    }
    pub fn u32() -> Self {
        Self {
            mono: PrimType::U32,
            arity: 0,
            // TODO:
        }
    }
}

type PolyTypeId = usize;

impl Expr for Hindley<PolyType, PolyTypeId> {
    type Index = PolyTypeId;
    type Value = PolyType;
    type Meta = usize;

    fn new(term: Term<Self::Value, Self::Index>, meta: Self::Meta) -> Self {
        Self {
            terms: DenseRepr::new(term, meta),
            // ty_graph: HashMap::new(),
            ty_graph: HashMap::new(),
            var_count: 1,
        }
    }

    fn reduce_ext_apps(
        &mut self,
        value: ExprResult<Self::Index, EvalInfo>,
    ) -> ExprResult<Self::Index, EvalInfo> {
        if let Some(ext_info) = &value.ext_info {
            if ext_info.complete() {
                let inner = self.get(&value.id).clone();
                let res = match inner {
                    Term::Ext(_) => inner,
                    _ => todo!("Un handled reduction"),
                };
                return ExprResult::unchanged(self.add(res)).changed();
            }
        }
        value
    }

    derive_expr_from!(terms);
}

impl<E: Expr<Value = NumExt>> TypeChecker<E> for Hindley<PolyType, PolyTypeId> {
    type Type = PolyTypeId;
    type Value = NumExt;
    type Error = Never;
    type R = DenseRepr<E::Value, Self::Type>;

    fn propagate(&mut self) -> Result<(), Self::Error> {
        // TODO
        Ok(())
    }

    fn check(&mut self, input: &E) -> Result<Self::R, Self::Error> {
        let numbered_expr: DenseRepr<E::Value, usize> = number_nodes(input);
        numbered_expr.traverse(self)?;

        <Self as TypeChecker<E>>::propagate(self)?;

        let mut res: Self::R = map_to_nodes(&self.ty_graph, numbered_expr);
        res.set_print_meta(true);
        Ok(res)
    }

    fn unknown(&mut self) -> Self::Type {
        self.var_count += 1;
        self.terms.add(Term::Var(self.var_count as usize)) // TODO: Rethink this
    }
    fn var(&mut self, id: VarId) -> Self::Type {
        self.terms.add(Term::Var(id as usize)) // TODO: Rethink this
    }

    fn app(&mut self, inner: Self::Type, arg: Self::Type) -> Self::Type {
        let id = self.terms.add(Term::App(inner, arg));
        let res = self.reduce_at(id, 0); // TODO
        res.id
    }

    fn abs(&mut self, arg: Self::Type, inner: Self::Type) -> Self::Type {
        self.terms.add(Term::Abs(Some(arg), inner))
    }

    fn get_type(&mut self, value: &Self::Value) -> Self::Type {
        match value {
            NumExt::Value(value) => {
                let ty = match value {
                    Prim::U32(_) => PolyType::u32(),
                    Prim::Type(_) => PolyType::top(),
                    // _ => self.unknown(), // TODO: For other types...
                };
                self.terms.add(Term::Ext(ty))
            }
            NumExt::Op(_) => {
                let mono_ty = PolyType::u32();
                let mut curr = self.terms.add(Term::Ext(mono_ty));
                // let ty = self.terms.add(Term::Ext(ty));
                for _ in 0..value.info().expect("???").arity {
                    let arg = PolyType::u32();
                    let arg = self.terms.add(Term::Ext(arg));
                    curr = <Self as TypeChecker<E>>::abs(self, arg, curr); // TODO: Look up types...
                }
                curr
            }
        }
    }
}

impl<E: Expr<Value = NumExt>> TypeChecker<E> for Hindley<NumTyInfo> {
    type Type = NumTyInfo;
    type Value = NumExt;
    type Error = Never;
    type R = DenseRepr<E::Value, Self::Type>;

    fn propagate(&mut self) -> Result<(), Self::Error> {
        // TODO
        Ok(())
    }

    fn check(&mut self, input: &E) -> Result<Self::R, Self::Error> {
        let numbered_expr: DenseRepr<E::Value, usize> = number_nodes(input);
        numbered_expr.traverse(self)?;

        <Self as TypeChecker<E>>::propagate(self)?;

        let mut res: Self::R = map_to_nodes(&self.ty_graph, numbered_expr);
        res.set_print_meta(true);
        Ok(res)
    }

    fn unknown(&mut self) -> Self::Type {
        self.var_count += 1;
        Self::Type::Var(self.var_count)
    }
    fn var(&mut self, id: VarId) -> Self::Type {
        Self::Type::Var(id)
    }

    fn app(&mut self, inner: Self::Type, arg: Self::Type) -> Self::Type {
        Self::Type::App(Box::new(inner), Box::new(arg))
    }

    fn abs(&mut self, arg: Self::Type, inner: Self::Type) -> Self::Type {
        Self::Type::Abs(Box::new(arg), Box::new(inner))
    }

    fn get_type(&mut self, value: &Self::Value) -> Self::Type {
        match value {
            NumExt::Value(value) => match value {
                Prim::U32(_) => Self::Type::U32,
                Prim::Type(_) => Self::Type::Unknown,
                // _ => self.unknown(), // TODO: For other types...
            },
            NumExt::Op(_) => {
                let mut curr = Self::Type::U32;
                for _ in 0..value.info().expect("???").arity {
                    curr = <Self as TypeChecker<E>>::abs(self, Self::Type::U32, curr);
                    // TODO: Look up types...
                }
                curr
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::reprs::compact_numerals::{LambdaCalc, NumOp};
    use crate::visitors::compact_to_church::map_nodes;
    use crate::Expr;
    use crate::{base_types::Empty, ext, new_expr, reprs::compact_numerals::NumTyInfo};

    #[test]
    fn simple_type_system_with_no_collapsing() {
        let expr = new_expr!(
            LambdaCalc,
            p_a_b,
            a = ext(3),
            b = ext(4),
            plus = ext(NumOp::Mul),
            p_a = App(plus, a),
            p_a_b = App(p_a, b),
        );
        eprintln!("{}", expr);

        let mut hm = Hindley::<NumTyInfo>::default();
        let with_ty = hm.check(&expr).expect("Whoops");
        eprintln!("{}", with_ty);

        assert_eq!(
            format!("{}", with_ty),
            "((Mul: (U32 -> (U32 -> U32)) 3: U32): ((U32 -> (U32 -> U32))U32) 4: U32): (((U32 -> (U32 -> U32))U32)U32)"
        );
    }

    #[test]
    fn simply_typed() {
        let expr = new_expr!(
            LambdaCalc,
            p_a_b,
            a = ext(3),
            b = ext(4),
            plus = ext(NumOp::Mul),
            p_a = App(plus, a),
            p_a_b = App(p_a, b),
        );
        eprintln!("{}", expr);

        let mut hm = Hindley::<PolyType, PolyTypeId>::default();
        let with_raw_ty = hm.check(&expr).expect("Whoops");
        eprintln!("{}", with_raw_ty);
        // hm.terms.set_print_meta(true);

        let mut with_ty: DenseRepr<NumExt, String> = map_nodes(
            &mut |key| format!("{}", hm.terms.as_context(key)),
            with_raw_ty,
        );
        with_ty.set_print_meta(true);
        eprintln!("{}", with_ty);

        assert_eq!(
            format!("{}", with_ty),
            "((Mul: (\\a: U32. (\\b: U32. U32)) 3: U32): (\\a: U32. U32) 4: U32): U32"
        );
        // TODO: Show constraints.
    }
}
