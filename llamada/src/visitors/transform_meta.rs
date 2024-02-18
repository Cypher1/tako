use crate::base_types::Never;
use crate::expr;
use crate::{Expr, Term, Visitor};
use std::collections::HashMap;
use std::marker::PhantomData;

pub struct TransformMeta<
    Ctx: Expr,
    Res: Expr,
    F: FnMut(&Ctx::Meta, Vec<&Res::Index>, Option<&Ctx::Value>) -> Res::Meta,
> {
    pub f: F,
    pub output: Res,
    _ctx: PhantomData<Ctx>,
}

impl<
        Ctx: Expr,
        Res: Expr,
        F: FnMut(&Ctx::Meta, Vec<&Res::Index>, Option<&Ctx::Value>) -> Res::Meta,
    > TransformMeta<Ctx, Res, F>
{
    pub fn new(f: F, output: Res) -> Self {
        Self {
            _ctx: PhantomData,
            f,
            output,
        }
    }
}

impl<
        Over: Expr,
        Res: Expr<Value = Over::Value>,
        F: FnMut(&Over::Meta, Vec<&Res::Index>, Option<&Over::Value>) -> Res::Meta,
    > Visitor<Never, Over> for TransformMeta<Over, Res, F>
where
    Over::Value: Clone,
{
    type Type = Res::Index; // TODO: Remove when item default associated types are standardized.
                            //
    fn start_value(&mut self) -> Res::Index {
        self.output.get_last_id()
    }

    fn on_ext(
        &mut self,
        _ctx: &Over,
        ext: &Over::Value,
        meta: &Over::Meta,
    ) -> Result<Res::Index, Never> {
        Ok(self
            .output
            .add_with_meta(Term::Ext(ext.clone()), (self.f)(meta, vec![], Some(ext))))
    }
    fn on_var(&mut self, _ctx: &Over, var: usize, meta: &Over::Meta) -> Result<Res::Index, Never> {
        Ok(self
            .output
            .add_with_meta(Term::Var(var), (self.f)(meta, vec![], None)))
    }
    fn on_abs(
        &mut self,
        ctx: &Over,
        _arg_meta: &Option<Over::Index>,
        inner: &Over::Index,
        meta: &Over::Meta,
    ) -> Result<Res::Index, Never> {
        let inner = self.on_id(ctx, inner)?;
        let meta = (self.f)(meta, vec![&inner], None);
        // TODO: Re-evaluate. Convert _arg_meta to meta
        Ok(self.output.add_with_meta(Term::Abs(None, inner), meta))
    }
    fn on_app(
        &mut self,
        ctx: &Over,
        inner: &Over::Index,
        arg: &Over::Index,
        meta: &Over::Meta,
    ) -> Result<Res::Index, Never> {
        let inner = self.on_id(ctx, inner)?;
        let arg = self.on_id(ctx, arg)?;
        let meta = (self.f)(meta, vec![&inner, &arg], None);
        Ok(self.output.add_with_meta(Term::App(inner, arg), meta))
    }
}

pub struct FromCompactToChurch {
    output: LambdaCalc,
}
use crate::reprs::compact_numerals::{LambdaCalc, NumExt, NumOp, Prim};

impl Visitor<Never, LambdaCalc> for FromCompactToChurch {
    type Type = usize; // TODO: Remove when item default associated types are standardized.
    fn start_value(&mut self) -> usize {
        self.output.get_last_id()
    }

    fn on_ext(
        &mut self,
        _ctx: &LambdaCalc,
        ext: &NumExt,
        _meta: &<LambdaCalc as Expr>::Meta,
    ) -> Result<usize, Never> {
        let res = match ext {
            NumExt::Value(Prim::U32(n)) => self.output.to_church(*n),
            NumExt::Value(_) => todo!(),
            NumExt::Op(op) => match op {
                NumOp::Mul => {
                    expr!(
                        self.output,
                        plus,
                        x = Var(1),
                        f = Var(2),
                        n = Var(3),
                        m = Var(4),
                        nf = App(n, f),
                        mnf = App(m, nf),
                        mnfx = App(mnf, x),
                        mnfx_1 = Term::abs(mnfx),
                        mnfx_2 = Term::abs(mnfx_1),
                        mnfx_3 = Term::abs(mnfx_2),
                        plus = Term::abs(mnfx_3),
                    )
                }
                NumOp::Add => {
                    expr!(
                        self.output,
                        mul,
                        x = Var(1),
                        f = Var(2),
                        n = Var(3),
                        m = Var(4),
                        nf = App(n, f),
                        mf = App(m, f),
                        nfx = App(nf, x),
                        mfnfx = App(mf, nfx),
                        mfnfx_1 = Term::abs(mfnfx),
                        mfnfx_2 = Term::abs(mfnfx_1),
                        mfnfx_3 = Term::abs(mfnfx_2),
                        mul = Term::abs(mfnfx_3),
                    )
                }
                _ => todo!(),
            },
        };
        Ok(res)
    }
    fn on_var(
        &mut self,
        _ctx: &LambdaCalc,
        var: usize,
        _meta: &<LambdaCalc as Expr>::Meta,
    ) -> Result<usize, Never> {
        Ok(self.output.add(Term::Var(var)))
    }
    fn on_abs(
        &mut self,
        ctx: &LambdaCalc,
        _arg_meta: &Option<<LambdaCalc as Expr>::Index>,
        inner: &usize,
        _meta: &<LambdaCalc as Expr>::Meta,
    ) -> Result<usize, Never> {
        let inner = self.on_id(ctx, inner)?;
        Ok(self.output.add(Term::abs(inner)))
    }
    fn on_app(
        &mut self,
        ctx: &LambdaCalc,
        inner: &usize,
        arg: &usize,
        _meta: &<LambdaCalc as Expr>::Meta,
    ) -> Result<usize, Never> {
        let a = self.on_id(ctx, inner)?;
        let b = self.on_id(ctx, arg)?;
        Ok(self.output.add(Term::App(a, b)))
    }
}

pub fn number_nodes<E: Expr, R: Expr<Value = E::Value, Meta = usize>>(expr: &E) -> R
where
    E::Value: Clone,
{
    let mut node_count: usize = 0;
    let mut cn = TransformMeta::new(
        move |_meta: &E::Meta, _args: Vec<&R::Index>, _ext: Option<&E::Value>| {
            node_count += 1;
            node_count
        },
        R::new(Term::Var(1), 0),
    );

    let root = expr.traverse(&mut cn).expect("Error");
    *cn.output.root_mut() = root;
    cn.output
}

pub fn map_nodes<E: Expr, R: Expr<Value = E::Value>, F: FnMut(&E::Meta) -> R::Meta>(
    f: &mut F,
    expr: E,
) -> R
where
    E::Value: Clone,
    R::Meta: Default,
{
    let mut cn = TransformMeta::new(
        move |meta: &E::Meta, _args: Vec<&R::Index>, _ext: Option<&E::Value>| f(meta),
        R::new(Term::Var(1), R::Meta::default()),
    );

    let root = expr.traverse(&mut cn).expect("Error");
    *cn.output.root_mut() = root;
    cn.output
}

pub fn map_to_nodes<
    K: Eq + std::hash::Hash,
    V: Clone + Default,
    E: Expr<Meta = K>,
    R: Expr<Value = E::Value, Meta = V>,
>(
    data: &HashMap<K, V>,
    expr: E,
) -> R
where
    E::Value: Clone,
{
    map_nodes(
        &mut |meta: &E::Meta| data.get(meta).cloned().unwrap_or_default(),
        expr,
    )
}

#[cfg(test)]
mod test {
    use crate::base_types::Empty;
    use crate::reprs::compact_numerals::{NumExt, NumOp};
    use crate::{ext, new_expr, DenseRepr, Evaluable};

    use super::*;

    #[test]
    fn arity_checker() {
        let (expr, _) = new_expr!(
            LambdaCalc,
            p_a_b,
            a = ext(3),
            b = ext(4),
            plus = ext(NumOp::Mul),
            p_a = App(plus, a),
            p_a_b = App(p_a, b),
        );
        eprintln!("{expr}");

        let expr: DenseRepr<NumExt, usize> = number_nodes(&expr);
        eprintln!("{expr}");

        use std::collections::HashMap;
        let mut arity_graph = HashMap::<usize, usize>::new();
        let mut cn = TransformMeta::new(
            move |meta: &usize, args: Vec<&usize>, ext: Option<&NumExt>| {
                let arity = if let Some(ext) = ext {
                    ext.info().expect("Hmmm").arity
                } else {
                    match args[..] {
                        [inner, _arg] => {
                            // App
                            let inner_arity = arity_graph.get(inner).expect("Huh???");
                            inner_arity - 1
                        }
                        [inner] => {
                            // Abs
                            let inner_arity = arity_graph.get(inner).expect("Huh???");
                            inner_arity + 1
                        }
                        [] => todo!(), // Var
                        _ => todo!(),
                    }
                };
                arity_graph.insert(*meta, arity);
                arity
            },
            DenseRepr::new(Term::<NumExt, usize>::Var(1), 0),
        );
        let root = expr.traverse(&mut cn).expect("Error");
        cn.output.set_root(root);

        let mut with_arity = cn.output;
        with_arity.set_print_meta(true);

        assert_eq!(format!("{with_arity}"), "((Mul: 2 3: 0): 1 4: 0): 0");
    }
}
