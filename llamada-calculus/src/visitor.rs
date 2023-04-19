use crate::expr;
use crate::types::Never;
use crate::{Expr, Term};

pub trait Visitor<T, E, Over: Expr> {
    fn start_value(&mut self) -> T;

    fn on_ext(&mut self, _ctx: &Over, _ext: &Over::Extension, _meta: &Over::Meta) -> Result<T, E> {
        Ok(self.start_value())
    }
    fn on_var(&mut self, _ctx: &Over, _var: usize, _meta: &Over::Meta) -> Result<T, E> {
        Ok(self.start_value())
    }
    fn on_abs(&mut self, ctx: &Over, inner: &Over::Index, _meta: &Over::Meta) -> Result<T, E> {
        self.on_id(ctx, inner)?;
        Ok(self.start_value())
    }
    fn on_app(
        &mut self,
        ctx: &Over,
        inner: &Over::Index,
        arg: &Over::Index,
        _meta: &Over::Meta,
    ) -> Result<T, E> {
        self.on_id(ctx, inner)?;
        self.on_id(ctx, arg)?;
        Ok(self.start_value())
    }
    fn on_id(&mut self, ctx: &Over, id: &Over::Index) -> Result<T, E> {
        let meta = ctx.get_meta(id);
        self.on_term(ctx, ctx.get(id), meta)
    }
    fn on_term(
        &mut self,
        ctx: &Over,
        term: &Term<Over::Extension, Over::Index>,
        meta: &Over::Meta,
    ) -> Result<T, E> {
        match term {
            Term::Ext(ext) => self.on_ext(ctx, ext, meta),
            Term::Var(n) => self.on_var(ctx, *n, meta),
            Term::Abs(inner) => self.on_abs(ctx, inner, meta),
            Term::App(inner, arg) => self.on_app(ctx, inner, arg, meta),
        }
    }
}

pub struct CountDepth {}

impl<Over: Expr> Visitor<usize, Never, Over> for CountDepth {
    fn start_value(&mut self) -> usize {
        0
    }

    fn on_ext(
        &mut self,
        _ctx: &Over,
        _ext: &Over::Extension,
        _meta: &Over::Meta,
    ) -> Result<usize, Never> {
        Ok(1)
    }
    fn on_var(&mut self, _ctx: &Over, _var: usize, _meta: &Over::Meta) -> Result<usize, Never> {
        Ok(1)
    }
    fn on_abs(
        &mut self,
        ctx: &Over,
        inner: &Over::Index,
        _meta: &Over::Meta,
    ) -> Result<usize, Never> {
        Ok(1 + self.on_id(ctx, inner)?)
    }
    fn on_app(
        &mut self,
        ctx: &Over,
        inner: &Over::Index,
        arg: &Over::Index,
        _meta: &Over::Meta,
    ) -> Result<usize, Never> {
        let a = self.on_id(ctx, inner)?;
        let b = self.on_id(ctx, arg)?;
        Ok(1 + std::cmp::max(a, b))
    }
}

pub struct CountNodes {}

impl<Over: Expr> Visitor<usize, Never, Over> for CountNodes {
    fn start_value(&mut self) -> usize {
        0
    }

    fn on_ext(
        &mut self,
        _ctx: &Over,
        _ext: &Over::Extension,
        _meta: &Over::Meta,
    ) -> Result<usize, Never> {
        Ok(1)
    }
    fn on_var(&mut self, _ctx: &Over, _var: usize, _meta: &Over::Meta) -> Result<usize, Never> {
        Ok(1)
    }
    fn on_abs(
        &mut self,
        ctx: &Over,
        inner: &Over::Index,
        _meta: &Over::Meta,
    ) -> Result<usize, Never> {
        Ok(1 + self.on_id(ctx, inner)?)
    }
    fn on_app(
        &mut self,
        ctx: &Over,
        inner: &Over::Index,
        arg: &Over::Index,
        _meta: &Over::Meta,
    ) -> Result<usize, Never> {
        Ok(1 + self.on_id(ctx, inner)? + self.on_id(ctx, arg)?)
    }
}

pub struct TransformMeta<Ctx: Expr, Res: Expr, F: FnMut(&Ctx::Meta, Vec<&Res::Index>, Option<&Ctx::Extension>) -> Res::Meta> {
    f: F,
    output: Res,
    _ctx: std::marker::PhantomData<Ctx>,
}

impl<
        Over: Expr,
        Res: Expr<Extension = Over::Extension>,
        F: FnMut(&Over::Meta, Vec<&Res::Index>, Option<&Over::Extension>) -> Res::Meta,
    > Visitor<Res::Index, Never, Over> for TransformMeta<Over, Res, F>
{
    fn start_value(&mut self) -> Res::Index {
        self.output.get_last_id()
    }

    fn on_ext(
        &mut self,
        _ctx: &Over,
        ext: &Over::Extension,
        meta: &Over::Meta,
    ) -> Result<Res::Index, Never> {
        Ok(self
            .output
            .add_with_meta(Term::Ext(ext.clone()), (self.f)(meta, vec![], Some(ext))))
    }
    fn on_var(&mut self, _ctx: &Over, var: usize, meta: &Over::Meta) -> Result<Res::Index, Never> {
        Ok(self.output.add_with_meta(Term::Var(var), (self.f)(meta, vec![], None)))
    }
    fn on_abs(
        &mut self,
        ctx: &Over,
        inner: &Over::Index,
        meta: &Over::Meta,
    ) -> Result<Res::Index, Never> {
        let inner = self.on_id(ctx, inner)?;
        let meta = (self.f)(meta, vec![&inner], None);
        Ok(self.output.add_with_meta(Term::Abs(inner), meta))
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
        Ok(self
            .output
            .add_with_meta(Term::App(inner, arg), meta))
    }
}

pub struct FromCompactToChurch {
    output: LambdaCalc,
}
use crate::compact_numerals::{LambdaCalc, NumExt, NumOp};

impl Visitor<usize, Never, LambdaCalc> for FromCompactToChurch {
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
            NumExt::Value(n) => self.output.to_church(*n),
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
                        mnfx_1 = Abs(mnfx),
                        mnfx_2 = Abs(mnfx_1),
                        mnfx_3 = Abs(mnfx_2),
                        plus = Abs(mnfx_3),
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
                        mfnfx_1 = Abs(mfnfx),
                        mfnfx_2 = Abs(mfnfx_1),
                        mfnfx_3 = Abs(mfnfx_2),
                        mul = Abs(mfnfx_3),
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
        inner: &usize,
        _meta: &<LambdaCalc as Expr>::Meta,
    ) -> Result<usize, Never> {
        let inner = self.on_id(ctx, inner)?;
        Ok(self.output.add(Term::Abs(inner)))
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

#[cfg(test)]
mod test {
    use std::marker::PhantomData;

    use crate::compact_numerals::{NumExt, NumOp};
    use crate::{new_expr, DenseRepr};
    use crate::types::Empty;

    use super::*;

    #[test]
    fn count_nodes_in_church_n() {
        let mut cn = CountNodes {};

        for i in 0..100 {
            let mut expr = LambdaCalc::new(Term::Var(1), Empty {});
            let num = expr.to_church(i as u32);
            *expr.root_mut() = num;

            eprintln!("{}", expr);
            assert_eq!(expr.traverse(&mut cn), Ok(i * 2 + 3));
        }
    }

    #[test]
    fn count_depth_in_church_n() {
        let mut cn = CountDepth {};

        for i in 0..100 {
            let mut expr = LambdaCalc::new(Term::Var(1), Empty {});
            let num = expr.to_church(i as u32);
            *expr.root_mut() = num;

            eprintln!("{}", expr);
            assert_eq!(expr.traverse(&mut cn), Ok(i + 3));
        }
    }

    #[test]
    fn from_compact_to_church_add() {
        let mut cn = FromCompactToChurch {
            output: LambdaCalc::new(Term::Var(1), Empty {}),
        };

        let mut expr = LambdaCalc::new(Term::Ext(NumExt::Value(5)), Empty {});
        let a = expr.get_last_id();
        let b = expr.add(Term::Ext(NumExt::Value(4)));
        let plus = expr.add(Term::Ext(NumExt::Op(NumOp::Add)));
        let p_a = expr.add(Term::App(plus, a));
        let p_a_b = expr.add(Term::App(p_a, b));
        *expr.root_mut() = p_a_b;

        eprintln!("{}", expr);
        let root = expr.traverse(&mut cn).expect("Error");
        *cn.output.root_mut() = root;
        let mut church_expr = cn.output;
        church_expr.reduce();

        assert_eq!(
            format!("{}", church_expr),
            "(\\a. (\\b. (a (a (a (a (a (a (a (a (a b)))))))))))"
        );
    }

    #[test]
    fn from_compact_to_church_mul() {
        let mut cn = FromCompactToChurch {
            output: LambdaCalc::new(Term::Var(1), Empty {}),
        };

        let expr = new_expr!(
            LambdaCalc,
            p_a_b,
            a = Ext(NumExt::Value(3)),
            b = Ext(NumExt::Value(4)),
            plus = Ext(NumExt::Op(NumOp::Mul)),
            p_a = App(plus, a),
            p_a_b = App(p_a, b),
        );

        eprintln!("{}", expr);
        let root = expr.traverse(&mut cn).expect("Error");
        *cn.output.root_mut() = root;
        let mut church_expr = cn.output;
        church_expr.reduce();

        assert_eq!(
            format!("{}", church_expr),
            "(\\a. (\\b. (a (a (a (a (a (a (a (a (a (a (a (a b))))))))))))))"
        );
    }

    #[test]
    fn arity_checker() {
        let expr = new_expr!(
            LambdaCalc,
            p_a_b,
            a = Ext(NumExt::Value(3)),
            b = Ext(NumExt::Value(4)),
            plus = Ext(NumExt::Op(NumOp::Mul)),
            p_a = App(plus, a),
            p_a_b = App(p_a, b),
        );
        let old_expr = expr.clone(); // TODO: NAH

        eprintln!("{}", expr);

        let mut node_count: usize = 0;
        let mut cn = TransformMeta {
            _ctx: PhantomData,
            f: move | _meta: &Empty, _args: Vec<&usize>, _ext: Option<&NumExt> | {
                node_count+=1;
                node_count
            },
            output: DenseRepr::new(Term::Var(1), 0),
        };

        let root = expr.traverse(&mut cn).expect("Error");
        cn.output.set_root(root);
        cn.output.set_print_meta(true);
        let expr = cn.output;

        eprintln!("{}", expr);

        use std::collections::HashMap;
        let mut arity_graph = HashMap::<usize, usize>::new();
        let mut cn = TransformMeta {
            _ctx: PhantomData,
            f: move | meta: &usize, args: Vec<&usize>, ext: Option<&NumExt> | {
                let arity = if let Some(ext) = ext {
                    old_expr.ext_info(ext).expect("Huh?").arity
                } else {
                    match args[..] {
                        [inner, _arg] => { // App
                            let inner_arity = arity_graph.get(inner).expect("Huh???");
                            inner_arity-1
                        }
                        [inner] => { // Abs
                            let inner_arity = arity_graph.get(inner).expect("Huh???");
                            inner_arity+1
                        }
                        [] => todo!(), // Var
                        _ => todo!(),
                    }
                };
                arity_graph.insert(*meta, arity);
                arity
            },
            output: DenseRepr::new(Term::<NumExt, usize>::Var(1), 0),
        };
        let root = expr.traverse(&mut cn).expect("Error");
        cn.output.set_root(root);

        let mut with_arity = cn.output;
        with_arity.set_print_meta(true);

        assert_eq!(
            format!("{}", with_arity),
            "((Mul: 2 3: 0): 1 4: 0): 0"
        );
    }
}
