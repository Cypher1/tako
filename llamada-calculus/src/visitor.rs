use crate::{Expr, Term};
use crate::types::Never;

pub trait Visitor<T, E, Over: Expr> {
    fn start_value(&mut self) -> T;

    fn on_ext(&mut self, _ctx: &Over, _ext: &Over::Extension) -> Result<T, E> {
        Ok(self.start_value())
    }
    fn on_var(&mut self, _ctx: &Over, _var: usize) -> Result<T, E> {
        Ok(self.start_value())
    }
    fn on_abs(&mut self, ctx: &Over, inner: &Term<Over::Extension, Over::Index>) -> Result<T, E> {
        self.on_term(ctx, inner)?;
        Ok(self.start_value())
    }
    fn on_app(&mut self, ctx: &Over,
        inner: &Term<Over::Extension, Over::Index>,
        arg: &Term<Over::Extension, Over::Index>,
    ) -> Result<T, E> {
        self.on_term(ctx, inner)?;
        self.on_term(ctx, arg)?;
        Ok(self.start_value())
    }
    fn on_term(&mut self, ctx: &Over, term: &Term<Over::Extension, Over::Index>) -> Result<T, E> {
        match term {
            Term::Ext(ext) => self.on_ext(ctx, ext),
            Term::Var(n) => self.on_var(ctx, *n),
            Term::Abs(inner) => {
                let inner = ctx.get(inner);
                self.on_abs(ctx, inner)
            }
            Term::App(inner, arg) => {
                let inner = ctx.get(inner);
                let arg = ctx.get(arg);
                self.on_app(ctx, inner, arg)
            }
        }
    }
}

struct CountDepth {}

impl<Over: Expr> Visitor<usize, Never, Over> for CountDepth {
    fn start_value(&mut self) -> usize {
        0
    }

    fn on_ext(&mut self, _ctx: &Over, _ext: &Over::Extension) -> Result<usize, Never> {
        Ok(1)
    }
    fn on_var(&mut self, _ctx: &Over, _var: usize) -> Result<usize, Never> {
        Ok(1)
    }
    fn on_abs(&mut self, ctx: &Over, inner: &Term<Over::Extension, Over::Index>) -> Result<usize, Never> {
        Ok(1+self.on_term(ctx, inner)?)
    }
    fn on_app(&mut self, ctx: &Over,
        inner: &Term<Over::Extension, Over::Index>,
        arg: &Term<Over::Extension, Over::Index>,
    ) -> Result<usize, Never> {
        let a = self.on_term(ctx, inner)?;
        let b = self.on_term(ctx, arg)?;
        Ok(1+std::cmp::max(a, b))
    }
}

struct CountNodes {}

impl<Over: Expr> Visitor<usize, Never, Over> for CountNodes {
    fn start_value(&mut self) -> usize {
        0
    }

    fn on_ext(&mut self, _ctx: &Over, _ext: &Over::Extension) -> Result<usize, Never> {
        Ok(1)
    }
    fn on_var(&mut self, _ctx: &Over, _var: usize) -> Result<usize, Never> {
        Ok(1)
    }
    fn on_abs(&mut self, ctx: &Over, inner: &Term<Over::Extension, Over::Index>) -> Result<usize, Never> {
        Ok(1+self.on_term(ctx, inner)?)
    }
    fn on_app(&mut self, ctx: &Over,
        inner: &Term<Over::Extension, Over::Index>,
        arg: &Term<Over::Extension, Over::Index>,
    ) -> Result<usize, Never> {
        Ok(1+self.on_term(ctx, inner)?+self.on_term(ctx, arg)?)
    }
}

struct FromCompactToChurch {
    output: LambdaCalc,
}
use crate::compact_numerals::{LambdaCalc, NumExt, NumOp};

impl Visitor<usize, Never, LambdaCalc> for FromCompactToChurch {
    fn start_value(&mut self) -> usize {
        self.output.get_last_id()
    }

    fn on_ext(&mut self, _ctx: &LambdaCalc, ext: &NumExt) -> Result<usize, Never> {
        let res = match ext {
            NumExt::Value(n) => self.output.to_church(*n),
            NumExt::Op(op) => match op {
                NumOp::Add => {
                    let x = self.output.add(Term::Var(1));
                    let f = self.output.add(Term::Var(2));
                    let n = self.output.add(Term::Var(3));
                    let m = self.output.add(Term::Var(4));
                    let nf = self.output.add(Term::App(n, f));
                    let mf = self.output.add(Term::App(m, f));
                    let nfx = self.output.add(Term::App(nf, x));
                    let mfnfx = self.output.add(Term::App(mf, nfx));
                    let mfnfx_1 = self.output.add(Term::Abs(mfnfx));
                    let mfnfx_2 = self.output.add(Term::Abs(mfnfx_1));
                    let mfnfx_3 = self.output.add(Term::Abs(mfnfx_2));
                    let plus = self.output.add(Term::Abs(mfnfx_3));
                    plus
                }
                _ => todo!(),
            }
        };
        Ok(res)
    }
    fn on_var(&mut self, _ctx: &LambdaCalc, var: usize) -> Result<usize, Never> {
        Ok(self.output.add(Term::Var(var)))
    }
    fn on_abs(&mut self, ctx: &LambdaCalc, inner: &Term<NumExt, usize>) -> Result<usize, Never> {
        let inner = self.on_term(ctx, inner)?;
        Ok(self.output.add(Term::Abs(inner)))
    }
    fn on_app(&mut self, ctx: &LambdaCalc,
        inner: &Term<NumExt, usize>,
        arg: &Term<NumExt, usize>,
    ) -> Result<usize, Never> {
        let a = self.on_term(ctx, inner)?;
        let b = self.on_term(ctx, arg)?;
        Ok(self.output.add(Term::App(a, b)))
    }
}

#[cfg(test)]
mod test {
    use crate::types::Empty;
    use crate::compact_numerals::{NumExt, NumOp};

    use super::*;

    #[test]
    fn count_nodes_in_church_n() {
        let mut cn = CountNodes {};

        for i in 0..100 {
            let mut expr = LambdaCalc::new(Term::Var(1), Empty {});
            let num = expr.to_church(i as u32);
            expr.set_root(num);

            eprintln!("{}", expr);
            assert_eq!(expr.traverse(&mut cn), Ok(i*2+3));
        }
    }

    #[test]
    fn count_depth_in_church_n() {
        let mut cn = CountDepth {};

        for i in 0..100 {
            let mut expr = LambdaCalc::new(Term::Var(1), Empty {});
            let num = expr.to_church(i as u32);
            expr.set_root(num);

            eprintln!("{}", expr);
            assert_eq!(expr.traverse(&mut cn), Ok(i+3));
        }
    }

    #[test]
    fn from_compact_to_church() {
        let mut cn = FromCompactToChurch {
            output: LambdaCalc::new(Term::Var(1), Empty {}),
        };

        let mut expr = LambdaCalc::new(Term::Ext(NumExt::Value(5)), Empty {});
        let a = expr.get_last_id();
        let b = expr.add(Term::Ext(NumExt::Value(4)));
        let plus = expr.add(Term::Ext(NumExt::Op(NumOp::Add)));
        let p_a = expr.add(Term::App(plus, a));
        let p_a_b = expr.add(Term::App(p_a, b));
        expr.set_root(p_a_b);

        eprintln!("{}", expr);
        let root = expr.traverse(&mut cn).expect("Error");
        cn.output.set_root(root);
        let mut church_expr = cn.output;
        church_expr.reduce();

        assert_eq!(
            format!("{}", church_expr),
            "(\\a. (\\b. (a (a (a (a (a (a (a (a (a b)))))))))))"
        );
    }
}
