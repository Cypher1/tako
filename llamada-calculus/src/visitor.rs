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


#[cfg(test)]
mod test {
    use crate::types::Empty;
    use crate::compact_numerals::LambdaCalc;

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
}
