use crate::base_types::Never;
use crate::{Expr, Visitor};

pub struct CountDepth {}

impl<Over: Expr> Visitor<Never, Over> for CountDepth {
    type Type = usize; // TODO: Remove when item default associated types are standardized.
    fn start_value(&mut self) -> usize {
        0
    }

    fn on_ext(
        &mut self,
        _ctx: &Over,
        _ext: &Over::Value,
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
        _arg_meta: &Option<Over::Index>,
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

#[cfg(test)]
mod test {
    use super::*;
    use crate::base_types::Empty;
    use crate::{LambdaCalc, Term};

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
}
