use crate::dense::DenseRepr;
use crate::types::Empty;
use crate::{Expr, Term};

#[derive(Copy, Eq, Hash, Debug, Clone, PartialEq, PartialOrd, Ord)]
pub enum NumOp {
    Add,
    Mul,
    Sub,
    Div,
    Mod,
}

#[derive(Copy, Eq, Hash, Debug, Clone, PartialEq, PartialOrd, Ord)]
pub enum NumExt {
    Value(u32),
    Op(NumOp),
}

impl std::fmt::Display for NumExt {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            NumExt::Value(val) => write!(f, "{val}")?,
            NumExt::Op(op) => write!(f, "{op:?}")?,
        }
        Ok(())
    }
}

struct CompactNumerals<Meta> {
    repr: DenseRepr<NumExt, Meta>,
}

impl<Meta: Default + std::fmt::Display> Expr for CompactNumerals<Meta> {
    type Index = usize;
    type Extension = NumExt;
    type Meta = Meta;
    // type Term = Term<Self::Extension, Self::Index>;

    fn new(term: Term<Self::Extension, usize>, meta: Meta) -> Self {
        Self {
            repr: DenseRepr::new(term, meta),
        }
    }
    fn get<'a>(&'a self, id: &'a Self::Index) -> &Term<Self::Extension, Self::Index> {
        self.repr.get(id)
    }
    fn get_meta<'a>(&'a self, id: &'a Self::Index) -> &Self::Meta {
        self.repr.get_meta(id)
    }
    fn get_last_id(&self) -> Self::Index {
        self.repr.get_last_id()
    }
    fn apply_to_value(
        &mut self,
        _value: Self::Extension,
        _arg: Term<Self::Extension, Self::Index>,
    ) -> Term<Self::Extension, Self::Index> {
        todo!(); // match value {}
    }
    fn root(&self) -> &Self::Index {
        self.repr.root()
    }
    fn root_mut(&mut self) -> &mut Self::Index {
        self.repr.root_mut()
    }
    fn add(&mut self, term: Term<Self::Extension, Self::Index>) -> Self::Index {
        self.repr.add(term)
    }
    fn print_meta(&self) -> bool {
        self.repr.print_meta()
    }
    fn set_print_meta(&mut self, print_meta: bool) {
        self.repr.set_print_meta(print_meta);
    }
}
pub type LambdaCalc = DenseRepr<NumExt, Empty>;

impl<Meta> std::fmt::Display for CompactNumerals<Meta>
where
    DenseRepr<NumExt, Meta>: Expr,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.repr.fmt_root(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    tests!(CompactNumerals<Empty>);

    #[test]
    fn mul_expr_huge() {
        let mut expr = CompactNumerals::<Empty>::new(Term::Ext(NumExt::Op(NumOp::Mul)), Empty {});
        let mul = expr.get_last_id();
        let a = expr.add(Term::Var(2));
        let b = expr.add(Term::Var(1));
        let ma = expr.add(Term::App(mul, a));
        let mab = expr.add(Term::App(ma, b));
        let abs1_mab = expr.add(Term::Abs(mab));
        let mul = expr.add(Term::Abs(abs1_mab));
        *expr.root_mut() = mul.clone();

        assert_eq!(
            format!("{}", &expr),
            "(\\a. (\\b. ((Op(Mul) a) b)))"
        );

        for n in 990..1000 {
            for m in 990..1000 {
                let church_n = expr.add(Term::Ext(NumExt::Value(n)));
                let church_m = expr.add(Term::Ext(NumExt::Value(m)));

                let mul_m = expr.add(Term::App(mul.clone(), church_m));
                let mul_n_m = expr.add(Term::App(mul_m, church_n));
                *expr.root_mut() = mul_n_m;
                expr.reduce();
                eprintln!("result: {}", expr.as_context(expr.root()));
                let result = expr.get(expr.root());
                eprintln!("{n:?} * {m:?} = {result:?}");
                assert_eq!(result, &Term::Ext(NumExt::Value(n * m)));
            }
        }
    }
}
