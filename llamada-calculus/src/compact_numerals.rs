use crate::dense::DenseRepr;
use crate::types::Empty;
use crate::{EvalInfo, Expr, Term};

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

#[derive(Debug, Eq, PartialEq)]
enum CompactErr {
    WrongArgs(NumOp, Vec<u32>),
    UnevaluatedArg(Term<NumExt, usize>),
}

impl<Meta: Default + std::fmt::Display> CompactNumerals<Meta> {
    fn get_arg(&self, value: Term<NumExt, usize>, args: &mut Vec<u32>) -> Result<(), CompactErr> {
        match value {
            Term::Ext(NumExt::Value(val)) => {
                args.push(val);
                Ok(())
            }
            _ => Err(CompactErr::UnevaluatedArg(value)),
        }
    }

    fn get_op_and_args(
        &self,
        value: Term<NumExt, usize>,
        args: &mut Vec<u32>,
    ) -> Result<NumOp, CompactErr> {
        let mut curr = value;
        loop {
            match curr {
                Term::Ext(NumExt::Op(op)) => return Ok(op),
                Term::App(inner, arg) => {
                    curr = self.repr.get(&inner).clone();
                    let arg = self.repr.get(&arg).clone();
                    self.get_arg(arg, args)?;
                }
                _ => return Err(CompactErr::UnevaluatedArg(curr)),
            }
        }
    }

    fn eval(&self, value: Term<NumExt, usize>) -> Result<Term<NumExt, usize>, CompactErr> {
        let mut args = vec![];
        let op = self.get_op_and_args(value, &mut args)?;
        let res = match (op, &args[..]) {
            (NumOp::Mod, [a, b]) => a % b,
            (NumOp::Div, [a, b]) => a - b,
            (NumOp::Sub, [a, b]) => a - b,
            (NumOp::Add, [a, b]) => a + b,
            (NumOp::Mul, [a, b]) => a * b,
            _ => return Err(CompactErr::WrongArgs(op, args)),
        };
        Ok(Term::Ext(NumExt::Value(res)))
    }
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
    fn reduce_ext_apps(
        &mut self,
        value: Term<Self::Extension, Self::Index>,
    ) -> Term<Self::Extension, Self::Index> {
        self.eval(value).expect("Oh no...")
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
    fn ext_info(&self, ext: Self::Extension) -> Option<EvalInfo> {
        Some(match ext {
            NumExt::Value(_) => EvalInfo::new(0),
            NumExt::Op(op) => match op {
                NumOp::Add | NumOp::Sub | NumOp::Div | NumOp::Mod | NumOp::Mul => EvalInfo::new(2),
            },
        })
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
        *expr.root_mut() = mul;

        assert_eq!(format!("{}", &expr), "(\\a. (\\b. ((Op(Mul) a) b)))");

        for n in 0..1000 {
            for m in 0..1000 {
                let church_n = expr.add(Term::Ext(NumExt::Value(n)));
                let church_m = expr.add(Term::Ext(NumExt::Value(m)));

                let mul_m = expr.add(Term::App(mul, church_m));
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

    #[test]
    fn add_expr_huge() {
        let mut expr = CompactNumerals::<Empty>::new(Term::Ext(NumExt::Op(NumOp::Add)), Empty {});
        let add = expr.get_last_id();
        let a = expr.add(Term::Var(2));
        let b = expr.add(Term::Var(1));
        let ma = expr.add(Term::App(add, a));
        let mab = expr.add(Term::App(ma, b));
        let abs1_mab = expr.add(Term::Abs(mab));
        let add = expr.add(Term::Abs(abs1_mab));
        *expr.root_mut() = add;

        assert_eq!(format!("{}", &expr), "(\\a. (\\b. ((Op(Add) a) b)))");

        for n in 0..1000 {
            for m in 0..1000 {
                let church_n = expr.add(Term::Ext(NumExt::Value(n)));
                let church_m = expr.add(Term::Ext(NumExt::Value(m)));

                let add_m = expr.add(Term::App(add, church_m));
                let add_n_m = expr.add(Term::App(add_m, church_n));
                *expr.root_mut() = add_n_m;
                expr.reduce();
                eprintln!("result: {}", expr.as_context(expr.root()));
                let result = expr.get(expr.root());
                eprintln!("{n:?} * {m:?} = {result:?}");
                assert_eq!(result, &Term::Ext(NumExt::Value(n + m)));
            }
        }
    }
}
