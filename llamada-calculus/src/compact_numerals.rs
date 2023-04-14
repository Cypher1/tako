use crate::dense::DenseRepr;
use crate::types::Empty;
use crate::{derive_expr_from, EvalInfo, Expr, Term};

#[derive(Copy, Eq, Hash, Debug, Clone, PartialEq, PartialOrd, Ord)]
pub enum NumOp {
    Not,
    And,
    Or,
    Add,
    Mul,
    Sub,
    Div,
    Mod,
}

type Prim = u32;

#[derive(Copy, Eq, Hash, Debug, Clone, PartialEq, PartialOrd, Ord)]
pub enum NumExt {
    Value(Prim),
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

#[derive(Debug, Clone)]
pub struct CompactNumerals<Meta> {
    repr: DenseRepr<NumExt, Meta>,
}

#[derive(Debug, Eq, PartialEq)]
enum CompactErr {
    WrongArgs(NumOp, Vec<Prim>),
    UnevaluatedArg(Term<NumExt, usize>),
}

impl<Meta: Default + std::fmt::Display> CompactNumerals<Meta> {
    fn get_arg(&self, value: Term<NumExt, usize>, args: &mut Vec<Prim>) -> Result<(), CompactErr> {
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
        args: &mut Vec<Prim>,
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
            (NumOp::Not, [a]) => !a,
            (NumOp::Mod, [a, b]) => a % b,
            (NumOp::Div, [a, b]) => a - b,
            (NumOp::Sub, [a, b]) => a - b,
            (NumOp::Add, [a, b]) => a + b,
            (NumOp::Mul, [a, b]) => a * b,
            (NumOp::And, [a, b]) => a & b,
            (NumOp::Or, [a, b]) => a | b,
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

    fn reduce_ext_apps(
        &mut self,
        value: Term<Self::Extension, Self::Index>,
    ) -> Term<Self::Extension, Self::Index> {
        self.eval(value).expect("Oh no...")
    }

    fn ext_info(&self, ext: &Self::Extension) -> Option<EvalInfo> {
        Some(match ext {
            NumExt::Op(NumOp::Not) => EvalInfo::new(1),
            // TODO: Make numbers act as church numbers (i.e. arity: 2) on lambda terms.
            NumExt::Value(_) => EvalInfo::new(0),
            NumExt::Op(NumOp::Not) => EvalInfo::new(1),
            _ => EvalInfo::new(2),
        })
    }

    derive_expr_from!(repr);
}
pub type LambdaCalc = CompactNumerals<Empty>;

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

        assert_eq!(format!("{}", &expr), "(\\a. (\\b. ((Mul a) b)))");

        for n in 998..1000 {
            for m in 998..1000 {
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

        assert_eq!(format!("{}", &expr), "(\\a. (\\b. ((Add a) b)))");

        for n in 998..1000 {
            for m in 998..1000 {
                let church_n = expr.add(Term::Ext(NumExt::Value(n)));
                let church_m = expr.add(Term::Ext(NumExt::Value(m)));

                let add_m = expr.add(Term::App(add, church_m));
                let add_n_m = expr.add(Term::App(add_m, church_n));
                *expr.root_mut() = add_n_m;
                expr.reduce();
                eprintln!("result: {}", expr.as_context(expr.root()));
                let result = expr.get(expr.root());
                eprintln!("{n:?} + {m:?} = {result:?}");
                assert_eq!(result, &Term::Ext(NumExt::Value(n + m)));
            }
        }
    }

    #[test]
    fn not_expr_huge() {
        let mut expr = CompactNumerals::<Empty>::new(Term::Ext(NumExt::Op(NumOp::Not)), Empty {});
        let not = expr.get_last_id();
        let a = expr.add(Term::Var(1));
        let ma = expr.add(Term::App(not, a));
        let not = expr.add(Term::Abs(ma));
        *expr.root_mut() = not;

        assert_eq!(format!("{}", &expr), "(\\a. (Not a))");

        for n in 998..1000 {
            let church_n = expr.add(Term::Ext(NumExt::Value(n)));

            let not_n = expr.add(Term::App(not, church_n));
            *expr.root_mut() = not_n;
            expr.reduce();
            eprintln!("result: {}", expr.as_context(expr.root()));
            let result = expr.get(expr.root());
            eprintln!("!{n:?} = {result:?}");
            assert_eq!(result, &Term::Ext(NumExt::Value(!n)));
        }
    }

    #[test]
    fn or_expr_huge() {
        let mut expr = CompactNumerals::<Empty>::new(Term::Ext(NumExt::Op(NumOp::Or)), Empty {});
        let or = expr.get_last_id();
        let a = expr.add(Term::Var(2));
        let b = expr.add(Term::Var(1));
        let ma = expr.add(Term::App(or, a));
        let mab = expr.add(Term::App(ma, b));
        let abs1_mab = expr.add(Term::Abs(mab));
        let or = expr.add(Term::Abs(abs1_mab));
        *expr.root_mut() = or;

        assert_eq!(format!("{}", &expr), "(\\a. (\\b. ((Or a) b)))");

        for n in 998..1000 {
            for m in 998..1000 {
                let church_n = expr.add(Term::Ext(NumExt::Value(n)));
                let church_m = expr.add(Term::Ext(NumExt::Value(m)));

                let or_m = expr.add(Term::App(or, church_m));
                let or_n_m = expr.add(Term::App(or_m, church_n));
                *expr.root_mut() = or_n_m;
                expr.reduce();
                eprintln!("result: {}", expr.as_context(expr.root()));
                let result = expr.get(expr.root());
                eprintln!("{n:?} | {m:?} = {result:?}");
                assert_eq!(result, &Term::Ext(NumExt::Value(n | m)));
            }
        }
    }

    #[test]
    fn and_expr_huge() {
        let mut expr = CompactNumerals::<Empty>::new(Term::Ext(NumExt::Op(NumOp::And)), Empty {});
        let and = expr.get_last_id();
        let a = expr.add(Term::Var(2));
        let b = expr.add(Term::Var(1));
        let ma = expr.add(Term::App(and, a));
        let mab = expr.add(Term::App(ma, b));
        let abs1_mab = expr.add(Term::Abs(mab));
        let and = expr.add(Term::Abs(abs1_mab));
        *expr.root_mut() = and;

        assert_eq!(format!("{}", &expr), "(\\a. (\\b. ((And a) b)))");

        for n in 998..1000 {
            for m in 998..1000 {
                let church_n = expr.add(Term::Ext(NumExt::Value(n)));
                let church_m = expr.add(Term::Ext(NumExt::Value(m)));

                let and_m = expr.add(Term::App(and, church_m));
                let and_n_m = expr.add(Term::App(and_m, church_n));
                *expr.root_mut() = and_n_m;
                expr.reduce();
                eprintln!("result: {}", expr.as_context(expr.root()));
                let result = expr.get(expr.root());
                eprintln!("{n:?} & {m:?} = {result:?}");
                assert_eq!(result, &Term::Ext(NumExt::Value(n & m)));
            }
        }
    }
}
