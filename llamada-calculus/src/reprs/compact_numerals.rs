use crate::base_types::Empty;
use crate::expr_result::{EvalInfo, ValueInfo};
use crate::reprs::dense::DenseRepr;
use crate::{derive_expr_from, Evaluable, Expr, ExprResult, Term};

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

#[derive(Copy, Eq, Hash, Debug, Clone, PartialEq, PartialOrd, Ord)]
pub enum PrimType {
    U32,
    Type,
}

#[derive(Copy, Eq, Hash, Debug, Clone, PartialEq, PartialOrd, Ord)]
pub enum Prim {
    U32(u32),
    Type(PrimType),
}

impl std::fmt::Display for Prim {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::U32(val) => write!(f, "{val}")?,
            Self::Type(ty) => write!(f, "{ty:?}")?,
        }
        Ok(())
    }
}

#[derive(Copy, Eq, Hash, Debug, Clone, PartialEq, PartialOrd, Ord)]
pub enum NumExt {
    Value(Prim),
    Op(NumOp),
}

impl Evaluable for NumExt {
    fn info(&self) -> Option<EvalInfo> {
        Some(match self {
            Self::Op(NumOp::Not) => EvalInfo::new(1),
            // TODO: Make numbers act as church numbers (i.e. arity: 2) on lambda terms.
            Self::Value(_) => EvalInfo::new(0),
            _ => EvalInfo::new(2),
        })
    }
}

impl From<u32> for NumExt {
    fn from(value: u32) -> Self {
        Self::Value(Prim::U32(value))
    }
}

impl From<NumOp> for NumExt {
    fn from(value: NumOp) -> Self {
        Self::Op(value)
    }
}

impl std::fmt::Display for NumExt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Value(val) => write!(f, "{val}")?,
            Self::Op(op) => write!(f, "{op:?}")?,
        }
        Ok(())
    }
}

#[derive(Default, Eq, Hash, Debug, Clone, PartialEq, PartialOrd, Ord)]
pub enum NumTyInfo {
    #[default]
    Unknown,
    Var(u32),
    U32,
    Abs(Box<NumTyInfo>, Box<NumTyInfo>),
    App(Box<NumTyInfo>, Box<NumTyInfo>),
}

impl Evaluable for NumTyInfo {
    fn info(&self) -> Option<EvalInfo> {
        None // TBD
    }
}

impl ValueInfo for NumTyInfo {
    fn complete(&self) -> bool {
        false
    }
    fn apply(inner: Option<Self>, arg: Option<Self>) -> Option<Self> {
        match (inner, arg) {
            (Some(inner), Some(arg)) => Some(Self::App(Box::new(inner), Box::new(arg))),
            _ => None,
        }
    }
}

impl std::fmt::Display for NumTyInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::U32 => write!(f, "U32")?,
            Self::Unknown => write!(f, "?")?,
            Self::Var(n) => write!(f, "var_{n}")?,
            Self::Abs(arg, result) => write!(f, "({arg} -> {result})")?,
            Self::App(inner, arg) => write!(f, "({inner}{arg})")?,
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

impl<Meta: Clone + Default + std::fmt::Display + std::fmt::Debug> CompactNumerals<Meta> {
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
            (NumOp::Not, [Prim::U32(a)]) => Prim::U32(!a),
            (NumOp::Mod, [Prim::U32(a), Prim::U32(b)]) => Prim::U32(a % b),
            (NumOp::Div, [Prim::U32(a), Prim::U32(b)]) => Prim::U32(a - b),
            (NumOp::Sub, [Prim::U32(a), Prim::U32(b)]) => Prim::U32(a - b),
            (NumOp::Add, [Prim::U32(a), Prim::U32(b)]) => Prim::U32(a + b),
            (NumOp::Mul, [Prim::U32(a), Prim::U32(b)]) => Prim::U32(a * b),
            (NumOp::And, [Prim::U32(a), Prim::U32(b)]) => Prim::U32(a & b),
            (NumOp::Or, [Prim::U32(a), Prim::U32(b)]) => Prim::U32(a | b),
            _ => return Err(CompactErr::WrongArgs(op, args)),
        };
        Ok(Term::Ext(NumExt::Value(res)))
    }
}

impl<Meta: Clone + Default + std::fmt::Display + std::fmt::Debug> Expr for CompactNumerals<Meta> {
    type Index = usize;
    type Value = NumExt;
    type Meta = Meta;
    // type Term = Term<Self::Value, Self::Index>;

    fn new(term: Term<Self::Value, usize>, meta: Meta) -> Self {
        Self {
            repr: DenseRepr::new(term, meta),
        }
    }

    fn reduce_ext_apps(
        &mut self,
        value: ExprResult<Self::Index, EvalInfo>,
    ) -> ExprResult<Self::Index, EvalInfo> {
        if let Some(ext_info) = &value.ext_info {
            if ext_info.complete() {
                let inner = self.get(&value.id).clone();
                let res = self.eval(inner).expect("Oh no...");
                return ExprResult::unchanged(self.add(res)).changed();
            }
        }
        value
    }

    derive_expr_from!(repr);
}
pub type LambdaCalc = CompactNumerals<Empty>;

impl<Meta> std::fmt::Display for CompactNumerals<Meta>
where
    DenseRepr<NumExt, Meta>: Expr,
    <DenseRepr<NumExt, Meta> as Expr>::Value: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.repr.fmt_root(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ext;
    tests!(CompactNumerals<Empty>);

    #[test]
    fn mul_expr_huge() {
        let mut expr = CompactNumerals::<Empty>::new(ext(NumOp::Mul), Empty {});
        let mul = expr.get_last_id();
        let a = expr.add(Term::Var(2));
        let b = expr.add(Term::Var(1));
        let ma = expr.add(Term::App(mul, a));
        let mab = expr.add(Term::App(ma, b));
        let abs1_mab = expr.add(Term::abs(mab));
        let mul = expr.add(Term::abs(abs1_mab));
        *expr.root_mut() = mul;

        assert_eq!(format!("{}", &expr), "(\\a. (\\b. ((Mul a) b)))");

        for n in 998..1000 {
            for m in 998..1000 {
                let church_n = expr.add(ext(n));
                let church_m = expr.add(ext(m));

                let mul_m = expr.add(Term::App(mul, church_m));
                let mul_n_m = expr.add(Term::App(mul_m, church_n));
                *expr.root_mut() = mul_n_m;
                expr.reduce();
                eprintln!("result: {}", expr.as_context(expr.root()));
                let result = expr.get(expr.root());
                eprintln!("{n:?} * {m:?} = {result:?}");
                assert_eq!(result, &ext(n * m));
            }
        }
    }

    #[test]
    fn add_expr_huge() {
        let mut expr = CompactNumerals::<Empty>::new(ext(NumOp::Add), Empty {});
        let add = expr.get_last_id();
        let a = expr.add(Term::Var(2));
        let b = expr.add(Term::Var(1));
        let ma = expr.add(Term::App(add, a));
        let mab = expr.add(Term::App(ma, b));
        let abs1_mab = expr.add(Term::abs(mab));
        let add = expr.add(Term::abs(abs1_mab));
        *expr.root_mut() = add;

        assert_eq!(format!("{}", &expr), "(\\a. (\\b. ((Add a) b)))");

        for n in 998..1000 {
            for m in 998..1000 {
                let church_n = expr.add(ext(n));
                let church_m = expr.add(ext(m));

                let add_m = expr.add(Term::App(add, church_m));
                let add_n_m = expr.add(Term::App(add_m, church_n));
                *expr.root_mut() = add_n_m;
                expr.reduce();
                eprintln!("result: {}", expr.as_context(expr.root()));
                let result = expr.get(expr.root());
                eprintln!("{n:?} + {m:?} = {result:?}");
                assert_eq!(result, &ext(n + m));
            }
        }
    }

    #[test]
    fn not_expr_huge() {
        let mut expr = CompactNumerals::<Empty>::new(ext(NumOp::Not), Empty {});
        let not = expr.get_last_id();
        let a = expr.add(Term::Var(1));
        let ma = expr.add(Term::App(not, a));
        let not = expr.add(Term::abs(ma));
        *expr.root_mut() = not;

        assert_eq!(format!("{}", &expr), "(\\a. (Not a))");

        for n in 998..1000 {
            let church_n = expr.add(ext(n));

            let not_n = expr.add(Term::App(not, church_n));
            *expr.root_mut() = not_n;
            expr.reduce();
            eprintln!("result: {}", expr.as_context(expr.root()));
            let result = expr.get(expr.root());
            eprintln!("!{n:?} = {result:?}");
            assert_eq!(result, &ext(!n));
        }
    }

    #[test]
    fn or_expr_huge() {
        let mut expr = CompactNumerals::<Empty>::new(ext(NumOp::Or), Empty {});
        let or = expr.get_last_id();
        let a = expr.add(Term::Var(2));
        let b = expr.add(Term::Var(1));
        let ma = expr.add(Term::App(or, a));
        let mab = expr.add(Term::App(ma, b));
        let abs1_mab = expr.add(Term::abs(mab));
        let or = expr.add(Term::abs(abs1_mab));
        *expr.root_mut() = or;

        assert_eq!(format!("{}", &expr), "(\\a. (\\b. ((Or a) b)))");

        for n in 998..1000 {
            for m in 998..1000 {
                let church_n = expr.add(ext(n));
                let church_m = expr.add(ext(m));

                let or_m = expr.add(Term::App(or, church_m));
                let or_n_m = expr.add(Term::App(or_m, church_n));
                *expr.root_mut() = or_n_m;
                expr.reduce();
                eprintln!("result: {}", expr.as_context(expr.root()));
                let result = expr.get(expr.root());
                eprintln!("{n:?} | {m:?} = {result:?}");
                assert_eq!(result, &ext(n | m));
            }
        }
    }

    #[test]
    fn and_expr_huge() {
        let mut expr = CompactNumerals::<Empty>::new(ext(NumOp::And), Empty {});
        let and = expr.get_last_id();
        let a = expr.add(Term::Var(2));
        let b = expr.add(Term::Var(1));
        let ma = expr.add(Term::App(and, a));
        let mab = expr.add(Term::App(ma, b));
        let abs1_mab = expr.add(Term::abs(mab));
        let and = expr.add(Term::abs(abs1_mab));
        *expr.root_mut() = and;

        assert_eq!(format!("{}", &expr), "(\\a. (\\b. ((And a) b)))");

        for n in 998..1000 {
            for m in 998..1000 {
                let church_n = expr.add(ext(n));
                let church_m = expr.add(ext(m));

                let and_m = expr.add(Term::App(and, church_m));
                let and_n_m = expr.add(Term::App(and_m, church_n));
                *expr.root_mut() = and_n_m;
                expr.reduce();
                eprintln!("result: {}", expr.as_context(expr.root()));
                let result = expr.get(expr.root());
                eprintln!("{n:?} & {m:?} = {result:?}");
                assert_eq!(result, &ext(n & m));
            }
        }
    }
}
