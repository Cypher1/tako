use crate::types::{Empty, Never};
use crate::with_context::WithContext;
use crate::{Expr, Term};

#[derive(Debug, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub struct DenseRepr<T, Meta> {
    terms: Vec<(Term<T, usize>, Meta)>,
    // Allows us to add new terms without
    // changing the meaning of the program.
    root: usize,
    print_meta: bool,
}

impl<T, Meta> DenseRepr<T, Meta> {
    // TODO: type Term=Term<T, usize>;
    pub fn new(term: Term<T, usize>, meta: Meta) -> Self {
        let mut this = Self {
            terms: vec![],
            root: 0,
            print_meta: false,
        };
        this.push(term, meta);
        this
    }
    pub fn print_meta(&mut self, print_meta: bool) {
        self.print_meta = print_meta;
    }
    pub fn get_last_id(&self) -> usize {
        self.terms.len() - 1
    }

    pub fn push(&mut self, term: Term<T, usize>, meta: Meta) -> usize {
        self.terms.push((term, meta));
        self.get_last_id()
    }

    pub fn set_root(&mut self, index: usize) {
        assert!(index < self.terms.len());
        self.root = index;
    }

    fn fmt_index<'a>(
        ctx: WithContext<'a, Self, usize>,
        f: &mut std::fmt::Formatter,
    ) -> std::fmt::Result
    where
        WithContext<'a, Self, Term<T, usize>>: std::fmt::Display,
        Meta: std::fmt::Display,
    {
        let id = *ctx.val;
        let Some((term, meta)) = ctx.ctx.terms.get(id) else {
            return write!(f, "<Unknown index {id}")
        };
        write!(f, "{term}", term = ctx.child(term, vec![]))?;
        if ctx.ctx.print_meta {
            write!(f, ": {meta}")?;
        }
        Ok(())
    }

    pub fn as_context<'a, U>(&'a self, val: &'a U) -> WithContext<'a, Self, U> {
        WithContext::new(self, val, vec![])
    }
}

impl<'a, T: std::fmt::Debug, Meta: std::fmt::Display> std::fmt::Display
    for WithContext<'a, DenseRepr<T, Meta>, Term<T, usize>>
where
    WithContext<'a, DenseRepr<T, Meta>, T>: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.val {
            Term::Val(val) => write!(f, "{:?}", val),
            Term::Var(var_id) => {
                let ind = self.names.len().checked_sub(1 + *var_id);
                if let Some(ind) = ind {
                    if let Some(name) = self.names.get(ind) {
                        return write!(f, "{name}");
                    }
                }
                write!(f, "unbound_variable#{var_id:?}")
            }
            Term::App(x, y) => {
                DenseRepr::fmt_index(self.child(x, vec![]), f)?;
                write!(f, " ")?;
                DenseRepr::fmt_index(self.child(y, vec![]), f)
            }
            Term::Abs(ind) => {
                let chr = ((self.names.len() % 26) + ('a' as usize)) as u8 as char;
                let name_ind = self.names.len() / 26;
                let name = format!(
                    "{chr}{}",
                    if name_ind > 0 {
                        format!("{}", name_ind - 1)
                    } else {
                        "".to_string()
                    }
                );
                write!(f, "(\\{name}. ")?;
                DenseRepr::fmt_index(self.child(ind, vec![name]), f)?;
                write!(f, ")")
            }
        }
    }
}

impl<T: std::fmt::Display, Ctx> std::fmt::Display for WithContext<'_, Ctx, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.val)
    }
}

impl<T, Meta: std::fmt::Display> std::fmt::Display for DenseRepr<T, Meta>
where
    for<'a> WithContext<'a, Self, Term<T, usize>>: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        DenseRepr::fmt_index(self.as_context(&self.root), f)
    }
}

impl<Meta> Expr for DenseRepr<Never, Meta> {
    type Index = usize;
    type Value = Never;
    type Meta = Meta;
    type Term = Term<Self::Value, Self::Index>;

    fn get(&self, id: Self::Index) -> &Self::Term {
        // TODO: Checked version?
        &self.terms[id].0
    }
    fn get_mut(&mut self, id: Self::Index) -> &mut Self::Term {
        // TODO: Checked version?
        &mut self.terms[id].0
    }
    fn apply_to_value(&mut self, value: Self::Value, _arg: Self::Term) -> Self::Term {
        match value {}
    }
    fn root(&self) -> &Self::Term {
        self.get(self.root)
    }
    fn root_mut(&mut self) -> &mut Self::Term {
        self.get_mut(self.root)
    }
}

pub type LambdaCalc = DenseRepr<Never, Empty>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn id_expr() {
        let mut expr = LambdaCalc::new(Term::Var(0), Empty {});
        let prev = expr.get_last_id();
        let abs = expr.push(Term::Abs(prev), Empty {});
        expr.set_root(abs);
        expr.print_meta = false;
        assert_eq!(format!("{}", expr), "(\\a. a)");
        let mut expr = expr.reduce();
        assert_eq!(format!("{}", expr), "(\\a. a)");
        assert_eq!(
            format!("{:?}", expr),
            "DenseRepr { terms: [(Var(0), Empty), (Abs(0), Empty)], root: 1, print_meta: false }"
        );
        expr.print_meta = true;
        assert_eq!(
            format!("{:?}", expr),
            "DenseRepr { terms: [(Var(0), Empty), (Abs(0), Empty)], root: 1, print_meta: true }"
        );
        assert_eq!(format!("{}", expr), "(\\a. a: Empty): Empty");
        let expr = expr.reduce();
        assert_eq!(format!("{}", expr), "(\\a. a: Empty): Empty");
    }

    #[test]
    fn true_expr() {
        let mut expr = LambdaCalc::new(Term::Var(1), Empty {});
        let prev = expr.get_last_id();
        let abs1 = expr.push(Term::Abs(prev), Empty {});
        let abs2 = expr.push(Term::Abs(abs1), Empty {});
        expr.set_root(abs2);
        assert_eq!(format!("{}", expr), "(\\a. (\\b. a))");
        let expr = expr.reduce();
        assert_eq!(format!("{}", expr), "(\\a. (\\b. a))");
    }

    #[test]
    fn false_expr() {
        let mut expr = LambdaCalc::new(Term::Var(0), Empty {});
        let prev = expr.get_last_id();
        let abs1 = expr.push(Term::Abs(prev), Empty {});
        let abs2 = expr.push(Term::Abs(abs1), Empty {});
        expr.set_root(abs2);
        assert_eq!(format!("{}", expr), "(\\a. (\\b. b))");
        let expr = expr.reduce();
        assert_eq!(format!("{}", expr), "(\\a. (\\b. b))");
    }

    #[test]
    fn not_expr() {
        let mut expr = LambdaCalc::new(Term::Var(0), Empty {});
        let true_case = expr.get_last_id();
        let false_case = expr.push(Term::Var(1), Empty {});
        let cond_case = expr.push(Term::Var(2), Empty {});
        let app1 = expr.push(Term::App(cond_case, false_case), Empty {});
        let app2 = expr.push(Term::App(app1, true_case), Empty {});
        let abs1 = expr.push(Term::Abs(app2), Empty {});
        let abs2 = expr.push(Term::Abs(abs1), Empty {});
        let abs3 = expr.push(Term::Abs(abs2), Empty {});
        expr.set_root(abs3);
        assert_eq!(format!("{}", expr), "(\\a. (\\b. (\\c. a b c)))");
        let expr = expr.reduce();
        assert_eq!(format!("{}", expr), "(\\a. (\\b. (\\c. a b c)))");
    }

    #[test]
    fn not_true_false_expr() {
        let mut expr = LambdaCalc::new(Term::Var(0), Empty {});
        let inner = {
            let true_case = expr.get_last_id();
            let false_case = expr.push(Term::Var(1), Empty {});
            let cond_case = expr.push(Term::Var(2), Empty {});
            let app1 = expr.push(Term::App(cond_case, false_case), Empty {});
            let app2 = expr.push(Term::App(app1, true_case), Empty {});
            let abs1 = expr.push(Term::Abs(app2), Empty {});
            let abs2 = expr.push(Term::Abs(abs1), Empty {});
            expr.push(Term::Abs(abs2), Empty {})
        };
        let _true_v = {
            let true_case = expr.push(Term::Var(1), Empty {});
            let abs1 = expr.push(Term::Abs(true_case), Empty {});
            expr.push(Term::Abs(abs1), Empty {})
        };
        let false_v = {
            let false_case = expr.push(Term::Var(0), Empty {});
            let abs1 = expr.push(Term::Abs(false_case), Empty {});
            expr.push(Term::Abs(abs1), Empty {})
        };
        let app1 = expr.push(Term::App(inner, false_v), Empty {});
        expr.set_root(app1);
        assert_eq!(format!("{}", expr), "(\\a. (\\b. (\\c. a b c))) (\\a. (\\b. b))");
        let expr = expr.reduce();
        assert_eq!(format!("{}", expr), "(\\a. (\\b. b))");
    }
}
