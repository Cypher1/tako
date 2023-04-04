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
}

impl<T: Clone + std::fmt::Debug + std::fmt::Display, Meta: Default + std::fmt::Display> std::fmt::Display for DenseRepr<T, Meta>
where
    for<'a> WithContext<'a, Self, Term<T, usize>>: std::fmt::Display,
    Self: Sized,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        DenseRepr::fmt_index(Expr::as_context(&self, &self.root), f)
    }
}

impl<T: Clone + std::fmt::Debug + std::fmt::Display, Meta: Default + std::fmt::Display> Expr for DenseRepr<T, Meta> {
    type Index = usize;
    type Value = T;
    type Meta = Meta;
    // type Term = Term<Self::Value, Self::Index>;

    fn get(&self, id: &Self::Index) -> &Term<Self::Value, Self::Index> {
        // TODO: Checked version?
        &self.terms[*id].0
    }
    fn get_mut(&mut self, id: &mut Self::Index) -> &mut Term<Self::Value, Self::Index> {
        // TODO: Checked version?
        &mut self.terms[*id].0
    }
    fn get_meta(&self, id: &Self::Index) -> &Self::Meta {
        // TODO: Checked version?
        &self.terms[*id].1
    }
    fn get_meta_mut(&mut self, id: &mut Self::Index) -> &mut Self::Meta {
        // TODO: Checked version?
        &mut self.terms[*id].1
    }
    fn apply_to_value(&mut self, _value: Self::Value, _arg: Term<Self::Value, Self::Index>) -> Term<Self::Value, Self::Index> {
        todo!(); // match value {}
    }
    fn root(&self) -> &Self::Index {
        &self.root
    }
    fn root_mut(&mut self) -> &mut Self::Index {
        &mut self.root
    }
    fn add(&mut self, term: Term<Self::Value, Self::Index>, meta: Meta) -> Self::Index {
        self.push(term, meta)
    }

    fn new_meta(&mut self) -> Self::Meta {
        Self::Meta::default()
    }
    fn print_meta(&self) -> bool {
        self.print_meta
    }
}
pub type LambdaCalc = DenseRepr<Never, Empty>;

#[cfg(test)]
mod tests {
    use super::*;
    use super::Expr;

    #[test]
    fn id_expr() {
        let mut expr = LambdaCalc::new(Term::Var(1), Empty {});
        let prev = expr.get_last_id();
        let abs = expr.push(Term::Abs(prev), Empty {});
        expr.set_root(abs);
        expr.print_meta = false;
        assert_eq!(format!("{}", expr), "(\\a. a)");
        expr.reduce();
        assert_eq!(format!("{}", expr), "(\\a. a)");
        assert_eq!(
            format!("{:?}", &expr),
            "DenseRepr { terms: [(Var(1), Empty), (Abs(0), Empty)], root: 1, print_meta: false }"
        );
        expr.print_meta = true;
        assert_eq!(
            format!("{:?}", &expr),
            "DenseRepr { terms: [(Var(1), Empty), (Abs(0), Empty)], root: 1, print_meta: true }"
        );
        assert_eq!(format!("{}", &expr), "(\\a. a: Empty): Empty");
        expr.reduce();
        assert_eq!(format!("{}", &expr), "(\\a. a: Empty): Empty");
    }

    #[test]
    fn true_expr() {
        let mut expr = LambdaCalc::new(Term::Var(2), Empty {});
        let prev = expr.get_last_id();
        let abs1 = expr.push(Term::Abs(prev), Empty {});
        let abs2 = expr.push(Term::Abs(abs1), Empty {});
        expr.set_root(abs2);
        assert_eq!(format!("{}", &expr), "(\\a. (\\b. a))");
        expr.reduce();
        assert_eq!(format!("{}", &expr), "(\\a. (\\b. a))");
    }

    #[test]
    fn false_expr() {
        let mut expr = LambdaCalc::new(Term::Var(1), Empty {});
        let prev = expr.get_last_id();
        let abs1 = expr.push(Term::Abs(prev), Empty {});
        let abs2 = expr.push(Term::Abs(abs1), Empty {});
        expr.set_root(abs2);
        assert_eq!(format!("{}", &expr), "(\\a. (\\b. b))");
        expr.reduce();
        assert_eq!(format!("{}", &expr), "(\\a. (\\b. b))");
    }

    #[test]
    fn not_expr() {
        let mut expr = LambdaCalc::new(Term::Var(1), Empty {});
        let true_case = expr.get_last_id();
        let false_case = expr.push(Term::Var(2), Empty {});
        let cond_case = expr.push(Term::Var(3), Empty {});
        let app1 = expr.push(Term::App(cond_case, false_case), Empty {});
        let app2 = expr.push(Term::App(app1, true_case), Empty {});
        let abs1 = expr.push(Term::Abs(app2), Empty {});
        let abs2 = expr.push(Term::Abs(abs1), Empty {});
        let abs3 = expr.push(Term::Abs(abs2), Empty {});
        expr.set_root(abs3);
        assert_eq!(format!("{}", &expr), "(\\a. (\\b. (\\c. a b c)))");
        expr.reduce();
        assert_eq!(format!("{}", &expr), "(\\a. (\\b. (\\c. a b c)))");
    }

    #[test]
    fn not_true_false_expr() {
        let mut expr = LambdaCalc::new(Term::Var(1), Empty {});
        let inner = {
            let true_case = expr.get_last_id();
            let false_case = expr.push(Term::Var(2), Empty {});
            let cond_case = expr.push(Term::Var(3), Empty {});
            let app1 = expr.push(Term::App(cond_case, false_case), Empty {});
            let app2 = expr.push(Term::App(app1, true_case), Empty {});
            let abs1 = expr.push(Term::Abs(app2), Empty {});
            let abs2 = expr.push(Term::Abs(abs1), Empty {});
            expr.push(Term::Abs(abs2), Empty {})
        };
        let _true_v = {
            let true_case = expr.push(Term::Var(2), Empty {});
            let abs1 = expr.push(Term::Abs(true_case), Empty {});
            expr.push(Term::Abs(abs1), Empty {})
        };
        let false_v = {
            let false_case = expr.push(Term::Var(1), Empty {});
            let abs1 = expr.push(Term::Abs(false_case), Empty {});
            expr.push(Term::Abs(abs1), Empty {})
        };
        let app1 = expr.push(Term::App(inner, false_v), Empty {});
        expr.set_root(app1);
        assert_eq!(format!("{}", expr), "(\\a. (\\b. (\\c. a b c))) (\\a. (\\b. b))");
        expr.reduce();
        assert_eq!(format!("{}", expr), "(\\a. (\\b. b))");
    }
}
