use crate::types::{Empty, Never};
use crate::with_context::WithContext;
use crate::{Expr, Term};

#[derive(Debug, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub struct Ptr<T, Meta> {
    val: Box<Term<T, Ptr<T, Meta>>>,
    meta: Meta,
}

impl <T, Meta> Ptr<T, Meta> {
    pub fn new(term: Term<T, Ptr<T, Meta>>, meta: Meta) -> Self {
        Ptr {
            val: Box::new(term),
            meta,
        }
    }
}

#[derive(Debug, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub struct SparseRepr<T, Meta> {
    terms: Vec<Ptr<T, Meta>>,
    root: usize,
    print_meta: bool,
}

impl<T, Meta> SparseRepr<T, Meta> {
    // TODO: type Term=Term<T, usize>;
    pub fn get_last_id(&mut self) -> Ptr<T, Meta> {
        self.terms.pop().unwrap()
    }

    pub fn push(&mut self, term: Term<T, Ptr<T, Meta>>, meta: Meta) -> Ptr<T, Meta> {
        Ptr::new(term, meta)
    }

    pub fn set_root(&mut self, index: Ptr<T, Meta>) {
        self.terms.push(index);
        self.root = self.terms.len() - 1
    }
}

impl<T: Eq + Clone + std::fmt::Debug + std::fmt::Display, Meta: Clone + Eq + Default + std::fmt::Display + std::fmt::Debug> std::fmt::Display for SparseRepr<T, Meta>
where
    for<'a> WithContext<'a, Self, Term<T, Ptr<T, Meta>>>: std::fmt::Display,
    Self: Sized,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        SparseRepr::fmt_index(&Expr::as_context(self, &self.root()), f)
    }
}

impl<T: Eq + Clone + std::fmt::Debug + std::fmt::Display, Meta: Clone + Eq + Default + std::fmt::Display + std::fmt::Debug> Expr for SparseRepr<T, Meta> {
    type Index = Ptr<T, Meta>;
    type Value = T;
    type Meta = Meta;
    // type Term = Term<Self::Value, Self::Index>;

    fn new(term: Term<T, Ptr<T, Meta>>, meta: Meta) -> Self {
        Self {
            terms: vec![
                Ptr::new(term, meta),
            ],
            root: 0,
            print_meta: false,
        }
    }
    fn get<'a>(&'a self, id: &'a Self::Index) -> &'a Term<Self::Value, Self::Index> {
        // TODO: Checked version?
        &id.val
    }
    fn get_mut<'a>(&'a mut self, id: &'a mut Self::Index) -> &'a mut Term<Self::Value, Self::Index> {
        // TODO: Checked version?
        &mut id.val
    }
    fn get_meta<'a>(&'a self, id: &'a Self::Index) -> &'a Self::Meta {
        // TODO: Checked version?
        &id.meta
    }
    fn get_meta_mut<'a>(&'a mut self, id: &'a mut Self::Index) -> &'a mut Self::Meta {
        // TODO: Checked version?
        &mut id.meta
    }
    fn apply_to_value(&mut self, _value: Self::Value, _arg: Term<Self::Value, Self::Index>) -> Term<Self::Value, Self::Index> {
        todo!(); // match value {}
    }
    fn root(&self) -> &Self::Index {
        &self.terms[self.root]
    }
    fn root_mut(&mut self) -> &mut Self::Index {
        &mut self.terms[self.root]
    }
    fn add(&mut self, term: Term<Self::Value, Self::Index>, meta: Meta) -> Self::Index {
        Ptr::new(term, meta)
    }

    fn new_meta(&mut self) -> Self::Meta {
        Self::Meta::default()
    }
    fn print_meta(&self) -> bool {
        self.print_meta
    }
}
pub type LambdaCalc = SparseRepr<Never, Empty>;

#[cfg(test)]
mod tests {
    use super::*;
    tests!(LambdaCalc);
}
