use crate::types::{Empty, Never};
use crate::{Expr, Term};

#[derive(Debug, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub struct Ptr<T, Meta> {
    val: Box<Term<T, Ptr<T, Meta>>>,
    meta: Meta,
}

impl<T, Meta> Ptr<T, Meta> {
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

impl<T, Meta> std::fmt::Display for SparseRepr<T, Meta>
where
    SparseRepr<T, Meta>: Expr,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.fmt_root(f)
    }
}

impl<
        T: Eq + Clone + std::fmt::Debug + std::fmt::Display,
        Meta: Clone + Eq + Default + std::fmt::Display + std::fmt::Debug,
    > Expr for SparseRepr<T, Meta>
{
    type Index = Ptr<T, Meta>;
    type Extension = T;
    type Meta = Meta;
    // type Term = Term<Self::Extension, Self::Index>;

    fn new(term: Term<T, Ptr<T, Meta>>, meta: Meta) -> Self {
        Self {
            terms: vec![Ptr::new(term, meta)],
            root: 0,
            print_meta: false,
        }
    }
    fn get_last_id(&self) -> Self::Index {
        self.terms.last().unwrap().clone()
    }
    fn get<'a>(&'a self, id: &'a Self::Index) -> &'a Term<Self::Extension, Self::Index> {
        // TODO: Checked version?
        &id.val
    }
    fn get_meta<'a>(&'a self, id: &'a Self::Index) -> &'a Self::Meta {
        // TODO: Checked version?
        &id.meta
    }
    fn reduce_ext_apps(
        &mut self,
        _value: Term<Self::Extension, Self::Index>,
    ) -> Term<Self::Extension, Self::Index> {
        todo!(); // match value {}
    }
    fn root(&self) -> &Self::Index {
        &self.terms[self.root]
    }
    fn root_mut(&mut self) -> &mut Self::Index {
        &mut self.terms[self.root]
    }
    fn add(&mut self, term: Term<Self::Extension, Self::Index>) -> Self::Index {
        let meta = Self::Meta::default();
        Ptr::new(term, meta)
    }
    fn print_meta(&self) -> bool {
        self.print_meta
    }
    fn set_print_meta(&mut self, print_meta: bool) {
        self.print_meta = print_meta;
    }
}
pub type LambdaCalc = SparseRepr<Never, Empty>;

#[cfg(test)]
mod tests {
    use super::*;
    tests!(LambdaCalc);
}
