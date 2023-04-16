use crate::types::{Empty, Never};
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
    fn get_last_id(&self) -> usize {
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

impl<T: Clone + std::fmt::Debug + std::fmt::Display, Meta: Default + std::fmt::Display> Expr
    for DenseRepr<T, Meta>
{
    type Index = usize;
    type Extension = T;
    type Meta = Meta;
    // type Term = Term<Self::Extension, Self::Index>;

    fn new(term: Term<T, usize>, meta: Meta) -> Self {
        let mut this = Self {
            terms: vec![],
            root: 0,
            print_meta: false,
        };
        this.push(term, meta);
        this
    }
    fn get_last_id(&self) -> Self::Index {
        self.terms.len() - 1
    }
    fn get(&self, id: &Self::Index) -> &Term<Self::Extension, Self::Index> {
        // TODO: Checked version?
        &self.terms[*id].0
    }
    fn get_meta(&self, id: &Self::Index) -> &Self::Meta {
        // TODO: Checked version?
        &self.terms[*id].1
    }
    fn reduce_ext_apps(
        &mut self,
        _value: Term<Self::Extension, Self::Index>,
    ) -> Term<Self::Extension, Self::Index> {
        todo!(); // match value {}
    }
    fn root(&self) -> &Self::Index {
        &self.root
    }
    fn root_mut(&mut self) -> &mut Self::Index {
        &mut self.root
    }
    fn add_with_meta(
        &mut self,
        term: Term<Self::Extension, Self::Index>,
        meta: Self::Meta,
    ) -> Self::Index {
        self.push(term, meta)
    }
    fn print_meta(&self) -> bool {
        self.print_meta
    }
    fn set_print_meta(&mut self, print_meta: bool) {
        self.print_meta = print_meta;
    }
}
pub type LambdaCalc = DenseRepr<Never, Empty>;

impl<T, Meta> std::fmt::Display for DenseRepr<T, Meta>
where
    DenseRepr<T, Meta>: Expr,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.fmt_root(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    tests!(LambdaCalc);
}
