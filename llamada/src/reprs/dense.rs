use crate::base_types::{Empty, Never};
use crate::{Evaluable, Expr, Term};

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

impl<T: Evaluable + Clone + std::fmt::Display, Meta: Clone + Default + std::fmt::Display> Expr
    for DenseRepr<T, Meta>
{
    type Index = usize;
    type Value = T;
    type Meta = Meta;
    // type Term = Term<Self::Value, Self::Index>;

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
    fn root(&self) -> &Self::Index {
        &self.root
    }
    fn root_mut(&mut self) -> &mut Self::Index {
        &mut self.root
    }
    fn add_with_meta(
        &mut self,
        term: Term<Self::Value, Self::Index>,
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
    Self: Expr,
    <Self as Expr>::Value: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_root(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    tests!(LambdaCalc);
}
