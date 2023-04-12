use crate::types::Empty;
use crate::{Expr, Term};
use crate::dense::DenseRepr;

struct CompactNumerals<Meta> {
    repr: DenseRepr<u32, Meta>,
}

impl<Meta: Default + std::fmt::Display> Expr
    for CompactNumerals<Meta>
{
    type Index = usize;
    type Extension = u32;
    type Meta = Meta;
    // type Term = Term<Self::Extension, Self::Index>;

    fn new(term: Term<Self::Extension, usize>, meta: Meta) -> Self {
        Self {
            repr: DenseRepr::new(term, meta)
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
pub type LambdaCalc = DenseRepr<u32, Empty>;

impl<Meta> std::fmt::Display for CompactNumerals<Meta>
where
    DenseRepr<u32, Meta>: Expr,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.repr.fmt_root(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    tests!(CompactNumerals<Empty>);
}

