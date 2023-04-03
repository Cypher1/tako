pub mod dense;
pub mod types;
pub mod with_context;
pub use dense::{DenseRepr, LambdaCalc};

#[derive(Debug, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub enum Term<T, Id> {
    Var(usize),
    App(Id, Id),
    Abs(Id),
    Val(T),
}

pub trait Expr {
    type Index;
    type Value; // =!;
    type Term; // =Term<Self::Value, Self::Index>;
    type Meta; // =();

    fn get(&self, id: Self::Index) -> &Self::Term;
    fn get_mut(&mut self, id: Self::Index) -> &mut Self::Term;

    fn root(&self) -> &Self::Term;
    fn root_mut(&mut self) -> &mut Self::Term;

    fn subst(&mut self, id: Self::Index, _val: Self::Term) -> Self::Index {
        // TODO: What...
        id
    }

    fn reduce(self) -> Self
    where
        Self: Sized,
    {
        // TODO: Beta and Eta reduction.
        let mut stack = vec![self.root()];
        while let Some(_curr) = stack.pop() {
            
        }
        self
    }

    fn apply_to_value(&mut self, value: Self::Value, _arg: Self::Term) -> Self::Term;
}
