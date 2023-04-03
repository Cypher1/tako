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
    // type Term; // =Term<Self::Value, Self::Index>;
    type Meta; // =();

    fn get(&self, id: &Self::Index) -> &Term<Self::Value, Self::Index>;
    fn get_mut(&mut self, id: &mut Self::Index) -> &mut Term<Self::Value, Self::Index>;

    fn root(&self) -> &Self::Index;
    fn root_mut(&mut self) -> &mut Self::Index;

    fn subst(&mut self, id: Self::Index, _val: Term<Self::Value, Self::Index>) -> Self::Index {
        // TODO: What...
        id
    }

    fn reduce_at(&mut self, id: &mut Self::Index, bindings: &Vec<Term<Self::Value, Self::Index>>)
    where
        Self: Sized,
        Self::Value: Clone,
        Self::Index: Clone,
        Term<Self::Value, Self::Index>: std::fmt::Debug + Clone,
    {
        let mut curr = self.get(id).clone();
        eprintln!("{:?}", curr);
        match &mut curr {
            Term::Val(_) => {},
            Term::Var(_) => {},
            Term::Abs(inner) => {
                self.reduce_at(inner, bindings);
            }
            Term::App(inner, arg) => {
                self.reduce_at(inner, &bindings);
                self.reduce_at(arg, &bindings);
            }
        }
        // Reassign?
        let new_curr = self.get_mut(id);
        *new_curr = curr;
    }
    fn reduce(mut self) -> Self
    where
        Self: Sized,
        Self::Value: Clone,
        Self::Index: Clone,
        Term<Self::Value, Self::Index>: std::fmt::Debug + Clone,
    {
        // TODO: Beta and Eta reduction.
        let mut root = self.root().clone();
        self.reduce_at(&mut root, &vec![]);
        // assign new root:
        *self.root_mut() = root;
        self
    }

    fn apply_to_value(&mut self, value: Self::Value, _arg: Term<Self::Value, Self::Index>) -> Term<Self::Value, Self::Index>;
}
