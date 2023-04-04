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
    type Index: Clone + Eq + PartialEq;
    type Value; // =!;
    // type Term; // =Term<Self::Value, Self::Index>;
    type Meta; // =();

    fn get(&self, id: &Self::Index) -> &Term<Self::Value, Self::Index>;
    fn get_mut(&mut self, id: &mut Self::Index) -> &mut Term<Self::Value, Self::Index>;

    fn root(&self) -> &Self::Index;
    fn root_mut(&mut self) -> &mut Self::Index;
    fn new_meta(&mut self) -> Self::Meta;

    fn add(&mut self, term: Term<Self::Value, Self::Index>, meta: Self::Meta) -> Self::Index;

    fn shift(&mut self, id: &Self::Index, depth: usize, delta: i64) -> Self::Index {
        match self.get(&id) {
            Term::Val(_) => id.clone(),
            Term::Var(d) if *d < depth => {
                self.add(Term::Var((*d as i64 +delta)as usize), self.new_meta())
            }
            Term::Var(_) => id.clone(),
            Term::Abs(inner) => {
                let new_inner = self.shift(inner, depth+1, delta);
                if &new_inner != inner {
                    self.add(Term::Abs(new_inner), self.new_meta())
                } else {
                    id.clone()
                }
            }
            Term::App(inner, arg) => {
                let new_inner = self.shift(inner, depth, delta);
                let new_arg = self.shift(arg, depth, delta);
                if &new_inner != inner || &new_arg != arg {
                    self.add(Term::App(new_inner, new_arg), self.new_meta())
                } else {
                    id.clone()
                }
            }
        }
    }
    fn subst(&mut self, id: &Self::Index, val: &Self::Index, depth: usize) -> Self::Index {
        // TODO: What...
        match self.get(&id) {
            Term::Val(_) => id.clone(),
            Term::Var(d) if *d == depth => {
                // Create a shifted version!
                let new_ind = self.shift(val, 0, depth as i64);
                new_ind
            }
            Term::Var(_) => id.clone(),
            Term::Abs(inner) => {
                let new_inner = self.subst(inner, val, depth+1);
                if &new_inner != inner {
                    self.add(Term::Abs(new_inner), self.new_meta())
                } else {
                    id.clone()
                }
            }
            Term::App(inner, arg) => {
                let new_inner = self.subst(inner, val, depth);
                let new_arg = self.subst(arg, val, depth);
                if &new_inner != inner || &new_arg != arg {
                    self.add(Term::App(new_inner, new_arg), self.new_meta())
                } else {
                    id.clone()
                }
            }
        }
    }

    fn reduce_at(&mut self, id: &Self::Index) -> Self::Index
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
                *inner = self.reduce_at(inner);
            }
            Term::App(inner, arg) => {
                *arg = self.reduce_at(arg);
                *inner = self.reduce_at(inner);
                if let Term::Abs(inner) = self.get(inner) {
                    // Beta reduction.
                    let inner = self.shift(inner, 0, -1);
                    return self.subst(&inner, arg, 0);
                }
                //if let Term::Val(val) = self.get(inner) {
                // TOdO
                //}
            }
        }
        id.clone()
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
        self.reduce_at(&mut root);
        // assign new root:
        *self.root_mut() = root;
        self
    }

    fn apply_to_value(&mut self, value: Self::Value, _arg: Term<Self::Value, Self::Index>) -> Term<Self::Value, Self::Index>;
}
