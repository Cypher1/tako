#![feature(never_type, exhaustive_patterns)]

#[derive(Debug, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub enum Term<T, Id=Box<Term<T>> {
    Var(u32),
    App(Id, Id),
    Abs(Id),
    Val(T),
}

trait Expr {
    type Index;
    type Value=!;
    type Term=Term<Self::Value, Self::Index>;
    type Meta=();

    fn get(&self, id: Self::Index) -> &Self::Term;
    fn get_mut(&mut self, id: Self::Index) -> &mut Self::Term;

    fn reduce(self) -> Self {
        // TODO: Beta and Eta reduction.
        self
    }

    fn applyToValue(&mut self, value: Self::Value, arg: Self::Term) -> Self::Term {
        match value {
            // Nada.
        }
    }
}

struct DenseRepr<T, Meta> {
    terms: Vec<(Term<T, usize>, Meta)>,
    // Allows us to add new terms without
    // changing the meaning of the program.
    root: usize,
}

type LambdaCalc = DenseRepr<!, ()>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
