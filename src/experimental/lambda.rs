
type Delta = i32;
type Ind = Delta; // Should be unsigned

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Debug)]
pub enum Term {
    Var {ind: Ind},
    App {inner: Box<Term>, arg: Box<Term>},
    Abs {inner: Box<Term>},
}
use Term::*;

fn var(ind: Ind) -> Term {
    Term::Var{ind}
}

fn app(inner: Term, arg: Term) -> Term {
    Term::App{inner: Box::new(inner), arg: Box::new(arg)}
}

fn abs(inner: Term) -> Term {
    Term::Abs{inner: Box::new(inner)}
}

impl Term {

    fn shift(&self, delta: Delta) -> Term {
        self.shift_with_cutoff(delta, 0)
    }

    fn shift_with_cutoff(&self, delta: Delta, cutoff: Ind) -> Term {
        match self {
            Var {ind} => {
                if *ind < cutoff {
                    self.clone()
                } else {
                    eprintln!("IND: {}, CUTOFF: {}, DELTA: {}", &ind, &cutoff, &delta);
                    var(ind.checked_add(delta).expect("Should not run out of indexes"))
                }
            }
            App {inner, arg} => app(
                inner.shift_with_cutoff(delta, cutoff),
                arg.shift_with_cutoff(delta, cutoff)
            ),
            Abs {inner} => abs(inner.shift_with_cutoff(delta, cutoff+1))
        }
    }

    fn substitute(&self, x: Ind, with: &Term) -> Term {
        match self {
            Var {ind} => {
                if *ind == x {
                    with
                } else {
                    self
                }.clone()
            }
            App {inner, arg} => app(inner.substitute(x, with), arg.substitute(x, with)),
            Abs {inner} => abs(inner.substitute(x+1, &with.shift(1))),
        }
    }
}

mod test {
    use pretty_assertions::assert_eq;
    use super::*;

    #[test]
    fn basic_shift() {
        let before = abs(abs(app(var(1), app(var(0), var(2)))));
        let after = abs(abs(app(var(1), app(var(0), var(4)))));
        assert_eq!(before.shift(2), after);
    }

    #[test]
    fn simple_shift() {
        let before = abs(app(app(var(0), var(1)), abs(app(app(var(0), var(1)), var(2)))));
        let after = abs(app(app(var(0), var(3)), abs(app(app(var(0), var(1)), var(4)))));
        assert_eq!(before.shift(2), after);
    }

    #[test]
    fn substitute_noop() {
        assert_eq!(var(1).substitute(0, &var(2)), var(1));
    }

    #[test]
    fn substitute_x() {
        assert_eq!(var(0).substitute(0, &var(2)), var(2));
    }

    #[test]
    fn substitute_id() {
        assert_eq!(abs(var(1)).substitute(0, &var(2)), abs(var(3)));
    }

    #[test]
    fn substitute_id_of_id() {
        assert_eq!(abs(var(1)).substitute(0, &abs(var(1))), abs(abs(var(2))));
    }

    #[test]
    fn substitute_x_app_y() {
        assert_eq!(app(var(0), var(1)).substitute(0, &abs(var(1))), app(abs(var(1)), var(1)));
    }

    #[test]
    fn substitute_x_app_x() {
        assert_eq!(app(var(0), var(0)).substitute(0, &abs(var(1))), app(abs(var(1)), abs(var(1))));
    }
}
