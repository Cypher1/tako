use crate::experimental::lambda;
use crate::experimental::ski;

use ski::{Ski, p};

use lambda::{Ind, Delta, Term};

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Debug)]
enum Lambski {
    Var {ind: Ind},
    App {inner: Box<Lambski>, arg: Box<Lambski>},
    Abs {inner: Box<Lambski>},
    S,
    K,
    I,
}

fn var(ind: Ind) -> Lambski {
    use Lambski::*;
    Var { ind }
}

fn app(inner: Lambski, arg: Lambski) -> Lambski {
    use Lambski::*;
    App {
        inner: Box::new(inner),
        arg: Box::new(arg),
    }
}

fn abs(inner: Lambski) -> Lambski {
    use Lambski::*;
    Abs {
        inner: Box::new(inner),
    }
}

impl Lambski {
    pub fn force_lambda(&self) -> Term {
        use Lambski::*;
        match self {
            Var {ind} => lambda::var(*ind),
            App {inner, arg} => lambda::app(inner.force_lambda(), arg.force_lambda()),
            Abs {inner} => lambda::abs(inner.force_lambda()),
            ski => panic!("Contained ski: {:?}", ski),
        }
    }

    pub fn force_ski(&self) -> Ski {
        match self {
            Lambski::S => Ski::S,
            Lambski::K => Ski::K,
            Lambski::I => Ski::I,
            Lambski::App { inner, arg } => Ski::P(vec![inner.force_ski(), arg.force_ski()].into()),
            lambda => panic!("Contained lambda: {:?}", lambda),
        }
    }

    pub fn to_lambda(&self) -> Term {
        use lambda::{abs, app, var};
        match self {
            Lambski::S => abs(abs(abs(app(app(var(2), var(0)), app(var(1), var(0)))))),
            Lambski::K => abs(abs(var(1))),
            Lambski::I => abs(var(0)),
            Lambski::Var{ind} => lambda::var(*ind),
            Lambski::App{inner, arg} => app(inner.to_lambda(), arg.to_lambda()),
            Lambski::Abs{inner} => abs(inner.to_lambda()),
        }
    }

    pub fn to_ski(&self) -> Ski {
        self.skified().force_ski()
    }

    pub fn uses(&self, var: Ind) -> bool {
        use Lambski::*;
        match self {
            Var { ind } => *ind == var,
            App { inner, arg } => inner.uses(var) || arg.uses(var),
            Abs { inner } => inner.uses(var+1),
            _ => false,
        }
    }

    pub fn shift(&self, delta: Delta) -> Lambski {
        self.shift_with_cutoff(delta, 0)
    }

    pub fn shift_with_cutoff(&self, delta: Delta, cutoff: Ind) -> Lambski {
        use Lambski::*;
        match self {
            Var { ind } => {
                if *ind < cutoff {
                    self.clone()
                } else {
                    var(ind
                        .checked_add(delta)
                        .expect("Should not run out of indexes"))
                }
            }
            App { inner, arg } => app(
                inner.shift_with_cutoff(delta, cutoff),
                arg.shift_with_cutoff(delta, cutoff),
            ),
            Abs { inner } => abs(inner.shift_with_cutoff(delta, cutoff + 1)),
            S => S,
            K => K,
            I => I,
        }
    }

    pub fn skified(&self) -> Lambski {
        use Lambski::*;
        match self {
            // See: https://blog.ngzhian.com/ski2.html
            Var {ind} => var(*ind), // Clause 1.
            App {inner, arg} => app(inner.skified(), arg.skified()), // Clause 2.
            Abs {inner} => {
                match &**inner {
                    App {inner, arg} => {
                        let inner = Abs{inner: inner.clone()}.skified();
                        let arg = Abs{inner: arg.clone()}.skified();
                        // p(&[S, inner, arg]) // Clause 6.
                        app(S, app(inner.skified(), arg.skified())) // Clause 6.
                    },
                    Abs { inner } => {
                            // translate (Abs (x, translate (Abs (y, e))))
                            Abs { inner: Box::new(Abs {inner: inner.clone()}.skified())}.skified() // Clause 5.
                    }
                    inner => {
                        if !inner.uses(0) {
                            let inner = inner.shift(-1).skified();
                            app(K, inner) // Clause 3.
                        } else if let Var {ind: 0} = inner {
                            I // Clause 4.
                        } else {
                            panic!("TODO: ${:?}", self)
                        }
                    }
                }
            }
            S => S,
            K => K,
            I => I,
        }
    }
}

impl Ski {
    pub fn to_lambski(&self) -> Lambski {
        use Lambski::*;
        match self {
            Ski::S => S,
            Ski::K => K,
            Ski::I => I,
            Ski::V(name) => panic!("Lambski does not support abstract variable {}", name),
            Ski::P(stack) => {
                let mut curr = None;
                for next in stack.iter() {
                    let next = next.to_lambski();
                    if let Some(head) = curr {
                        curr = Some(App {
                            inner: Box::new(head),
                            arg: Box::new(next),
                        });
                    } else {
                        curr = Some(next);
                    }
                }
                curr.expect("Cannot convert empty stack")
            }
        }
    }

    pub fn to_lambda(&self) -> Term {
        self.to_lambski().to_lambda()
    }
}

impl lambda::Term {
    pub fn to_lambski(&self) -> Lambski {
        use Lambski::*;
        match self {
            Term::Var {ind} => var(*ind),
            Term::App {inner, arg} => app(inner.to_lambski(), arg.to_lambski()),
            Term::Abs {inner} => Abs {
                inner: Box::new(inner.to_lambski()),
            },
        }
    }

    pub fn to_ski(&self) -> Ski {
        self.to_lambski().to_ski()
    }
}

#[cfg(test)]
mod test {
    use std::collections::VecDeque;

    use pretty_assertions::assert_eq;
    use super::*;
    use lambda::util::*;
    use Ski::*;

    fn test(l: lambda::Term, s: Ski) {
        let skid = l.to_ski();
        let skid = ski::eval(vec![skid].into());
        let s = ski::eval(vec![s].into());
        assert_eq!(format!("{:?}", skid), format!("{:?}", s));
        assert_eq!(skid, s);
    }

    #[test]
    fn church_0_to_ski() {
        test(
            church_nat(0),
            p(&[K,I])
        );
    }

    #[test]
    fn church_1_to_ski() {
        // (\f. (\x. (f x)))
        test(
            church_nat(1),
            p(&[I])
        );
    }

    #[test]
    fn church_2_to_ski() {
        // S(S(K(S)K)I
        test(
            church_nat(2),
            p(&[S, p(&[S, p(&[K, S, K])]), I])
        );
    }

    #[test]
    fn church_succ_to_ski() {
        // S(S(KSK)
        test(
            church_succ(),
            p(&[S, p(&[S, p(&[K, S, K])])])
        );
    }

    #[test]
    fn church_plus_to_ski() {
        test(
            church_plus(),
            p(&[I]) // THIS IS WRONG
        );
    }

    #[test]
    fn church_3_plus_4_to_ski() {
        test(
            lambda::app(lambda::app(church_plus(), church_nat(3)), church_nat(4)),
            p(&[I]) // THIS IS WRONG
        );
    }
}
