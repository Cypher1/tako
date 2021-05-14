use crate::experimental::lambda;
use crate::experimental::ski;

use ski::{SKI, p};

use lambda::{Ind, Term};

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Debug)]
enum Lambski {
    Var {ind: Ind},
    App {inner: Box<Lambski>, arg: Box<Lambski>},
    Abs {inner: Box<Lambski>},
    S,
    K,
    I,
}

impl Lambski {
    pub fn force_lambda(&self) -> Term {
        use Lambski::*;
        match self {
            Var {ind} => Term::Var {ind: *ind},
            App {inner, arg} => Term::App {
                inner: Box::new(inner.force_lambda()),
                arg: Box::new(arg.force_lambda()),
            },
            Abs {inner} => Term::Abs {
                inner: Box::new(inner.force_lambda()),
            },
            ski => panic!("Contained ski: {:?}", ski),
        }
    }

    pub fn force_ski(&self) -> SKI {
        match self {
            Lambski::S => SKI::S,
            Lambski::K => SKI::K,
            Lambski::I => SKI::I,
            lambda => panic!("Contained lambda: {:?}", lambda),
        }
    }

    pub fn to_lambda(&self) -> Term {
        use lambda::{abs, app, var};
        match self {
            Lambski::S => abs(abs(abs(app(app(var(2), var(0)), app(var(1), var(0)))))),
            Lambski::K => abs(abs(var(1))),
            Lambski::I => abs(var(0)),
            Lambski::Var{ind} => Term::Var{ind: *ind},
            Lambski::App{inner, arg} => app(inner.to_lambda(), arg.to_lambda()),
            Lambski::Abs{inner} => abs(inner.to_lambda()),
        }
    }

    pub fn to_ski(&self) -> SKI {
        self.skified().force_ski()
    }

    pub fn skified(&self) -> Lambski {
        use Lambski::*;
        match self {
            // See: https://blog.ngzhian.com/ski2.html
            Var {ind} => Lambski::Var {ind: *ind}, // Clause 1.
            App {inner, arg} => P(vec![inner.skified(), arg.skified()].into()), // Clause 2.
            Abs {inner} => {
                match &**inner {
                    Var {ind} => {
                        if *ind == 0 {
                            I // Clause 4.
                        } else {
                            panic!("Wrong index {}", ind) // Hmmmm.
                        }
                    },
                    App {inner, arg} => {
                        let inner = Abs{inner: inner.clone()}.skified();
                        let arg = Abs{inner: arg.clone()}.skified();
                        p(&[S, inner, arg]) // Clause 6.
                    },
                    inner => {
                        if !inner.uses(0) {
                            let inner = inner.shift(-1).skified();
                            p(&[K, inner]) // Clause 3.
                        } else {
                            // translate (Abs (x, translate (Abs (y, e))))
                            // Clause 5.

                        }
                    }
                }
            }
        }
    }
}

impl SKI {
    pub fn to_lambski(&self) -> Lambski {
        use Lambski::*;
        match self {
            SKI::S => S,
            SKI::K => K,
            SKI::I => I,
            SKI::V(_name) => todo!(),
            SKI::P(_stack) => todo!(),
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
            Term::Var {ind} => Var {ind: *ind},
            Term::App {inner, arg} => App {
                inner: Box::new(inner.to_lambski()),
                arg: Box::new(arg.to_lambski()),
            },
            Term::Abs {inner} => Abs {
                inner: Box::new(inner.to_lambski()),
            },
        }
    }

    pub fn to_ski(&self) -> SKI {
        self.to_lambski().to_ski()
    }
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;
    use super::*;
    use lambda::util::*;
    use SKI::*;

    fn test(l: lambda::Term, s: SKI) {
        let skid = l.to_ski();
        assert_eq!(format!("{}", skid), format!("{}", s));
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
        // S(S(K(S)K)
        todo!();
    }

    #[test]
    fn church_3_plus_4_to_ski() {
        todo!();
    }
}
