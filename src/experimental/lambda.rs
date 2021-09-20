pub type Delta = i32;
pub type Ind = Delta; // Should be unsigned

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Debug)]
pub enum Term {
    Var { ind: Ind },
    App { inner: Box<Term>, arg: Box<Term> },
    Abs { inner: Box<Term> },
}
use Term::{Abs, App, Var};

pub fn var(ind: Ind) -> Term {
    Var { ind }
}

pub fn app(inner: Term, arg: Term) -> Term {
    App {
        inner: Box::new(inner),
        arg: Box::new(arg),
    }
}

pub fn abs(inner: Term) -> Term {
    Abs {
        inner: Box::new(inner),
    }
}

impl Term {
    pub fn uses(&self, var: Ind) -> bool {
        match self {
            Var { ind } => *ind == var,
            App { inner, arg } => inner.uses(var) || arg.uses(var),
            Abs { inner } => inner.uses(var+1),
        }
    }

    pub fn shift(&self, delta: Delta) -> Term {
        self.shift_with_cutoff(delta, 0)
    }

    pub fn shift_with_cutoff(&self, delta: Delta, cutoff: Ind) -> Term {
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
        }
    }

    pub fn substitute(&self, x: Ind, with: &Term) -> Term {
        match self {
            Var { ind } => if *ind == x { with } else { self }.clone(),
            App { inner, arg } => app(inner.substitute(x, with), arg.substitute(x, with)),
            Abs { inner } => abs(inner.substitute(x + 1, &with.shift(1))),
        }
    }

    pub fn beta_reduce(&self) -> Term {
        if let App { inner, arg } = self {
            let inner: &Term = &*inner; // TODO: Work out why I can't write this in one line.
            if let Abs { inner } = inner {
                return inner.substitute(0, &arg.shift(1)).shift(-1);
            }
        }
        self.clone()
    }

    pub fn eval(&self) -> Term {
        match self {
            Var { .. } => self.clone(),
            App { inner, arg } => {
                let simplified = app(inner.eval(), arg.eval());
                let reduced = simplified.beta_reduce();
                if reduced != simplified {
                    return reduced.eval();
                }
                simplified
            }
            Abs { inner } => abs(inner.eval()),
        }
    }
}

pub mod util {
    use super::{abs, app, var, Abs, App, Term};

    pub fn church_bool(b: bool) -> Term {
        abs(abs(var(if b { 1 } else { 0 })))
    }

    pub fn church_not() -> Term {
        abs(app(app(var(0), church_bool(false)), church_bool(true)))
    }

    pub fn church_nat(n: i32) -> Term {
        let f = || var(1);
        let x = || var(0);

        let mut curr = x();
        for _ in 0..n {
            curr = app(f(), curr);
        }
        abs(abs(curr))
    }

    pub fn church_succ() -> Term {
        let n = || var(2);
        let f = || var(1);
        let x = || var(0);
        abs(abs(abs(app(app(n(), f()), app(f(), x())))))
    }

    #[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Debug)]
    pub enum DecodeChurchError {
        ExpectedAbstractionOverFunction(Term),
        ExpectedAbstractionOverBaseValue(Term),
        ExpectedFunctionApplication(Term),
        ExpectedFunctionApplicationOrBaseValue(Term),
    }

    pub fn church_to_i32(t: Term) -> Result<i32, DecodeChurchError> {
        use DecodeChurchError::*;
        let mut curr = if let Abs{inner} = t {
            *inner
        } else {
            return Err(ExpectedAbstractionOverFunction(t));
        };
        curr = if let Abs{inner} = curr {
            *inner
        } else {
            return Err(ExpectedAbstractionOverBaseValue(curr));
        };
        let mut n = 0;
        loop {
            if var(0) == curr {
                return Ok(n);
            } else if let App{inner, arg} = curr {
                if var(1) == *inner {
                    curr = *arg;
                    n += 1;
                } else {
                    return Err(ExpectedFunctionApplication(*inner));
                }
            } else {
                return Err(ExpectedFunctionApplicationOrBaseValue(curr));
            }
        }
    }

    pub fn church_plus() -> Term {
        //          m.  n.  f.  x.          ((m f)((n f) x))
        abs(abs(abs(abs(app(
            app(var(3), var(1)),
            app(app(var(2), var(1)), var(0)),
        )))))
    }
}

#[cfg(test)]
mod test {
    use super::util::{church_bool, church_nat, church_not, church_succ, church_plus, church_to_i32, DecodeChurchError};
    use super::*;

    #[test]
    fn basic_shift() {
        let before = abs(abs(app(var(1), app(var(0), var(2)))));
        let after = abs(abs(app(var(1), app(var(0), var(4)))));
        assert_eq!(before.shift(2), after);
    }

    #[test]
    fn simple_shift() {
        let before = abs(app(
            app(var(0), var(1)),
            abs(app(app(var(0), var(1)), var(2))),
        ));
        let after = abs(app(
            app(var(0), var(3)),
            abs(app(app(var(0), var(1)), var(4))),
        ));
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
        assert_eq!(
            app(var(0), var(1)).substitute(0, &abs(var(1))),
            app(abs(var(1)), var(1))
        );
    }

    #[test]
    fn substitute_x_app_x() {
        assert_eq!(
            app(var(0), var(0)).substitute(0, &abs(var(1))),
            app(abs(var(1)), abs(var(1)))
        );
    }

    #[test]
    fn basic_beta_reduction() {
        assert_eq!(
            app(abs(app(app(var(1), var(0)), var(2))), abs(var(0))).beta_reduce(),
            app(app(var(0), abs(var(0))), var(1))
        );
    }

    #[test]
    fn eval_one_step() {
        let id = abs(var(0));
        let before = app(
            app(
                app(abs(abs(abs(app(app(var(1), var(2)), var(0))))), id.clone()),
                var(5),
            ),
            var(6),
        );
        let after = app(app(var(5), id), var(6));
        assert_eq!(before.eval(), after);
    }

    #[test]
    fn eval_two_step() {
        let id = abs(var(0));
        let before = app(
            app(
                app(abs(abs(abs(app(app(var(1), var(2)), var(0))))), var(5)),
                id,
            ),
            var(6),
        );
        let after = app(var(5), var(6));
        assert_eq!(before.eval(), after);
    }

    #[test]
    fn eval_church_true() {
        assert_eq!(
            app(app(church_bool(true), church_nat(5)), church_nat(6)).eval(),
            church_nat(5)
        );
    }

    #[test]
    fn eval_church_false() {
        assert_eq!(
            app(app(church_bool(false), church_nat(5)), church_nat(6)).eval(),
            church_nat(6)
        );
    }

    #[test]
    fn eval_church_not_true() {
        assert_eq!(
            app(church_not(), church_bool(true)).eval(),
            church_bool(false)
        );
    }

    #[test]
    fn eval_church_not_false() {
        assert_eq!(
            app(church_not(), church_bool(false)).eval(),
            church_bool(true)
        );
    }

    #[test]
    fn eval_church_succ_3() {
        let three = church_nat(3);
        let four = church_nat(4);
        assert_eq!(app(church_succ(), three).eval(), four);
    }

    #[test]
    fn eval_church_succ_7() {
        let seven = church_nat(7);
        let eight = church_nat(8);
        assert_eq!(app(church_succ(), seven).eval(), eight);
    }

    #[test]
    fn eval_church_succ_70() {
        let seventy = church_nat(70);
        let seventy_one = church_nat(71);
        assert_eq!(app(church_succ(), seventy).eval(), seventy_one);
    }

    #[test]
    fn eval_church_succ_700() {
        let seven_hundred = church_nat(700);
        let seven_hundred_and_one = church_nat(701);
        assert_eq!(app(church_succ(), seven_hundred).eval(), seven_hundred_and_one);
    }

    #[test]
    #[ignore]
    fn eval_church_succ_7000() {
        let seven_thousand = church_nat(7000);
        let seven_thousand_and_one = church_nat(7001);
        assert_eq!(app(church_succ(), seven_thousand).eval(), seven_thousand_and_one);
    }

    #[test]
    fn eval_church_3_plus_4_eq_7() {
        let three = church_nat(3);
        let four = church_nat(4);
        let seven = church_nat(7);
        assert_eq!(app(app(church_plus(), three), four).eval(), seven);
    }

    #[test]
    fn eval_church_3_plus_4_to_i32_eq_7() {
        let three = church_nat(3);
        let four = church_nat(4);
        assert_eq!(church_to_i32(app(app(church_plus(), three), four).eval()), Ok(7));
    }

    #[test]
    fn fail_to_decode_true_as_nat() {
        assert_eq!(church_to_i32(church_bool(true)), Err(DecodeChurchError::ExpectedFunctionApplicationOrBaseValue(var(1))));
    }

    #[test]
    fn decode_false_as_nat() {
        // This is surprising, but it seems to be right.
        assert_eq!(church_to_i32(church_bool(false)), Ok(0));
    }

    #[test]
    fn fail_to_decode_x_as_nat() {
        assert_eq!(church_to_i32(var(0)), Err(DecodeChurchError::ExpectedAbstractionOverFunction(var(0))));
    }

    #[test]
    fn fail_to_decode_x_y_as_nat() {
        assert_eq!(church_to_i32(app(var(0), var(1))), Err(DecodeChurchError::ExpectedAbstractionOverFunction(app(var(0), var(1)))));
    }

    #[test]
    fn fail_to_decode_id_as_nat() {
        assert_eq!(church_to_i32(abs(var(0))), Err(DecodeChurchError::ExpectedAbstractionOverBaseValue(var(0))));
    }

}
