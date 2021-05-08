use crate::experimental::lambda;
use crate::experimental::ski;

use ski::{SKI, SKI::*, p};

impl lambda::Term {
    pub fn to_ski(&self) -> SKI {
        use lambda::Term::*;
        match self {
            Var {ind} => V(format!("unknown_{}", *ind)),
            App {inner, arg} => P(vec![inner.to_ski(), arg.to_ski()].into()),
            Abs {inner} => {
                if **inner == (Var{ind: 0}) {
                    I
                } else if let App {inner, arg} = &**inner {
                    let inner = Abs{inner: inner.clone()}.to_ski();
                    let arg = Abs{inner: arg.clone()}.to_ski();
                    p(&[S, inner, arg])
                } else {
                    let inner = inner.shift(-1).to_ski();
                    p(&[K, inner])
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;
    use super::*;
    use lambda::util::*;

    #[test]
    fn church_0_to_ski() {
        assert_eq!(
            church_nat(0).to_ski(),
            p(&[K,I])
        );
    }

    #[test]
    fn church_1_to_ski() {
        // (\f. (\x. (f x)))
        eprintln!("{:?}", church_nat(1));
        assert_eq!(
            church_nat(1).to_ski(),
            p(&[I])
        );
    }

    #[test]
    fn church_2_to_ski() {
        todo!();
    }

    #[test]
    fn church_plus_to_ski() {
        todo!();
    }

    #[test]
    fn church_3_plus_4_to_ski() {
        todo!();
    }
}
