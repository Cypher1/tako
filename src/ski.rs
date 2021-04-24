use std::collections::VecDeque;

#[derive(Debug, PartialEq, Eq, Clone, Hash, PartialOrd, Ord)]
pub enum SKI {
    S,
    K,
    I,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, PartialOrd, Ord)]
pub enum SVal {
    T(SKI),
    V(String),
    P(Stack),
}

type Stack = VecDeque<SVal>;

use SVal::*;
use SKI::*;

pub fn show(s: &SVal) -> String {
    match s {
        T(S) => "S".to_string(),
        T(K) => "K".to_string(),
        T(I) => "I".to_string(),
        V(name) => name.to_string(),
        P(st) => {
            let mut s = "(".to_string();
            for t in st.iter() {
                s += &show(t);
            }
            s += ")";
            s
        }
    }
}

pub fn shows(s: &Stack) -> String {
    show(&P(s.clone()))
}

pub fn eval(mut stack: Stack) -> Stack {
    eprintln!("{:?}", shows(&stack));
    while let Some(curr) = stack.pop_front() {
        match curr {
            T(S) => {
                if stack.len() >= 3 {
                    let x = stack.pop_front().expect("Empty pop_front from non-empty stack (S.x).");
                    let y = stack.pop_front().expect("Empty pop_front from non-empty stack (S.y).");
                    let z = stack.pop_front().expect("Empty pop_front from non-empty stack (S.z).");
                    // xz(yz)
                    stack.push_front(P(vec![y, z.clone()].into()));
                    stack.push_front(z);
                    stack.push_front(x);
                } else {
                    stack.push_front(T(S));
                    return stack;
                }
            }
            T(K) => {
                if stack.len() >= 2 {
                    let val = stack.pop_front().expect("Empty pop_front from non-empty stack (K).");
                    stack.pop_front(); // drop
                    stack.push_front(val); // TODO: Replace rather than pop_front+push_front
                } else {
                    stack.push_front(T(K));
                    return stack;
                }
            }
            T(I) => {} // Identity
            V(name) => {
                stack.push_front(V(name));
                return stack;
            }
            P(vs) => {
                for v in vs.iter().rev() {
                    stack.push_front(v.clone());
                }
            }
        }
        eprintln!("{:?}", shows(&stack));
    }
    // Error: no instructions
    panic!("no instructions")
}

fn v(name: &str) -> SVal {
    SVal::V(name.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test(stack: Stack) {
        eprintln!("Running: {:?}", &stack);
        let out = eval(stack);
        eprintln!("Got: {:?}", out);
    }

    #[test]
    fn main() {
        test(vec![T(I), v("x"), v("y"), v("z")].into());
        test(vec![T(K), v("x"), v("y"), v("z")].into());
        test(vec![T(S), v("x"), v("y"), v("z")].into());

        /*
        S(K(SI))Kαβ →
        K(SI)α(Kα)β →
        SI(Kα)β →
        Iβ(Kαβ) →
        Iβα →
        βα
         */
        test(vec![T(S), P(vec![T(K), P(vec![T(S), T(I)].into())].into()), T(K), v("a"), v("b")].into());
    }
}
