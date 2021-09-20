#![allow(dead_code)]
use std::collections::VecDeque;

// Thanks to the following sources for info on these topics:
// Donnacha Oisín Kidney for https://doisinkidney.com/posts/2020-10-17-ski.html
// Ben Lynn for https://crypto.stanford.edu/~blynn/lambda/sk.html
// and of course Pearson for 'Types and Programming languages'

#[derive(Debug, PartialEq, Eq, Clone, Hash, PartialOrd, Ord)]
pub enum Ski {
    S,
    K,
    I,
    V(String),
    P(Stack),
}

use std::fmt;
impl fmt::Display for Ski {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", show(self))
    }
}

pub fn p(stack: &[Ski]) -> Ski {
    P(toVecDeque(stack))
}

type Stack = VecDeque<Ski>;

use Ski::{I, K, S, P, V};

pub fn eval(mut stack: Stack) -> Stack {
    // debug!("{:?}", shows(&stack));
    while let Some(curr) = stack.pop_front() {
        match curr {
            S => {
                if stack.len() >= 3 {
                    let x = stack
                        .pop_front()
                        .expect("Empty pop_front from non-empty stack (S.x).");
                    let y = stack
                        .pop_front()
                        .expect("Empty pop_front from non-empty stack (S.y).");
                    let z = stack
                        .pop_front()
                        .expect("Empty pop_front from non-empty stack (S.z).");
                    // xz(yz)
                    let mut parend = VecDeque::new();
                    if let P(y) = y {
                        parend.extend(y);
                    } else {
                        parend.push_back(y);
                    }
                    parend.push_back(z.clone());
                    stack.push_front(P(parend));
                    stack.push_front(z);
                    stack.push_front(x);
                } else {
                    stack.push_front(S);
                    return stack;
                }
            }
            K => {
                if stack.len() >= 2 {
                    let val = stack
                        .pop_front()
                        .expect("Empty pop_front from non-empty stack (K).");
                    stack.pop_front(); // drop
                    stack.push_front(val); // TODO: Replace rather than pop_front+push_front
                } else {
                    stack.push_front(K);
                    return stack;
                }
            }
            I => {} // Identity
            V(name) => {
                let mut simplified = VecDeque::new();
                for val in stack.iter().cloned() {
                    let out = if let P(val) = val {
                        let mut vals = eval(val);
                        if vals.len() == 1 {
                            vals.pop_front().expect("Length 1 vec should have a value")
                        } else {
                            P(vals)
                        }
                    } else {
                        val
                    };
                    simplified.push_back(out);
                }
                simplified.push_front(V(name));
                return simplified;
            }
            P(vs) => {
                for v in vs.iter().rev() {
                    stack.push_front(v.clone());
                }
            }
        }
        // debug!("{:?}", shows(&stack));
    }
    // Error: no instructions
    panic!("no instructions")
}

pub fn show(s: &Ski) -> String {
    match s {
        S => "S".to_string(),
        K => "K".to_string(),
        I => "I".to_string(),
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

fn toVecDeque(items: &[Ski]) -> VecDeque<Ski> {
    items.iter().cloned().collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use log::info;

    fn v(name: &str) -> Ski {
        Ski::V(name.to_string())
    }

    fn test(stack: Stack, expected: Stack) {
        info!("Running: {:?}", &stack);
        let out = eval(stack);
        info!("Got: {:?}", &out);

        assert_eq!(out, expected);
    }

    #[test]
    fn term_i() {
        test(
            toVecDeque(&[I, v("x"), v("y"), v("z")]),
            toVecDeque(&[v("x"), v("y"), v("z")]),
        );
    }

    #[test]
    fn term_k() {
        test(
            toVecDeque(&[K, v("x"), v("y"), v("z")]),
            toVecDeque(&[v("x"), v("z")]),
        );
    }

    #[test]
    fn term_s() {
        test(
            toVecDeque(&[S, v("x"), v("y"), v("z")]),
            toVecDeque(&[v("x"), v("z"), P(toVecDeque(&[v("y"), v("z")]))]),
        );
    }

    #[test]
    fn abc_to_a() {
        // S(KK)K
        test(
            toVecDeque(&[S, p(&[K, K]), K, v("a"), v("b"), v("c")]),
            toVecDeque(&[v("a")]),
        );
    }

    #[test]
    fn abc_to_b() {
        // KK
        test(
            toVecDeque(&[K, K, v("a"), v("b"), v("c")]),
            toVecDeque(&[v("b")]),
        );
    }

    #[test]
    fn abc_to_c() {
        // SSK(SK)
        test(
            toVecDeque(&[S, S, K, p(&[S, K]), v("a"), v("b"), v("c")]),
            toVecDeque(&[v("c")]),
        );
    }

    #[test]
    fn complex_expression() {
        /*
        S(K(SI))Kαβ →
        K(SI)α(Kα)β →
        SI(Kα)β →
        Iβ(Kαβ) →
        Iβα →
        βα
         */
        test(
            toVecDeque(&[
                S,
                p(&[K, p(&[S, I])]),
                K,
                v("a"),
                v("b"),
            ]
            ),
            toVecDeque(&[v("b"), v("a")]),
        );
    }
}
