#![allow(unused)]

/// Library stuff
#[repr(u8)]
#[derive(Debug, Copy, Clone)]
enum Merge {
    Right = 0b01,
    Left = 0b10,
    Paste = 0b11,
}


/// Example Language stuff
#[repr(u8)]
#[derive(Debug, Default, Copy, Clone, PartialEq, Hash, Eq)]
enum Op {
    #[default]
    Noop,
    Add,
    Mul,
}

#[repr(u8)]
#[derive(Default, Debug, Copy, Clone, PartialEq, Hash, Eq)]
enum Symbol {
    #[default]
    Digits,
    //Open, Close,
    Raw(Op),
    Prefix(Op),
    // Suffix(Op),
    SubExpr(Op),
}

fn promote(e: Symbol) -> Option<Symbol> {
    let promotion = match e {
        Symbol::Digits => Symbol::SubExpr(Op::Mul),
        //Symbol::Prefix(Op::Add) => Symbol::SubExpr(Op::Add),
        Symbol::SubExpr(Op::Mul) => Symbol::SubExpr(Op::Add),
        _ => return None,
    };
    return Some(promotion);
}

fn merge(l: Symbol, r: Symbol) -> Option<(Merge, Symbol)> {
    let res = match l {
        Symbol::Prefix(_) => return None,
        Symbol::Digits => match r {
            Symbol::Digits => (Merge::Paste, Symbol::Digits),
            _ => return None,
        }
        Symbol::Raw(op) => match r {
            Symbol::SubExpr(sub_op) if op == sub_op => (Merge::Left, Symbol::Prefix(op)),
            _ => return None,
        },
        Symbol::SubExpr(op) => match r {
            Symbol::Prefix(sub_op) if op == sub_op => (Merge::Right, Symbol::SubExpr(op)),
            _ => return None,
        }
    };
    Some(res)
}

fn padded<T: std::fmt::Debug>(d: T, s: usize) -> String {
    let content = format!("{:?}", d);
    format!("{content}{p}", p=" ".repeat(s.saturating_sub(content.len())))
}

#[test]
fn main() {
    let initial = [
      //  Symbol::Raw(Op::Add),
      //  Symbol::Digits,
      //  Symbol::Digits,
      //  Symbol::Raw(Op::Add),
      //  Symbol::Digits,
        Symbol::Digits,
        Symbol::Raw(Op::Mul),
      //  Symbol::Digits,
      //  Symbol::Digits,
      //  Symbol::Digits,
        Symbol::Digits,
    ].to_vec();

    println!("\n{initial:?} START");

    let mut seen = vec![];
    let mut pool = vec![initial];
    let mut solutions = vec![];

    while let Some(toks) = pool.pop() {
        let mut terminal = true;
        let mut merged = false;
        let mut new_candidate = |ty: &str, toks: &Vec<Symbol>, candidate: Vec<Symbol>| {
            let has_seen = seen.contains(&candidate);
            println!("{toks:?} => {ty} {candidate:?}{s}", s=if has_seen {" DONE"} else {""});
            if has_seen {
                return;
            }
            seen.push(candidate.clone());
            pool.push(candidate);
            terminal = false;

        };
        for (pos, win) in toks.windows(2).enumerate() {
            let [l, r] = win else { unreachable!() };
            let m = merge(*l, *r);
            /*println!(
                "{pos:3?}: {l} vs {r} => {m}",
                l=padded(l, 10),
                r=padded(r, 10),
                m=padded(m, 10),
            );*/

            let Some((m, new)) = m else { continue };

            // New candidate
            let candidate = [&toks[0..pos], &[new][..], &toks[(pos+2)..]].concat();
            new_candidate(&format!("{m:?}"), &toks, candidate);
            merged = true;
        }
        
        // Promotions!
        if merged {
            continue;
        }

        match toks[..] {
            [] => {},
            [a] => {
                if let Some(new) = promote(a) {
                    new_candidate("pro1", &toks, vec![new]);
                }
            }
            [a, b] => {
                if let Some(new) = promote(a) {
                    new_candidate("pro2.0", &toks, vec![new, b]);
                }
                if let Some(new) = promote(b) {
                    new_candidate("pro2.1", &toks, vec![a, new]);
                }
            }
            [a, b, c] => {
                if let Some(new) = promote(a) {
                    new_candidate("pro3.0", &toks, vec![new, b, c]);
                }
                if let Some(new) = promote(c) {
                    new_candidate("pro3.2", &toks, vec![a, b, new]);
                }
            }
            _ => {}
        };

        for (pos, win) in toks.windows(3).enumerate() {
            let [a, b, c] = win else { unreachable!() };
            let Some(new) = promote(*b) else { continue };

            let candidate = [&toks[0..pos], &[*a, new, *c][..], &toks[(pos+3)..]].concat();
            new_candidate("pro", &toks, candidate);
        }

        if terminal {
            solutions.push(toks);
        }
    }
    println!("DONE");

    for sol in &solutions {
        println!("{sol:?}");
    }
    assert_eq!(solutions.len(), 1);
}
