#![allow(unused)]

use super::super::*;

#[derive(Clone, Copy, Debug, Hash, PartialEq, PartialOrd, Eq, Ord)]
enum SimpleSymbol {
    Add,
    Mul,
    Int,
    ParenOpen,
    ParenClose,
}

impl SimpleSymbol {
    fn to_str(s: Self) -> &'static str {
        match s {
            Self::Add => "+",
            Self::Mul => "*",
            Self::Int => "i",
            Self::ParenOpen => "(",
            Self::ParenClose => ")",
        }
    }
}

const N: usize = 5; // std::mem::variant_count::<SimpleSymbol>()

use Direction::*;
impl Symbol<{N}> for SimpleSymbol {

    const TABLE: DirTable< {N} > = DirTable {
        table: [
           //         +  *  i  (  )
            /* + */ [ R, I, B, B, I ], // Add contains Add (right) and subexprs
            /* * */ [ I, R, B, B, I ], // Mul contains Add | Mul (right) and subexprs
            /* i */ [ I, I, M, I, I ], // Ints are made up of ints
            /* ( */ [ R, R, R, R, C ], // ( contains anythng to the right except )
            /* ) */ [ I, I, I, I, I ], // ) never contains anything
        ]
    };
}

#[test]
fn it_works() {
    let result = add(2, 2);
    assert_eq!(result, 4);
}
