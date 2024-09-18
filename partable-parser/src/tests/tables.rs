use strum::{EnumCount, IntoEnumIterator};
use strum_macros::{EnumCount, EnumIter};
fn padded<T: std::fmt::Debug>(d: T, s: usize) -> String {
    let content = format!("{:?}", d);
    format!(
        "{content}{p}",
        p = " ".repeat(s.saturating_sub(content.len()))
    )
}

/*

# Partable approach

- Use bottom up 'spans' to tokenize
- Parsing can proceed at locations chosen non-determinisically
- Results should be non-determinisic
- This requires that:
    - Every non-deterministic choice is either:
        - A Noop (waiting on further input)
        - Produces an update that can be performed out of order (i.e. other steps



    Parllelizable for systems (toposort) & only run later rules on new tokens
    Cost for each update, O(nm) (where n is the size of the update and m is the size of the pre-existing data).

# Performance
    - Can Lex but we shouldn't count the cost because we're not for Pratt & RD

    - Estimates:
        Pratt: 6+2*stack
        RD: 13+6*stack
        PT: 36


# Example
    (1+20)*3

    Classify tokens

    (1+20)*3
    ODADDCMD

    Entries are (range, type, parent)

    Digits = [1,2,0,3]
    Ops = [+,*]
    Opens = [(]
    Closes = [)]


    ..1..20...3 Paste(DD=>D)
    ( I+  I )* I

    ## Promote(D=>I)

    I = [1,20,3]
    IMH=[]
    IAH=[]

    Parsing

    ( I+  I )* I Merge(I,* => IMH)
    ...........*I

    ( I+  I )* I Merge(IMH,I => I)
    .............

    ( I+  I)*I Right(I+ => IAH)
    (IAH I)*I

    (IAH I)*I  Merge(IAH,I => I)
    ( I    )*I

    ( I ) *I Merge(O,!C => O)

    O ) *I Merge(O,C => I)
    I     *I

    }

    Lowest found token was I so we have to go back to I rules.


    I*I   I Right(I,* => IMH)
    IMH I

    IMH I Merge(IMH,I => I)
    I



*/

#[derive(Debug, Default, Copy, Clone, PartialEq, Hash, Eq)]
struct Entry {
    kind: Symbol,
    start: usize,
    end: usize,
    parent: Option<usize>,
}

// Example Language stuff
#[repr(u8)]
#[derive(Debug, Default, Copy, Clone, PartialEq, Hash, Eq, EnumIter)]
enum Op {
    #[default]
    Noop,
    Add,
    Mul,
}

#[repr(u8)]
#[derive(Default, Debug, EnumCount, Copy, Clone, PartialEq, Hash, Eq, EnumIter)]
enum Symbol {
    #[default]
    Digits,
    Integer,
    OpenParen,
    CloseParen,
    Mul,
    IntegerMulHole,
    Add,
    IntegerAddHole,
}

fn classify_char(ch: char) -> Option<Symbol> {
    let ty = match ch {
        '0'..'9' => Symbol::Digits,
        '+' => Symbol::Add,
        '*' => Symbol::Mul,
        '(' => Symbol::OpenParen,
        ')' => Symbol::CloseParen,
        _ => return None,
    };
    Some(ty)
}

#[derive(Debug, Copy, Clone, PartialEq, Hash, Eq)]
enum Parent {
    Both, // Merge/Paste, update symbol type
    Left, // Left is new parent, update symbol type
    Right, // Right is new parent, update symbol type
}

#[derive(Debug, Copy, Clone, PartialEq, Hash, Eq)]
enum Rule {
    Merge {
        left: Symbol,
        right: Symbol,
        out: Symbol,
        Parent: Parent,
    },
    Promote {
        from: Symbol,
        to: Symbol,
    },
}

use Symbol::*;

impl Rule {
    const fn paste(left: Symbol, right: Symbol, out: Symbol) -> Self {
        Self::Merge { left, right, out, Parent: Parent::Paste }
    }
    const fn left(left: Symbol, right: Symbol, out: Symbol) -> Self {
        Self::Merge { left, right, out, Parent: Parent::ParentUpdateLeft }
    }
    const fn right(left: Symbol, right: Symbol, out: Symbol) -> Self {
        Self::Merge { left, right, out, Parent: Parent::ParentUpdateRight }
    }
    const fn promote(from: Symbol, to: Symbol) -> Self {
        Self::Promote { from, to }
    }
}

const RULES: &[Rule] = &[
    Rule::paste(Symbol::Digits, Symbol::Digits, Symbol::Digits),
    Rule::promote(Symbol::Digits, Symbol::Integer),
    Rule::right(
        Symbol::Integer,
        Symbol::Mul,
        Symbol::IntegerMulHole,
    ),
    Rule::left(
        Symbol::IntegerMulHole,
        Symbol::Integer,
        Symbol::Integer,
    ),
    Rule::right(
        Symbol::Integer,
        Symbol::Add,
        Symbol::IntegerAddHole,
    ),
    Rule::left(
        Symbol::IntegerAddHole),
        Symbol::Integer,
        Symbol::Integer,
    ),
    //Rule::left(
        //Symbol::OpenParen,
        //<AnyNoneClose>,
        //Symbol::OpenParen,
    //),
    Rule::paste(
        Symbol::OpenParenn,
        Symbol::CloseParen,
        Symbol::Integer,
    ),
];

#[derive(Debug, Default, Clone, PartialEq, Hash, Eq)]
struct State {
    table: [Vec<Entry>; Symbol::COUNT],
}

fn run_rule(state: &State, rule: &'static Rule) {
    println!("{state:?}");
    println!("{rule:?}");
    
    match rule {
        Rule::Merge { Parent: Parent::Paste, left, right, out } => {
            eprintln!("{rule:?} paste");
            let ls = state.table[left.kind as usize];
            let rs = state.table[right.kind as usize];
            println!("{ls:?}");
            println!("{rs:?}");
            println!("{out:?}");
        }
        _ => todo!(),
    }
}

const EMPTY_ROW: Vec<Entry> = Vec::new();
const DEFAULT_TABLE: [Vec<Entry>; Symbol::COUNT] = [EMPTY_ROW; Symbol::COUNT];

fn run_test(input: &str) {
    let symbols: Vec<Symbol> = Symbol::iter().collect();
    for symbol in symbols {
        println!("SYMBOL: {:?}", symbol);
    }
    for rule in RULES {
        println!("RULE:   {rule:?}");
    }

    println!("START\n");
    println!("{input}");

    let mut entries: Vec<Entry> = vec![];
    let mut table = DEFAULT_TABLE.clone();

    for (start, ch) in input.chars().enumerate() {
        let Some(kind) = classify_char(ch) else {
            todo!("DID NOT HANDLE {ch:?}");
        };
        let entry = Entry {
            kind,
            start,
            end: start+1,
            parent: None,
        };
        entries.push(entry.clone());
        table[kind as usize].push(entry);
    }
    println!("{entries:#?}");

    let mut state = State {
        table,
    };
    for rule in RULES {
        run_rule(&mut state, &rule);
        println!();
    }
    todo!();
}

#[test]
fn table_test() {
    let initial = "(1+20)*3";

    run_test(initial);
}
