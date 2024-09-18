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
enum SymbolPattern {
    Eqs(Symbol),
    Not(Symbol),
}

#[derive(Debug, Copy, Clone, PartialEq, Hash, Eq)]
enum Rule {
    Paste {
        left: SymbolPattern,
        right: SymbolPattern,
        out: Symbol,
    },
    Merge {
        left: SymbolPattern,
        right: SymbolPattern,
        out: Symbol,
    },
    Promote {
        from: SymbolPattern,
        to: Symbol,
    },
}

use SymbolPattern::*;

impl Rule {
    const fn paste(left: SymbolPattern, right: SymbolPattern, out: Symbol) -> Self {
        Self::Paste { left, right, out }
    }
    const fn merge(left: SymbolPattern, right: SymbolPattern, out: Symbol) -> Self {
        Self::Merge { left, right, out }
    }
    const fn promote(from: SymbolPattern, to: Symbol) -> Self {
        Self::Promote { from, to }
    }
}

const RULES: &[Rule] = &[
    Rule::paste(Eqs(Symbol::Digits), Eqs(Symbol::Digits), Symbol::Digits),
    Rule::promote(Eqs(Symbol::Digits), Symbol::Integer),
    Rule::merge(
        Eqs(Symbol::Integer),
        Eqs(Symbol::Mul),
        Symbol::IntegerMulHole,
    ),
    Rule::merge(
        Eqs(Symbol::IntegerMulHole),
        Eqs(Symbol::Integer),
        Symbol::Integer,
    ),
    Rule::merge(
        Eqs(Symbol::Integer),
        Eqs(Symbol::Add),
        Symbol::IntegerAddHole,
    ),
    Rule::merge(
        Eqs(Symbol::IntegerAddHole),
        Eqs(Symbol::Integer),
        Symbol::Integer,
    ),
    Rule::merge(
        Eqs(Symbol::OpenParen),
        Not(Symbol::CloseParen),
        Symbol::Integer,
    ),
];

#[derive(Debug, Default, Clone, PartialEq, Hash, Eq)]
struct State {
    entries: Vec<Entry>,
    table: [Vec<Entry>; Symbol::COUNT],
}

fn run_rule(state: &State, rule: &'static Rule) {
    println!("{state:?}");
    println!("{rule:?}");
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
        entries.push(entry);
    }

    let mut state = State {
        entries,
        table: DEFAULT_TABLE.clone(),
    };
    for rule in RULES {
        run_rule(&mut state, &rule);
        println!();
    }
    // println!("{entries:#?}");
    todo!();
}

#[test]
fn table_test() {
    let initial = "(1+20)*3";

    run_test(initial);
}
