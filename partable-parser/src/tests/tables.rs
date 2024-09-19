use strum::{EnumCount, IntoEnumIterator};
use strum_macros::{EnumCount, EnumIter};
/*
 *
# Par-table

The result of a parser to parse parts in parallel tables:
    a part-able par-table parse-table!

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

    Entries are (range, type, ?parent)

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
enum ReplaceMode {
    Both,
    Left,
    Right,
}

type Pattern = Symbol;

#[derive(Debug, Copy, Clone, PartialEq, Hash, Eq)]
enum Rule {
    Merge {
        left: Pattern,
        right: Pattern,
        out: Symbol,
        mode: ReplaceMode,
    },
    Promote {
        from: Pattern,
        to: Symbol,
    },
    // TODO: Consider rules to inject missing stuff and produce a warning
}

impl Rule {
    // TODO: These could be const functions if `into` was const.
    // This is waiting on `~const Into<_>` stabilization.

    fn paste<L: Into<Pattern>, R: Into<Pattern>>(left: L, right: R, out: Symbol) -> Self {
        Self::Merge { left: left.into(), right: right.into(), out, mode: ReplaceMode::Both }
    }
    fn left<L: Into<Pattern>, R: Into<Pattern>>(left: L, right: R, out: Symbol) -> Self {
        Self::Merge { left: left.into(), right: right.into(), out, mode: ReplaceMode::Left }
    }
    fn right<L: Into<Pattern>, R: Into<Pattern>>(left: L, right: R, out: Symbol) -> Self {
        Self::Merge { left: left.into(), right: right.into(), out, mode: ReplaceMode::Right }
    }
    fn promote<F: Into<Pattern>>(from: F, to: Symbol) -> Self {
        Self::Promote { from: from.into(), to }
    }
}

// TODO: This could be a const.
fn get_rules() -> Vec<Rule> {
    vec![
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
            Symbol::IntegerAddHole,
            Symbol::Integer,
            Symbol::Integer,
        ),
        /*
        Rule::left(
            Symbol::OpenParen,
            Pattern::Not(Symbol::CloseParen),
            Symbol::OpenParen,
        ),*/
        Rule::paste(
            Symbol::OpenParen,
            Symbol::CloseParen,
            Symbol::Integer,
        ),
    ]
}

#[derive(Debug, Default, Clone, PartialEq, Hash, Eq)]
struct State {
    table: [Vec<Entry>; Symbol::COUNT],
}

impl std::ops::Index<&Symbol> for State {
    type Output = Vec<Entry>;

    fn index<'a>(&'a self, row: &Symbol) -> &'a Vec<Entry> {
        &self.table[*row as usize]
    }
}

impl std::ops::IndexMut<&Symbol> for State {
    fn index_mut<'a>(&'a mut self, row: &Symbol) -> &'a mut Vec<Entry> {
        &mut self.table[*row as usize]
    }
}

fn run_rule(state: &mut State, rule: &Rule) {
    // println!("{state:?}");
    match rule {
        Rule::Merge { mode, left, right, out } => {
            eprintln!("{left:?} {right:?} => {mode:?} => {out:?}");
            for li in 0..state[left].len() {
                let l = state[left][li];
                for ri in 0..state[right].len() {
                    let r = state[right][ri];
                    if l.end != r.start {
                        continue
                    }
                    println!(
                        "{l_start:?}..{l_end:?}@{li:?} {r_start:?}..{r_end:?}@{ri:?}",
                        l_start =  l.start,
                        l_end =  l.end,
                        r_start =  r.start,
                        r_end =  r.end,
                    );
                    
                    let new = Entry {
                        kind: *out,
                        start: l.start,
                        end: r.end,
                    };

                    // Replace the old nodes, swapping the second out if necessary.
                }
            }
        }
        rule => todo!("Unhandled rule {rule:?}"),
    }
}

const EMPTY_ROW: Vec<Entry> = Vec::new();
const DEFAULT_TABLE: [Vec<Entry>; Symbol::COUNT] = [EMPTY_ROW; Symbol::COUNT];

fn run_test(input: &str) {
    let symbols: Vec<Symbol> = Symbol::iter().collect();
    let rules = get_rules();
    for symbol in symbols {
        println!("SYMBOL: {:?}", symbol);
    }
    for rule in &rules {
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
        };
        entries.push(entry.clone());
        table[kind as usize].push(entry);
    }
    // println!("{entries:#?}");

    let mut state = State {
        table,
    };
    for rule in &rules {
        run_rule(&mut state, &rule);
        println!();
    }
    todo!("TESTING");
}

#[test]
fn table_test() {
    let initial = "(1+20)*3";

    run_test(initial);
}
