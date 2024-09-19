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
        PT: 90... :/


# Example
    (1+20)*3

    Classify tokens

    (1+20)*3
    ODADDCMD

    Entries are (type, at, ?node)

    Entries = [(1,+,2,0),*,3]

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

#[derive(Default, Copy, Clone, PartialEq, Hash, Eq)]
struct Entry {
    kind: Kind,
    at: usize,
    node: Option<(SubExprKind, usize)>,
}

impl std::fmt::Debug for Entry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}@{:?}", self.kind, self.at)?;
        if let Some((exprkind, id)) = self.node {
            write!(f, "({exprkind:?} {id:?})")
        } else {
            Ok(())
        }
    }
}

// Example Language stuff
#[repr(u8)]
#[derive(Debug, Default, Copy, Clone, PartialEq, Hash, Eq, EnumIter, EnumCount)]
enum SubExprKind {
    #[default]
    None, // TODO: Omit from table in future?
    NumLit,
    Add,
    Mul,
    SubExpr, // TODO: Omit from table and promote children?
}

#[repr(u8)]
#[derive(Default, Debug, Copy, Clone, PartialEq, Hash, Eq, EnumIter, EnumCount)]
enum Kind {
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

fn classify_char(ch: char) -> Option<Kind> {
    let ty = match ch {
        '0'..'9' => Kind::Digits,
        '+' => Kind::Add,
        '*' => Kind::Mul,
        '(' => Kind::OpenParen,
        ')' => Kind::CloseParen,
        _ => return None,
    };
    Some(ty)
}

#[derive(Debug, Copy, Clone, PartialEq, Hash, Eq)]
enum ParentChoice {
    Both,
    Left,
    Right,
}

type Pattern = Kind;

#[derive(Debug, Copy, Clone, PartialEq, Hash, Eq)]
enum Rule {
    Merge {
        left: Pattern,
        right: Pattern,
        out: Kind,
        mode: ParentChoice,
        expr: SubExprKind,
    },
    Promote {
        from: Pattern,
        to: Kind,
        expr: SubExprKind,
    },
    // TODO: Consider rules to inject missing stuff and produce a warning
}

impl Rule {
    // TODO: These could be const functions if `into` was const.
    // This is waiting on `~const Into<_>` stabilization.

    fn paste<L: Into<Pattern>, R: Into<Pattern>>(left: L, right: R, out: Kind, expr: SubExprKind) -> Self {
        Self::Merge { left: left.into(), right: right.into(), out, mode: ParentChoice::Both, expr }
    }
    fn left<L: Into<Pattern>, R: Into<Pattern>>(left: L, right: R, out: Kind, expr: SubExprKind) -> Self {
        Self::Merge { left: left.into(), right: right.into(), out, mode: ParentChoice::Left, expr }
    }
    fn right<L: Into<Pattern>, R: Into<Pattern>>(left: L, right: R, out: Kind, expr: SubExprKind) -> Self {
        Self::Merge { left: left.into(), right: right.into(), out, mode: ParentChoice::Right, expr }
    }
    fn promote<F: Into<Pattern>>(from: F, to: Kind, expr: SubExprKind) -> Self {
        Self::Promote { from: from.into(), to, expr }
    }
}

// TODO: This could be a const.
fn get_rules() -> Vec<Rule> {
    vec![
        Rule::paste(Kind::Digits, Kind::Digits, Kind::Digits, SubExprKind::None),
        Rule::promote(Kind::Digits, Kind::Integer, SubExprKind::NumLit),
        Rule::right(
            Kind::Integer,
            Kind::Mul,
            Kind::IntegerMulHole,
            SubExprKind::None, // Already captured
        ),
        Rule::left(
            Kind::IntegerMulHole,
            Kind::Integer,
            Kind::Integer,
            SubExprKind::Mul,
        ),
        Rule::right(
            Kind::Integer,
            Kind::Add,
            Kind::IntegerAddHole,
            SubExprKind::None, // Already captured
        ),
        Rule::left(
            Kind::IntegerAddHole,
            Kind::Integer,
            Kind::Integer,
            SubExprKind::Add,
        ),
        Rule::left(
            Kind::OpenParen,
            Kind::Integer,
            Kind::OpenParen,
            SubExprKind::None, // Already captured
        ),
        Rule::paste(
            Kind::OpenParen,
            Kind::CloseParen,
            Kind::Integer,
            SubExprKind::SubExpr,
        ),
    ]
}

#[derive(Debug, Default, Clone, PartialEq, Hash, Eq)]
struct State {
    // Finish subexpressions
    expression_table: [Vec<Entry>; SubExprKind::COUNT],

    // Tokens / expressions being parsed.
    entries: Vec<Entry>,
}

impl std::ops::Index<&SubExprKind> for State {
    type Output = Vec<Entry>;

    fn index<'a>(&'a self, row: &SubExprKind) -> &'a Vec<Entry> {
        &self.expression_table[*row as usize]
    }
}

impl std::ops::IndexMut<&SubExprKind> for State {
    fn index_mut<'a>(&'a mut self, row: &SubExprKind) -> &'a mut Vec<Entry> {
        &mut self.expression_table[*row as usize]
    }
}

fn run_rule(state: &mut State, rule: &Rule) -> bool {
    let mut progress = false;
    // println!("{state:?}");
    match rule {
        Rule::Merge { mode, left, right, out, expr } => {
            println!("  {left:?} {right:?} => {out:?} {mode:?}");
            println!();
            // FIXME: Scan backwards for ParentChoice::Right.
            let mut delete = 0;
            let mut output_index = 0;
            let mut li = 0; 
            let mut ri = 1; 
            while ri < state.entries.len() {
                let l = state.entries[li];
                let r = state.entries[ri];
                if l.kind != *left || r.kind != *right {
                    // Move up!
                    state.entries[output_index] = l;
                    output_index += 1;
                    state.entries[output_index] = r;
                    li = ri;
                    ri += 1;
                    continue;
                }
                // println!(
                    // "    [{li:?}..{ri:?}] = {l:?} & {r:?}",
                // );

                progress = true;
                ri += 1; // Right moves up

                let mut new = Entry {
                    kind: *out,
                    at: l.at,
                    node: None, // todo update.
                };
                // Replace the old nodes, swapping the second out if necessary.

                // println!(
                    // "      [{output_index:?}] = {new:?}",
                // );
                
                delete += 1; // Merging at least one!
                match mode {
                    ParentChoice::Both => {}
                    ParentChoice::Left => {
                        // Left is parent, preserve the right as a child.
                        new.node = Some((*expr, state[expr].len()));
                        // FIXME: Capture multiple children
                        state[expr].push(r);
                    }
                    ParentChoice::Right => {
                        // Right is parent, preserve the left as a child.
                        new.node = Some((*expr, state[expr].len()));
                        // FIXME: Capture multiple children
                        state[expr].push(l);
                    }
                }
                state.entries[output_index] = new;
            }
            for _ in 0..delete {
                state.entries.pop();
            }
        }
        Rule::Promote { from, to, expr } => {
            // println!("  {from:?} => {to:?}");
            // println!();
            for i in 0..state.entries.len() {
                // TODO: Avoid reserving id here to avoid locking issues.
                let id = state[expr].len();
                let entry = &mut state.entries[i];

                if entry.kind != *from {
                    continue;
                }
                // println!("    {entry:?}");
                progress = true;
                entry.kind = *to;
                entry.node = Some((*expr, id));
                let entry = state.entries[i];
                state[expr].push(entry);
            }
        }
    }
    progress
}

const EMPTY_ROW: Vec<Entry> = Vec::new();
const EMPTY_TABLE: [Vec<Entry>; SubExprKind::COUNT] = [EMPTY_ROW; SubExprKind::COUNT];

fn setup(input: &str, rules: &[Rule]) -> State {
    let kinds: Vec<Kind> = Kind::iter().collect();
    for kinds in kinds {
        println!("KINDS:  {:?}", kinds);
    }
    println!();
    for rule in rules {
        println!("RULE:   {rule:?}");
    }

    println!();
    println!("START");
    println!();
    println!("{input}");

    let mut entries: Vec<Entry> = vec![];
    for (at, ch) in input.chars().enumerate() {
        let Some(kind) = classify_char(ch) else {
            todo!("DID NOT HANDLE {ch:?}");
        };
        let entry = Entry {
            kind,
            at,
            node: None,
        };
        entries.push(entry.clone());
    }
    // println!("{entries:#?}");

    let mut state = State {
        expression_table: EMPTY_TABLE.clone(),
        entries,
    };
    let mut earliest_progressing_rule = 0;
    while earliest_progressing_rule < rules.len() {
        let start = earliest_progressing_rule;
        earliest_progressing_rule = rules.len();
        for (rule_index, rule) in rules[start..].iter().enumerate() {
            // println!("{:?}", state.entries);
            if run_rule(&mut state, &rule) {
                earliest_progressing_rule = std::cmp::min(earliest_progressing_rule, rule_index);
            }
            // println!();
        }
    }
    state
}

#[test]
fn table_test_1() {
    let initial = "(1+201)*3";
    let rules = &get_rules()[..1]; // Digits* -> Digits
    let state = setup(initial, rules);
    assert_eq!(format!("{:?}", state.entries), "[OpenParen@0, Digits@1, Add@2, Digits@3, CloseParen@6, Mul@7, Digits@8]");
}

#[test]
fn table_test_2() {
    let initial = "(1+201)*3";
    let rules = &get_rules()[..2]; // Digits -> Integer
    let state = setup(initial, rules);
    assert_eq!(format!("{:?}", state.entries), "[OpenParen@0, Integer@1(NumLit 0), Add@2, Integer@3(NumLit 1), CloseParen@6, Mul@7, Integer@8(NumLit 2)]");
}

#[test]
fn table_test_3() {
    let initial = "(1+201)*3";
    let rules = &get_rules()[..3]; // Integer * -> IntegerMulHole
    let state = setup(initial, rules);
    assert_eq!(format!("{:?}", state.entries), "[OpenParen@0, Integer@1(NumLit 0), Add@2, Integer@3(NumLit 1), CloseParen@6, Mul@7, Integer@8(NumLit 2)]");
    // TODO: Add test case for this that is not a noop.
}

#[test]
fn table_test_4() {
    let initial = "(1+201)*3";
    let rules = &get_rules()[..4]; // IntegerMulHole Integer -> Integer
    let state = setup(initial, rules);
    assert_eq!(format!("{:?}", state.entries), "[OpenParen@0, Integer@1(NumLit 0), Add@2, Integer@3(NumLit 1), CloseParen@6, Mul@7, Integer@8(NumLit 2)]");
    // TODO: Add test case for this that is not a noop.
}

#[test]
fn table_test_5() {
    let initial = "(1+201)*3";
    let rules = &get_rules()[..5]; // Integer + -> IntegerAddHole
    let state = setup(initial, rules);
    assert_eq!(format!("{:?}", state.entries), "[OpenParen@0, IntegerAddHole@1(None 0), Integer@3(NumLit 1), CloseParen@6, Mul@7, Integer@8(NumLit 2)]");
    // TODO: Add test case for this that is not a noop.
}

#[test]
fn table_test_6() {
    let initial = "(1+201)*3";
    let rules = &get_rules()[..6]; // IntegerAddHole Integer -> Integer
    let state = setup(initial, rules);
    assert_eq!(format!("{:?}", state.entries), "[OpenParen@0, Integer@1(Add 0), CloseParen@6, Mul@7, Integer@8(NumLit 2)]");
    // TODO: Add test case for this that is not a noop.
}

#[test]
fn table_test_7() {
    let initial = "(1+201)*3";
    let rules = &get_rules()[..7]; // OpenParen Integer -> OpenParen
    let state = setup(initial, rules);
    assert_eq!(format!("{:?}", state.entries), "[OpenParen@0(None 1), CloseParen@6, Mul@7, Integer@8(NumLit 2)]");
    // TODO: Add test case for this that is not a noop.
}

#[test]
fn table_test_all() {
    let initial = "(1+201)*3";
    let rules = get_rules();
    let state = setup(initial, &rules);
    assert_eq!(format!("{:?}", state.entries), "[Integer@0(Mul 0)]");
    assert_eq!(format!("{:?}", state.expression_table), "");
}
