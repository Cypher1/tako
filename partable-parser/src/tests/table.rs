use strum::{EnumCount, IntoEnumIterator};
use strum_macros::{EnumCount, EnumIter};

fn rpad(content: &str, s: usize, pad: &str) -> String {
    format!(
        "{content}{p}",
        p = pad.repeat(s.saturating_sub(content.len()))
    )
}

/*
 *
# Par-table

Concept TODOs:
- Is the algorithm what TreeSitter is doing?
- Can we generate partables from TreeSitter grammars?
- Can we generate partables from parsec style parser combinators?

Impl TODOs:
- Finish gathering children
- Run from 'cache' (or previous)
- With parallel workers
- Left vs right 'recursion'
- Rules as systems with topological sort and parallelism
- Semantic analysis / systems with topological sort
- Split out library code vs language definitions
- FUZZ tests

Benchmark TODOs:
- Write JSON parser
- Write Python parser
- Benchmark against off the shelf parsers
- Is the algorithm faster than Pratt / RD / TreeSitter / peg
- e.g. tree-sitter parser react.dev.js --time = ~54ms.


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

    Entries are (type, at, nodes)

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

#[derive(Default, Clone, PartialEq, Hash, Eq)]
struct Entry {
    kind: Kind,                // TODO: Drop thia field.
    id: usize,                 // Location in kind.
    at: usize,                 // TODO: Rename start
    end: usize,                // TODO: Use length
    nodes: Vec<(Kind, usize)>, // TODO: Avoid nested vec
}

impl std::fmt::Debug for Entry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // write!(f, "{:?}_{:?}@{:?}..{:?}", self.kind, self.id, self.at, self.end)?;
        write!(f, "{:?}@{:?}..{:?}", self.kind, self.at, self.end)?;
        if self.nodes.is_empty() {
            return Ok(());
        }
        let mut ch = '(';
        for (exprkind, id) in &self.nodes {
            write!(f, "{ch}{exprkind:?}_{id:?}")?;
            ch = ' ';
        }
        write!(f, ")")
    }
}

// Example Language stuff
#[repr(u8)]
#[derive(Debug, Default, Copy, Clone, PartialEq, Hash, Eq, EnumIter, EnumCount)]
enum Kind {
    // Ast nodes
    #[default]
    Digits,
    Add,
    Mul,

    // Partials
    Expr,
    OpenParen,
    CloseParen,
    MulOp,
    ExprMulHole,
    AddOp,
    ExprAddHole,
}

fn classify_char(ch: char) -> Option<Kind> {
    let ty = match ch {
        '+' => Kind::AddOp,
        '*' => Kind::MulOp,
        '(' => Kind::OpenParen,
        ')' => Kind::CloseParen,
        '0'..='9' => Kind::Digits,
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

type Archetype = Kind; // TODO: Implement negative patterns if necessary

#[derive(Copy, Clone, PartialEq, Hash, Eq)]
enum Rule {
    // TODO: Split out Kind::None into rule types.
    // TODO: Split out ParentChoice::* into rule types.
    Merge {
        left: Archetype,
        right: Archetype,
        out: Kind,
        mode: ParentChoice,
    },
    Promote {
        from: Archetype,
        to: Kind,
    },
    // TODO: Consider rules to inject missing stuff and produce a warning
}

impl std::fmt::Debug for Rule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Rule::Merge {
                mode,
                left,
                right,
                out,
            } => {
                write!(
                    f,
                    "{left} {right} => {out} {mode}",
                    left = rpad(&format!("{:?}", left), 11, " "),
                    right = rpad(&format!("{:?}", right), 11, " "),
                    out = rpad(&format!("{:?}", out), 11, " "),
                    mode = rpad(&format!("{:?}", mode), 11, " "),
                )
            }
            Rule::Promote { from, to } => {
                write!(
                    f,
                    "{from} => {to}",
                    from = rpad(&format!("{:?}", from), 23, " "),
                    to = rpad(&format!("{:?}", to), 11, " "),
                )
            }
        }
    }
}

impl Rule {
    // TODO: These could be const functions if `into` was const.
    // This is waiting on `~const Into<_>` stabilization.

    fn paste<L: Into<Archetype>, R: Into<Archetype>>(left: L, right: R, out: Kind) -> Self {
        Self::Merge {
            left: left.into(),
            right: right.into(),
            out,
            mode: ParentChoice::Both,
        }
    }
    fn left<L: Into<Archetype>, R: Into<Archetype>>(left: L, right: R, out: Kind) -> Self {
        Self::Merge {
            left: left.into(),
            right: right.into(),
            out,
            mode: ParentChoice::Left,
        }
    }
    fn right<L: Into<Archetype>, R: Into<Archetype>>(left: L, right: R, out: Kind) -> Self {
        Self::Merge {
            left: left.into(),
            right: right.into(),
            out,
            mode: ParentChoice::Right,
        }
    }
    fn promote<F: Into<Archetype>>(from: F, to: Kind) -> Self {
        Self::Promote {
            from: from.into(),
            to,
        }
    }
}

// TODO: This could be a const.
fn get_rules() -> Vec<Rule> {
    vec![
        Rule::paste(Kind::Digits, Kind::Digits, Kind::Digits),
        Rule::promote(Kind::Digits, Kind::Expr),
        Rule::right(Kind::Expr, Kind::MulOp, Kind::ExprMulHole),
        Rule::left(Kind::ExprMulHole, Kind::Expr, Kind::Mul),
        Rule::promote(Kind::Mul, Kind::Expr),
        Rule::right(Kind::Expr, Kind::AddOp, Kind::ExprAddHole),
        Rule::left(Kind::ExprAddHole, Kind::Expr, Kind::Add),
        Rule::promote(Kind::Add, Kind::Expr),
        Rule::left(Kind::OpenParen, Kind::Expr, Kind::OpenParen),
        Rule::paste(Kind::OpenParen, Kind::CloseParen, Kind::Expr),
    ]
}

#[derive(Debug, Default, Clone, PartialEq, Hash, Eq)]
struct TableRow {
    entries: Vec<Entry>,
    // Index of the last entry to have had all rules applied.
    processed: usize,
}

#[derive(Default, Clone, PartialEq, Hash, Eq)]
struct State<'a> {
    input: &'a str,
    rules: &'a [Rule],
    // Finish subexpressions
    expression_table: [TableRow; Kind::COUNT],

    // Tokens / expressions being parsed.
    entries: Vec<Entry>,

    // Performance / work counters
    loop_runs: u32,
    rule_runs: u32,
    token_runs: u32,
}

impl std::fmt::Debug for State<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "INPUT")?;
        writeln!(f, "  {}", self.input)?;
        writeln!(f)?;
        /*
        writeln!(f, "RULES")?;
        for rule in self.rules {
            writeln!(f, "  {rule:?}")?;
        }
        writeln!(f)?;
        */

        writeln!(f, "TABLES")?;
        for symbol in Kind::iter() {
            let table = &self[&symbol];
            writeln!(
                f,
                "  {symbol} {table:?}",
                symbol = rpad(&format!("{:?}", symbol), 12, " ")
            )?;
        }
        writeln!(f)?;

        writeln!(f, "PERF")?;
        writeln!(
            f,
            "  Ran on {num_rules:3} rules      {rule_runs:4} times",
            num_rules = self.rules.len(),
            rule_runs = self.rule_runs
        )?;
        writeln!(
            f,
            "  Ran on {num_bytes:3} bytes with {token_runs:4} merge/promotion attempts",
            num_bytes = self.input.len(),
            token_runs = self.token_runs
        )?;
        writeln!(
            f,
            "  Ran                    {loops:3} times",
            loops = self.loop_runs
        )?;
        Ok(())
    }
}

impl std::ops::Index<&Kind> for State<'_> {
    type Output = Vec<Entry>;

    fn index<'a>(&'a self, row: &Kind) -> &'a Vec<Entry> {
        &self.expression_table[*row as usize].entries
    }
}

impl std::ops::IndexMut<&Kind> for State<'_> {
    fn index_mut<'a>(&'a mut self, row: &Kind) -> &'a mut Vec<Entry> {
        &mut self.expression_table[*row as usize].entries
    }
}

const EMPTY_ROW: TableRow = TableRow {
    entries: Vec::new(),
    processed: 0,
};
const EMPTY_TABLE: [TableRow; Kind::COUNT] = [EMPTY_ROW; Kind::COUNT];

impl<'a> State<'a> {
    fn run_rule(&mut self, rule: &Rule) -> bool {
        self.rule_runs += 1;
        let mut progress = false;
        // println!("{self:?}");
        // println!("  {:3?}: {rule:?}", self.rule_runs);
        match rule {
            Rule::Merge {
                mode,
                left,
                right,
                out,
            } => {
                // FIXME: Scan backwards for ParentChoice::Right.
                let mut delete = 0;
                let mut output_index = 0;
                let mut li = 0;
                let mut ri = 1;
                while ri < self.entries.len() {
                    self.token_runs += 1;
                    let start = self.entries[li].at;
                    let end = self.entries[ri].end;
                    let l = self.entries[li].clone(); // TODO: Avoid clone
                    let r = self.entries[ri].clone(); // TODO: Avoid clone
                    if l.kind != *left || r.kind != *right {
                        // Move up!
                        self.entries[output_index] = l;
                        li = ri;
                        output_index += 1;
                        self.entries[output_index] = r;
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
                        id: self[out].len(),
                        at: start,
                        end,
                        nodes: vec![], // todo update.
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
                            new.nodes.push((*out, r.id));
                            // FIXME: Capture multiple children
                        }
                        ParentChoice::Right => {
                            // Right is parent, preserve the left as a child.
                            new.nodes.push((*out, l.id));
                            // FIXME: Capture multiple children
                        }
                    }
                    self[out].push(new.clone());
                    self.entries[output_index] = new;
                }
                for _ in 0..delete {
                    self.entries.pop();
                }
            }
            Rule::Promote { from, to } => {
                // println!();
                for i in 0..self.entries.len() {
                    self.token_runs += 1;
                    // TODO: Avoid reserving id here to avoid locking issues.
                    let id = self[to].len();
                    let entry = &mut self.entries[i];

                    if entry.kind != *from {
                        continue;
                    }
                    // println!("    {entry:?}");
                    progress = true;
                    entry.kind = *to;
                    entry.nodes = vec![(*from, entry.id)];
                    entry.id = id;
                    let entry = entry.clone();
                    self[to].push(entry);
                }
            }
        }
        progress
    }

    fn run(&mut self) {
        // TODO: Only run rules with inputs (/ updates).
        // TODO: Is this working? Helping?
        let mut earliest_progressing_rule = 0;
        while earliest_progressing_rule < self.rules.len() {
            self.loop_runs += 1;
            let start = earliest_progressing_rule;
            earliest_progressing_rule = self.rules.len();
            for (rule_index, rule) in self.rules[start..].iter().enumerate() {
                // println!("{:?}", self.entries);
                if self.run_rule(rule) {
                    earliest_progressing_rule =
                        std::cmp::min(earliest_progressing_rule, rule_index);
                }
                // println!();
            }
        }
    }
}

fn setup<'a>(input: &'a str, rules: &'a [Rule]) -> State<'a> {
    let mut state = State {
        input,
        rules,
        expression_table: EMPTY_TABLE.clone(),
        entries: vec![],
        loop_runs: 0,
        rule_runs: 0,
        token_runs: 0,
    };
    for (at, ch) in input.chars().enumerate() {
        let Some(kind) = classify_char(ch) else {
            todo!("DID NOT HANDLE {ch:?}");
        };
        let id = state[&kind].len();
        let entry = Entry {
            kind,
            id,
            at,
            end: at + 1,
            nodes: vec![],
        };
        state.entries.push(entry.clone());
        state[&kind].push(entry);
    }
    // println!("{entries:#?}", entries=state.entries);

    state.run();
    println!("{state:?}");
    state
}

mod example1 {
    use super::*;
    const EXAMPLE: &str = "(1+201)*3";

    #[test]
    fn table_test_1() {
        let rules = &get_rules()[..1]; // Digits* -> Digits
        let state = setup(EXAMPLE, rules);
        assert_eq!(
            format!("{:?}", state.entries),
            "[OpenParen@0..1, Digits@1..2, AddOp@2..3, Digits@3..6, CloseParen@6..7, MulOp@7..8, Digits@8..9]"
        );
    }

    #[test]
    fn table_test_2() {
        let rules = &get_rules()[..2]; // Digits -> Expr
        let state = setup(EXAMPLE, rules);
        assert_eq!(format!("{:?}", state.entries),
            "[OpenParen@0..1, Expr@1..2(Digits_0), AddOp@2..3, Expr@3..6(Digits_6), CloseParen@6..7, MulOp@7..8, Expr@8..9(Digits_4)]"
        );
    }

    #[test]
    fn table_test_3() {
        let rules = &get_rules()[..3]; // Expr * -> ExprMulHole
        let state = setup(EXAMPLE, rules);
        assert_eq!(format!("{:?}", state.entries), "[OpenParen@0..1, Expr@1..2(Digits_0), AddOp@2..3, Expr@3..6(Digits_6), CloseParen@6..7, MulOp@7..8, Expr@8..9(Digits_4)]");
        // TODO: Add test case for this that is not a noop.
    }

    #[test]
    fn table_test_4() {
        let rules = &get_rules()[..4]; // ExprMulHole Expr -> Expr
        let state = setup(EXAMPLE, rules);
        assert_eq!(format!("{:?}", state.entries), "[OpenParen@0..1, Expr@1..2(Digits_0), AddOp@2..3, Expr@3..6(Digits_6), CloseParen@6..7, MulOp@7..8, Expr@8..9(Digits_4)]");
        // TODO: Add test case for this that is not a noop.
    }

    #[test]
    fn table_test_5() {
        let rules = &get_rules()[..5]; // Expr + -> ExprAddHole
        let state = setup(EXAMPLE, rules);
        assert_eq!(format!("{:?}", state.entries), "[OpenParen@0..1, ExprAddHole@1..3(Digits_0), Expr@3..6(Digits_1), CloseParen@6..7, MulOp@7..8, Expr@8..9(Digits_4)]");
        // TODO: Add test case for this that is not a noop.
    }

    #[test]
    fn table_test_6() {
        let rules = &get_rules()[..6]; // ExprAddHole Expr -> Expr
        let state = setup(EXAMPLE, rules);
        assert_eq!(
            format!("{:?}", state.entries),
            "[OpenParen@0, Expr@1(Add_0), CloseParen@6, MulOp@7, Expr@8(Digits_4)]"
        );
        // TODO: Add test case for this that is not a noop.
    }

    #[test]
    fn table_test_7() {
        let rules = &get_rules()[..7]; // OpenParen Expr -> OpenParen
        let state = setup(EXAMPLE, rules);
        assert_eq!(
            format!("{:?}", state.entries),
            "[OpenParen@0(Noop_1), CloseParen@6, MulOp@7, Expr@8(Digits_4)]"
        );
        // TODO: Add test case for this that is not a noop.
    }

    #[test]
    fn table_test_all() {
        let rules = get_rules();
        let state = setup(EXAMPLE, &rules);
        assert_eq!(format!("{:?}", state.entries), "[Expr@0..9(Mul_0)]");
        // assert_eq!(format!("{:?}", state.expression_table), "");
    }
}

mod example2 {
    use super::*;
    const EXAMPLE: &str = "2+(1+201)*3";

    #[test]
    fn table_test_1() {
        let rules = &get_rules()[..1]; // Digits* -> Digits
        let state = setup(EXAMPLE, rules);
        assert_eq!(format!("{:?}", state.entries),
            "[Digits@0..1, AddOp@1..2, OpenParen@2..3, Digits@3..4, AddOp@4..5, Digits@5..8, CloseParen@8..9, MulOp@9..10, Digits@10..11]"
        );
    }

    #[test]
    fn table_test_2() {
        let rules = &get_rules()[..2]; // Digits -> Expr
        let state = setup(EXAMPLE, rules);
        assert_eq!(format!("{:?}", state.entries),
            "[Expr@0..1(Digits_0), AddOp@1..2, OpenParen@2..3, Expr@3..4(Digits_1), AddOp@4..5, Expr@5..8(Digits_7), CloseParen@8..9, MulOp@9..10, Expr@10..11(Digits_5)]"
        );
    }

    #[test]
    fn table_test_3() {
        let rules = &get_rules()[..3]; // Expr * -> ExprMulHole
        let state = setup(EXAMPLE, rules);
        assert_eq!(format!("{:?}", state.entries), "[Expr@0..1(Digits_0), AddOp@1..2, OpenParen@2..3, Expr@3..4(Digits_1), AddOp@4..5, Expr@5..8(Digits_7), CloseParen@8..9, MulOp@9..10, Expr@10..11(Digits_5)]");
        // TODO: Add test case for this that is not a noop.
    }

    #[test]
    fn table_test_4() {
        let rules = &get_rules()[..4]; // ExprMulHole Expr -> Expr
        let state = setup(EXAMPLE, rules);
        assert_eq!(format!("{:?}", state.entries), "[Expr@0..1(Digits_0), AddOp@1..2, OpenParen@2..3, Expr@3..4(Digits_1), AddOp@4..5, Expr@5..8(Digits_7), CloseParen@8..9, MulOp@9..10, Expr@10..11(Digits_5)]");
        // TODO: Add test case for this that is not a noop.
    }

    #[test]
    fn table_test_5() {
        let rules = &get_rules()[..5]; // Expr + -> ExprAddHole
        let state = setup(EXAMPLE, rules);
        assert_eq!(format!("{:?}", state.entries), "[ExprAddHole@0..1(Digits_0), OpenParen@2..3, ExprAddHole@3..5(Digits_1), Expr@5..8(Digits_7), CloseParen@8..9, MulOp@9..10, Expr@10..11(Digits_5)]");
        // TODO: Add test case for this that is not a noop.
    }

    #[test]
    fn table_test_6() {
        let rules = &get_rules()[..6]; // ExprAddHole Expr -> Expr
        let state = setup(EXAMPLE, rules);
        assert_eq!(format!("{:?}", state.entries),
        "[ExprAddHole@0..2(ExprAddHole_0), OpenParen@2..3, Expr@3..4(Expr_1), Expr@5..8(Expr_2), CloseParen@8..9, MulOp@9..10, Expr@10..11(Expr_3)]");
        // TODO: Add test case for this that is not a noop.
    }

    #[test]
    fn table_test_7() {
        let rules = &get_rules()[..7]; // OpenParen Expr -> OpenParen
        let state = setup(EXAMPLE, rules);
        assert_eq!(
            format!("{:?}", state.entries),
            "[ExprAddHole@0..2(ExprAddHole_0), OpenParen@2..3, Expr@3..8(Expr_2), CloseParen@8..9, MulOp@9..10, Expr@10..11(Expr_3)]",
        );
        // TODO: Add test case for this that is not a noop.
    }

    #[test]
    fn table_test_all() {
        let rules = get_rules();
        let state = setup(EXAMPLE, &rules);
        assert_eq!(format!("{:?}", state.entries), "[Expr@0..11(Add_1)]");
        // assert_eq!(format!("{:?}", state.expression_table), "");
    }
}
