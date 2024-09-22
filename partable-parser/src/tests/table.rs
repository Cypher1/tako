use strum::{EnumCount, IntoEnumIterator};
use strum_macros::{EnumCount, EnumIter};
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
        '+' => Kind::Add,
        '*' => Kind::Mul,
        '(' => Kind::OpenParen,
        ')' => Kind::CloseParen,
        '0'..'9' => Kind::Digits,
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

type Pattern = Kind; // TODO: Implement negative patterns if necessary

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

    fn paste<L: Into<Pattern>, R: Into<Pattern>>(
        left: L,
        right: R,
        out: Kind,
        expr: SubExprKind,
    ) -> Self {
        Self::Merge {
            left: left.into(),
            right: right.into(),
            out,
            mode: ParentChoice::Both,
            expr,
        }
    }
    fn left<L: Into<Pattern>, R: Into<Pattern>>(
        left: L,
        right: R,
        out: Kind,
        expr: SubExprKind,
    ) -> Self {
        Self::Merge {
            left: left.into(),
            right: right.into(),
            out,
            mode: ParentChoice::Left,
            expr,
        }
    }
    fn right<L: Into<Pattern>, R: Into<Pattern>>(
        left: L,
        right: R,
        out: Kind,
        expr: SubExprKind,
    ) -> Self {
        Self::Merge {
            left: left.into(),
            right: right.into(),
            out,
            mode: ParentChoice::Right,
            expr,
        }
    }
    fn promote<F: Into<Pattern>>(from: F, to: Kind, expr: SubExprKind) -> Self {
        Self::Promote {
            from: from.into(),
            to,
            expr,
        }
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

#[derive(Default, Clone, PartialEq, Hash, Eq)]
struct State<'a> {
    input: &'a str,
    rules: &'a [Rule],
    // Finish subexpressions
    expression_table: [Vec<Entry>; SubExprKind::COUNT],

    // Tokens / expressions being parsed.
    entries: Vec<Entry>,

    // Performance / work counters
    loop_runs: u32,
    rule_runs: u32,
    token_runs: u32,
}

fn padded<T: std::fmt::Debug>(d: T, s: usize) -> String {
    let content = format!("{:?}", d);
    format!(
        "{content}{p}",
        p = " ".repeat(s.saturating_sub(content.len()))
    )
}

impl std::fmt::Debug for State<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "INPUT")?;
        writeln!(f, "  {}", self.input)?;
        writeln!(f)?;
        writeln!(f, "RULES")?;
        for rule in self.rules {
            writeln!(f, "  {rule:?}")?;
        }
        writeln!(f)?;

        writeln!(f, "TABLES")?;
        for symbol in SubExprKind::iter() {
            let table = &self[&symbol];
            writeln!(f, "  {symbol}: {table:?}", symbol = padded(symbol, 10))?;
        }
        writeln!(f)?;

        writeln!(f, "PERF")?;
        writeln!(
            f,
            "  Ran {num_rules} rules {rule_runs} times",
            num_rules = self.rules.len(),
            rule_runs = self.rule_runs
        )?;
        writeln!(
            f,
            "  Ran on {num_bytes} bytes with {token_runs} merge/promotion attempts",
            num_bytes = self.input.len(),
            token_runs = self.token_runs
        )?;
        writeln!(f, "  Ran loop {loops} times", loops = self.loop_runs)?;
        Ok(())
    }
}

impl std::ops::Index<&SubExprKind> for State<'_> {
    type Output = Vec<Entry>;

    fn index<'a>(&'a self, row: &SubExprKind) -> &'a Vec<Entry> {
        &self.expression_table[*row as usize]
    }
}

impl std::ops::IndexMut<&SubExprKind> for State<'_> {
    fn index_mut<'a>(&'a mut self, row: &SubExprKind) -> &'a mut Vec<Entry> {
        &mut self.expression_table[*row as usize]
    }
}

const EMPTY_ROW: Vec<Entry> = Vec::new();
const EMPTY_TABLE: [Vec<Entry>; SubExprKind::COUNT] = [EMPTY_ROW; SubExprKind::COUNT];

impl<'a> State<'a> {
    fn run_rule(&mut self, rule: &Rule) -> bool {
        self.rule_runs += 1;
        let mut progress = false;
        // println!("{self:?}");
        match rule {
            Rule::Merge {
                mode,
                left,
                right,
                out,
                expr,
            } => {
                println!("  {:3?}: {left:?} {right:?} => {out:?} {mode:?}", self.rule_runs);
                // FIXME: Scan backwards for ParentChoice::Right.
                let mut delete = 0;
                let mut output_index = 0;
                let mut li = 0;
                let mut ri = 1;
                while ri < self.entries.len() {
                    self.token_runs += 1;
                    let l = self.entries[li];
                    let r = self.entries[ri];
                    if l.kind != *left || r.kind != *right {
                        // Move up!
                        self.entries[output_index] = l;
                        output_index += 1;
                        self.entries[output_index] = r;
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
                            new.node = Some((*expr, self[expr].len()));
                            // FIXME: Capture multiple children
                            self[expr].push(r);
                        }
                        ParentChoice::Right => {
                            // Right is parent, preserve the left as a child.
                            new.node = Some((*expr, self[expr].len()));
                            // FIXME: Capture multiple children
                            self[expr].push(l);
                        }
                    }
                    self.entries[output_index] = new;
                }
                for _ in 0..delete {
                    self.entries.pop();
                }
            }
            Rule::Promote { from, to, expr } => {
                println!("  {:3?}: {from:?} => {to:?}", self.rule_runs);
                // println!();
                for i in 0..self.entries.len() {
                    self.token_runs += 1;
                    // TODO: Avoid reserving id here to avoid locking issues.
                    let id = self[expr].len();
                    let entry = &mut self.entries[i];

                    if entry.kind != *from {
                        continue;
                    }
                    // println!("    {entry:?}");
                    progress = true;
                    entry.kind = *to;
                    entry.node = Some((*expr, id));
                    let entry = self.entries[i];
                    self[expr].push(entry);
                }
            }
        }
        progress
    }

    fn run(&mut self) -> () {
        let mut earliest_progressing_rule = 0; // TODO: Is this working? Helping?
        while earliest_progressing_rule < self.rules.len() {
            self.loop_runs += 1;
            let start = earliest_progressing_rule;
            earliest_progressing_rule = self.rules.len();
            for (rule_index, rule) in self.rules[start..].iter().enumerate() {
                // println!("{:?}", self.entries);
                if self.run_rule(&rule) {
                    earliest_progressing_rule =
                        std::cmp::min(earliest_progressing_rule, rule_index);
                }
                // println!();
            }
        }
    }
}

fn setup<'a>(input: &'a str, rules: &'a [Rule]) -> State<'a> {
    println!("START");

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
        input,
        rules,
        expression_table: EMPTY_TABLE.clone(),
        entries,
        loop_runs: 0,
        rule_runs: 0,
        token_runs: 0,
    };
    state.run();
    println!("{state:?}");
    state
}

mod example1 {
    use super::*;
    const EXAMPLE: &'static str = "(1+201)*3";

    #[test]
    fn table_test_1() {
        let rules = &get_rules()[..1]; // Digits* -> Digits
        let state = setup(EXAMPLE, rules);
        assert_eq!(
            format!("{:?}", state.entries),
            "[OpenParen@0, Digits@1, Add@2, Digits@3, CloseParen@6, Mul@7, Digits@8]"
        );
    }

    #[test]
    fn table_test_2() {
        let rules = &get_rules()[..2]; // Digits -> Integer
        let state = setup(EXAMPLE, rules);
        assert_eq!(format!("{:?}", state.entries), "[OpenParen@0, Integer@1(NumLit 0), Add@2, Integer@3(NumLit 1), CloseParen@6, Mul@7, Integer@8(NumLit 2)]");
    }

    #[test]
    fn table_test_3() {
        let rules = &get_rules()[..3]; // Integer * -> IntegerMulHole
        let state = setup(EXAMPLE, rules);
        assert_eq!(format!("{:?}", state.entries), "[OpenParen@0, Integer@1(NumLit 0), Add@2, Integer@3(NumLit 1), CloseParen@6, Mul@7, Integer@8(NumLit 2)]");
        // TODO: Add test case for this that is not a noop.
    }

    #[test]
    fn table_test_4() {
        let rules = &get_rules()[..4]; // IntegerMulHole Integer -> Integer
        let state = setup(EXAMPLE, rules);
        assert_eq!(format!("{:?}", state.entries), "[OpenParen@0, Integer@1(NumLit 0), Add@2, Integer@3(NumLit 1), CloseParen@6, Mul@7, Integer@8(NumLit 2)]");
        // TODO: Add test case for this that is not a noop.
    }

    #[test]
    fn table_test_5() {
        let rules = &get_rules()[..5]; // Integer + -> IntegerAddHole
        let state = setup(EXAMPLE, rules);
        assert_eq!(format!("{:?}", state.entries), "[OpenParen@0, IntegerAddHole@1(None 0), Integer@3(NumLit 1), CloseParen@6, Mul@7, Integer@8(NumLit 2)]");
        // TODO: Add test case for this that is not a noop.
    }

    #[test]
    fn table_test_6() {
        let rules = &get_rules()[..6]; // IntegerAddHole Integer -> Integer
        let state = setup(EXAMPLE, rules);
        assert_eq!(
            format!("{:?}", state.entries),
            "[OpenParen@0, Integer@1(Add 0), CloseParen@6, Mul@7, Integer@8(NumLit 2)]"
        );
        // TODO: Add test case for this that is not a noop.
    }

    #[test]
    fn table_test_7() {
        let rules = &get_rules()[..7]; // OpenParen Integer -> OpenParen
        let state = setup(EXAMPLE, rules);
        assert_eq!(
            format!("{:?}", state.entries),
            "[OpenParen@0(None 1), CloseParen@6, Mul@7, Integer@8(NumLit 2)]"
        );
        // TODO: Add test case for this that is not a noop.
    }

    #[test]
    fn table_test_all() {
        let rules = get_rules();
        let state = setup(EXAMPLE, &rules);
        assert_eq!(format!("{:?}", state.entries), "[Integer@0(Mul 0)]");
        assert_eq!(format!("{:?}", state.expression_table), "");
    }
}

mod example2 {
    use super::*;
    const EXAMPLE: &'static str = "2+(1+201)*3";

    #[test]
    fn table_test_1() {
        let rules = &get_rules()[..1]; // Digits* -> Digits
        let state = setup(EXAMPLE, rules);
        assert_eq!(format!("{:?}", state.entries), "[Digits@0, Add@1, OpenParen@2, Digits@3, Add@4, Digits@5, CloseParen@8, Mul@9, Digits@10]");
    }

    #[test]
    fn table_test_2() {
        let rules = &get_rules()[..2]; // Digits -> Integer
        let state = setup(EXAMPLE, rules);
        assert_eq!(format!("{:?}", state.entries), "[Integer@0(NumLit 0), Add@1, OpenParen@2, Integer@3(NumLit 1), Add@4, Integer@5(NumLit 2), CloseParen@8, Mul@9, Integer@10(NumLit 3)]");
    }

    #[test]
    fn table_test_3() {
        let rules = &get_rules()[..3]; // Integer * -> IntegerMulHole
        let state = setup(EXAMPLE, rules);
        assert_eq!(format!("{:?}", state.entries), "[Integer@0(NumLit 0), Add@1, OpenParen@2, Integer@3(NumLit 1), Add@4, Integer@5(NumLit 2), CloseParen@8, Mul@9, Integer@10(NumLit 3)]");
        // TODO: Add test case for this that is not a noop.
    }

    #[test]
    fn table_test_4() {
        let rules = &get_rules()[..4]; // IntegerMulHole Integer -> Integer
        let state = setup(EXAMPLE, rules);
        assert_eq!(format!("{:?}", state.entries), "[Integer@0(NumLit 0), Add@1, OpenParen@2, Integer@3(NumLit 1), Add@4, Integer@5(NumLit 2), CloseParen@8, Mul@9, Integer@10(NumLit 3)]");
        // TODO: Add test case for this that is not a noop.
    }

    #[test]
    fn table_test_5() {
        let rules = &get_rules()[..5]; // Integer + -> IntegerAddHole
        let state = setup(EXAMPLE, rules);
        assert_eq!(format!("{:?}", state.entries), "[IntegerAddHole@0(None 0), OpenParen@2, Integer@3(NumLit 1), Integer@5(NumLit 2), CloseParen@8, Mul@9, Integer@10(NumLit 3)]");
        // TODO: Add test case for this that is not a noop.
    }

    #[test]
    fn table_test_6() {
        let rules = &get_rules()[..6]; // IntegerAddHole Integer -> Integer
        let state = setup(EXAMPLE, rules);
        assert_eq!(format!("{:?}", state.entries), "[IntegerAddHole@0(None 0), OpenParen@2, Integer@3(NumLit 1), Integer@5(NumLit 2), CloseParen@8, Mul@9, Integer@10(NumLit 3)]");
        // TODO: Add test case for this that is not a noop.
    }

    #[test]
    fn table_test_7() {
        let rules = &get_rules()[..7]; // OpenParen Integer -> OpenParen
        let state = setup(EXAMPLE, rules);
        assert_eq!(format!("{:?}", state.entries), "[IntegerAddHole@0(None 0), OpenParen@2(None 3), CloseParen@8, Mul@9, Integer@10(NumLit 3)]");
        // TODO: Add test case for this that is not a noop.
    }

    #[test]
    fn table_test_all() {
        let rules = get_rules();
        let state = setup(EXAMPLE, &rules);
        assert_eq!(format!("{:?}", state.entries), "[Integer@0(Add 0)]");
        assert_eq!(format!("{:?}", state.expression_table), "");
    }
}
