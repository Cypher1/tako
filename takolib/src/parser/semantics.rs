use super::tokens::Symbol::{self, *};
use lazy_static::lazy_static;

#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum Literal {
    Bool,    // A boolean of arbitrary size :P (true/false)
    Numeric, // An Integer or Float of arbitrary size
    String,  // A character or strings of arbitrary size (e.g. UTF-8 or Unicode)
    Color,   // A color of arbitrary size in Hex. e.g. #ff00ff (purple)
    Array,   // An abstract array literal, any of Vector, Array, List, Set, etc. (e.g. [123, 234])
    Map, // An abstract map literal, any of OrderedMap, HashMap, Dictionary, etc. (e.g. { 'a': 123, 'b': 234 })
}

#[derive(Default, Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum BindingMode {
    #[default]
    Given, // i.e. value, lambda/given x, y
    Forall, // i.e. dependant type, pi/forall x, y
    With,   // i.e. dependant type, sigma/with/exists x, y
}

impl std::fmt::Display for BindingMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Given => "given",
                Self::Forall => "forall",
                Self::With => "with",
            }
        )
    }
}

pub const fn op_from_assign_op(s: Symbol) -> Option<Symbol> {
    // TODO(clarity): Move to a symbol module.
    Some(match s {
        AddAssign => Add,
        SubAssign => Sub,
        DivAssign => Div,
        MulAssign => Mul,
        AndAssign => And,
        OrAssign => Or,
        BitXorAssign => BitXor,
        LogicalAndAssign => LogicalAnd,
        LogicalOrAssign => LogicalOr,
        ModuloAssign => Modulo,
        _ => return None,
    })
}

pub const fn binding_mode_from_op(s: Symbol) -> Option<BindingMode> {
    // TODO(clarity): Move to a symbol module.
    Some(match s {
        Lambda => BindingMode::Given,
        Pi => BindingMode::Forall,
        Forall => BindingMode::Forall,
        Exists => BindingMode::With,
        Sigma => BindingMode::With,
        _ => return None,
    })
}

pub const fn is_assign(s: Symbol) -> bool {
    // TODO(clarity): Move to a symbol module.
    matches!(s, Assign) || op_from_assign_op(s).is_some()
}

// TODO: Make lazy / single init.
lazy_static! {
    // Left associativity is the current default.
    static ref RIGHT_ASSOCIATIVE: HashSet<Symbol> = hash_set!{
        OpenParen,
        OpenCurly,
        OpenBracket,
        Exp,
        Sequence,
    };
    static ref ASSOCIATIVE: HashSet<Symbol> = hash_set!{
        Add,
        Mul,
        And, // Note: LogicalAnd is not associative due to shortcircuiting
        Or, // Note: LogicalOr is not associative due to shortcircuiting
        BitXor,
    };

    /*
    Source: https://www.foonathan.net/2017/07/operator-precedence/

    Inside the categories the relative precedence of the operators is as follows:

        logical operators: ! > &&,||, but not mixed && and || chains

        comparison operators: no chaining at all

        mathematical operators: unary +,- > *,/ > +,-, with the usual associativity

        bitwise operators: unary ~ before the binary operators, but again no mixed chaining of &, | and ^ and no chaining of the shift operators

        unary operators: just as usual
    */
    static ref LOOSER_THAN_MAP: HashMap<Symbol, Vec<Symbol>> = map!{
        OpenParen => vec![OpenCurly],
        OpenCurly => vec![OpenBracket],
        OpenBracket => vec![Sequence],
        Sequence => vec![
            Assign,
            AddAssign,
            SubAssign,
            DivAssign,
            MulAssign,
            AndAssign,
            OrAssign,
            BitXorAssign,
            LogicalAndAssign,
            LogicalOrAssign,
            ModuloAssign,
        ],
        Assign => vec![LeftPipe],
        AddAssign => vec![LeftPipe],
        SubAssign => vec![LeftPipe],
        DivAssign => vec![LeftPipe],
        MulAssign => vec![LeftPipe],
        AndAssign => vec![LeftPipe],
        OrAssign => vec![LeftPipe],
        BitXorAssign => vec![LeftPipe],
        LogicalAndAssign => vec![LeftPipe],
        LogicalOrAssign => vec![LeftPipe],
        ModuloAssign => vec![LeftPipe],
        LeftPipe => vec![RightPipe],
        RightPipe => vec![Sigma],
        Sigma => vec![Lambda],
        Lambda => vec![Arrow],
        Arrow => vec![DoubleArrow],
        DoubleArrow => vec![Forall],
        Forall => vec![Pi],
        Pi => vec![Exists],
        Exists => vec![
            HasType,
            Eqs,
            NotEqs,
            Lt,
            LtEqs,
            Gt,
            GtEqs,
        ],
        HasType => vec![Add, LeftShift, RightShift, Try],
        Eqs => vec![Add, LeftShift, RightShift, Try],
        NotEqs => vec![Add, LeftShift, RightShift, Try],
        Lt => vec![Add, LeftShift, RightShift, Try],
        LtEqs => vec![Add, LeftShift, RightShift, Try],
        Gt => vec![Add, LeftShift, RightShift, Try],
        GtEqs => vec![Add, LeftShift, RightShift, Try],
        LeftShift => vec![BitNot],
        RightShift => vec![BitNot],
        Add => vec![Sub],
        Sub => vec![Div],
        Div => vec![Mul],
        Mul => vec![Exp],
        Exp => vec![And, LogicalAnd, BitXor, Modulo, GetAddress],
        And => vec![Or],
        Or => vec![And],
        LogicalAnd => vec![LogicalOr],
        LogicalOr => vec![LogicalAnd],
        BitXor => vec![BitNot, LogicalNot],
        Try => vec![Dot],
        Dot => vec![Range],
        Range => vec![Spread],
        Spread => vec![Escape],
    };

    static ref LOOSER_THAN: HashSet<(Symbol, Symbol)> = {
        let mut looser_than = hash_set!{};
        for (k, vs) in LOOSER_THAN_MAP.iter() {
            for v in vs {
                looser_than.insert((*k, *v));
            }
        }
        loop {
            let mut looser_than_news = hash_set!{};
            for (a, b1) in &looser_than {
                if a == b1 {
                    trace!("Precedence cycle on {a:?} {b1:?}");
                }
                for (b2, c) in &looser_than {
                    let transitive = (*a, *c);
                    if b1 == b2 && !looser_than.contains(&transitive) {
                        if a == c {
                            trace!("Precedence cycle on {a:?} {b1:?} {c:?}");
                        }
                        looser_than_news.insert(transitive);
                    }
                }
            }
            if looser_than_news.is_empty() {
                break;
            }
            looser_than.extend(looser_than_news);
        }
        looser_than
    };
}

impl Symbol {
    #[must_use]
    pub fn is_associative(&self) -> bool {
        ASSOCIATIVE.contains(self)
    }

    #[must_use]
    pub fn is_right_associative(&self) -> bool {
        RIGHT_ASSOCIATIVE.contains(self)
    }

    #[must_use]
    pub fn is_left_associative(&self) -> bool {
        !(self.is_associative() || self.is_right_associative())
    }

    #[must_use]
    pub fn binding_type(&self) -> OpBinding {
        match self {
            Escape
            | BitNot
            | LogicalNot
            | GetAddress
            | Spread
            | Lambda
            | Sigma
            | Forall
            | Pi
            | Exists => OpBinding::PrefixOp,
            Try => OpBinding::PostfixOp,
            Sub => OpBinding::PrefixOrInfixBinOp,
            CloseCurly => OpBinding::Close(OpenCurly),
            CloseParen => OpBinding::Close(OpenParen),
            CloseBracket => OpBinding::Close(CloseParen),
            OpenCurly => OpBinding::Open(CloseCurly),
            OpenParen => OpBinding::Open(CloseParen),
            OpenBracket => OpBinding::Open(CloseBracket),
            Sequence => OpBinding::InfixOrPostfixBinOp,
            _ => OpBinding::InfixBinOp,
        }
    }

    #[must_use]
    pub fn is_looser(&self, other: Self) -> bool {
        if *self == other {
            return self.is_right_associative();
        }
        LOOSER_THAN.contains(&(*self, other))
    }
}
