use super::tokens::Symbol;
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
        Symbol::AddAssign => Symbol::Add,
        Symbol::SubAssign => Symbol::Sub,
        Symbol::DivAssign => Symbol::Div,
        Symbol::MulAssign => Symbol::Mul,
        Symbol::AndAssign => Symbol::And,
        Symbol::OrAssign => Symbol::Or,
        Symbol::BitXorAssign => Symbol::BitXor,
        Symbol::LogicalAndAssign => Symbol::LogicalAnd,
        Symbol::LogicalOrAssign => Symbol::LogicalOr,
        Symbol::ModuloAssign => Symbol::Modulo,
        _ => return None,
    })
}

pub const fn binding_mode_from_op(s: Symbol) -> Option<BindingMode> {
    // TODO(clarity): Move to a symbol module.
    Some(match s {
        Symbol::Lambda => BindingMode::Given,
        Symbol::Pi => BindingMode::Forall,
        Symbol::Forall => BindingMode::Forall,
        Symbol::Exists => BindingMode::With,
        Symbol::Sigma => BindingMode::With,
        _ => return None,
    })
}

pub const fn is_assign(s: Symbol) -> bool {
    // TODO(clarity): Move to a symbol module.
    matches!(s, Symbol::Assign) || op_from_assign_op(s).is_some()
}

// TODO: Make lazy / single init.
lazy_static! {
    // Left associativity is the current default.
    pub static ref RIGHT_ASSOCIATIVE: HashSet<Symbol> = hash_set!{
        Symbol::OpenParen,
        Symbol::OpenCurly,
        Symbol::OpenBracket,
        Symbol::Exp,
        Symbol::Sequence,
    };
    pub static ref ASSOCIATIVE: HashSet<Symbol> = hash_set!{
        Symbol::Add,
        Symbol::Mul,
        Symbol::And, // Note: LogicalAnd is not associative due to shortcircuiting
        Symbol::Or, // Note: LogicalOr is not associative due to shortcircuiting
        Symbol::BitXor,
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
        Symbol::OpenParen => vec![Symbol::OpenCurly],
        Symbol::OpenCurly => vec![Symbol::OpenBracket],
        Symbol::OpenBracket => vec![Symbol::Sequence],
        Symbol::Sequence => vec![
            Symbol::Assign,
            Symbol::AddAssign,
            Symbol::SubAssign,
            Symbol::DivAssign,
            Symbol::MulAssign,
            Symbol::AndAssign,
            Symbol::OrAssign,
            Symbol::BitXorAssign,
            Symbol::LogicalAndAssign,
            Symbol::LogicalOrAssign,
            Symbol::ModuloAssign,
        ],
        Symbol::Assign => vec![Symbol::LeftPipe],
        Symbol::AddAssign => vec![Symbol::LeftPipe],
        Symbol::SubAssign => vec![Symbol::LeftPipe],
        Symbol::DivAssign => vec![Symbol::LeftPipe],
        Symbol::MulAssign => vec![Symbol::LeftPipe],
        Symbol::AndAssign => vec![Symbol::LeftPipe],
        Symbol::OrAssign => vec![Symbol::LeftPipe],
        Symbol::BitXorAssign => vec![Symbol::LeftPipe],
        Symbol::LogicalAndAssign => vec![Symbol::LeftPipe],
        Symbol::LogicalOrAssign => vec![Symbol::LeftPipe],
        Symbol::ModuloAssign => vec![Symbol::LeftPipe],
        Symbol::LeftPipe => vec![Symbol::RightPipe],
        Symbol::RightPipe => vec![Symbol::Sigma],
        Symbol::Sigma => vec![Symbol::Lambda],
        Symbol::Lambda => vec![Symbol::Arrow],
        Symbol::Arrow => vec![Symbol::DoubleArrow],
        Symbol::DoubleArrow => vec![Symbol::Forall],
        Symbol::Forall => vec![Symbol::Pi],
        Symbol::Pi => vec![Symbol::Exists],
        Symbol::Exists => vec![
            Symbol::HasType,
            Symbol::Eqs,
            Symbol::NotEqs,
            Symbol::Lt,
            Symbol::LtEqs,
            Symbol::Gt,
            Symbol::GtEqs,
        ],
        Symbol::HasType => vec![Symbol::Add, Symbol::LeftShift, Symbol::RightShift, Symbol::Try],
        Symbol::Eqs => vec![Symbol::Add, Symbol::LeftShift, Symbol::RightShift, Symbol::Try],
        Symbol::NotEqs => vec![Symbol::Add, Symbol::LeftShift, Symbol::RightShift, Symbol::Try],
        Symbol::Lt => vec![Symbol::Add, Symbol::LeftShift, Symbol::RightShift, Symbol::Try],
        Symbol::LtEqs => vec![Symbol::Add, Symbol::LeftShift, Symbol::RightShift, Symbol::Try],
        Symbol::Gt => vec![Symbol::Add, Symbol::LeftShift, Symbol::RightShift, Symbol::Try],
        Symbol::GtEqs => vec![Symbol::Add, Symbol::LeftShift, Symbol::RightShift, Symbol::Try],
        Symbol::LeftShift => vec![Symbol::BitNot],
        Symbol::RightShift => vec![Symbol::BitNot],
        Symbol::Add => vec![Symbol::Sub],
        Symbol::Sub => vec![Symbol::Div],
        Symbol::Div => vec![Symbol::Mul],
        Symbol::Mul => vec![Symbol::Exp],
        Symbol::Exp => vec![Symbol::And, Symbol::LogicalAnd, Symbol::BitXor, Symbol::Modulo, Symbol::GetAddress],
        Symbol::And => vec![Symbol::Or],
        Symbol::Or => vec![Symbol::And],
        Symbol::LogicalAnd => vec![Symbol::LogicalOr],
        Symbol::LogicalOr => vec![Symbol::LogicalAnd],
        Symbol::BitXor => vec![Symbol::BitNot, Symbol::LogicalNot],
        Symbol::Try => vec![Symbol::Dot],
        Symbol::Dot => vec![Symbol::Range],
        Symbol::Range => vec![Symbol::Spread],
        Symbol::Spread => vec![Symbol::Escape],
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
