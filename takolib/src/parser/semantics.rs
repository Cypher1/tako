use super::tokens::Symbol;

#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum Literal {
    Bool,    // A boolean of arbitrary size :P (true/false)
    Numeric, // An Integer or Float of arbitrary size
    Text,    // A character or strings of arbitrary size (e.g. UTF-8 or Unicode)
    Color,   // A color of arbitrary size in Hex. e.g. #ff00ff (purple)
    Array,   // An abstract array literal, any of Vector, Array, List, Set, etc. (e.g. [123, 234])
    Map, // An abstract map literal, any of OrderedMap, HashMap, Dictionary, etc. (e.g. { 'a': 123, 'b': 234 })
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum BindingMode {
    Given,  // i.e. value, lambda/given x, y
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
