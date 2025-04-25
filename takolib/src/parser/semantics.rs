use std::collections::HashSet;

use super::tokens::Symbol::{self, *};
use better_std::hash_set;
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
    static ref ASSOCIATIVE: HashSet<Symbol> = hash_set!{
        Add,
        Mul,
        And, // Note: LogicalAnd is not associative due to shortcircuiting
        Or, // Note: LogicalOr is not associative due to shortcircuiting
        BitXor,
    };
}
