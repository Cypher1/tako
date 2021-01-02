use crate::ast::{Info, Node};
use crate::primitives::Prim;

use thiserror::Error;

use derivative::Derivative;
#[derive(Error, PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Derivative)]
#[derivative(Debug)]
pub enum TError {
    #[error("call to C++ compiler failed with error code: {1:?}\n{0}")]
    CppCompilerError(String, Option<i32>, Info),

    #[error("unknown symbol `{0}` at {1:?}, {2}")]
    UnknownSymbol(String, Info, String),
    #[error("unknown infix operator `{0}` at {1:?}")]
    UnknownInfixOperator(String, Info),
    #[error("unknown prefix operator `{0}` at {1:?}")]
    UnknownPrefixOperator(String, Info),
    #[error(
        "unknown size of variable type (i.e. type variables could have any size) `{0}` at {1:?}"
    )]
    UnknownSizeOfVariableType(String, Info),

    #[error("unknown cardinality of static pointers (i.e. static pointers could point at anything) at {0:?}")]
    StaticPointerCardinality(Info),

    #[error("impossible type, {0} at {1}")]
    TypeMismatch(String, Box<Prim>, Info),
    #[error("type mismatch, arguments {0}, {1} vs {2} {3:?}")]
    TypeMismatch2(String, Box<Prim>, Box<Prim>, Info),
    #[error("runtime requirement failed at {0:?}")]
    RequirementFailure(Info),

    #[error("parse failed, {0} at {1:?}")]
    FailedParse(String, Info),
    #[error("internal error `{0}` at {1}")]
    InternalError(String, Box<Node>),

    #[error("Expected a let node, got `{0}`")]
    ExpectedLetNode(Box<Node>),
}
