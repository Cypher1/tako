use crate::ast::{Info, Node};
use crate::primitives::Val;

use thiserror::Error;

use derivative::Derivative;
#[derive(Error, PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Derivative)]
#[derivative(Debug)]
pub enum TError {
    #[error("call to C++ compiler failed with error code: {1:?}\n{0}")]
    CppCompilerError(String, Option<i32>, Info),

    #[error("unknown symbol `{0}` in {2} at {1}")]
    UnknownSymbol(String, Info, String),
    #[error("out of scope type variable `{0}` at {1}")]
    OutOfScopeTypeVariable(String, Info),
    #[error("unknown infix operator `{0}` at {1}")]
    UnknownInfixOperator(String, Info),
    #[error("unknown prefix operator `{0}` at {1}")]
    UnknownPrefixOperator(String, Info),
    #[error(
        "unknown size of variable type (i.e. type variables could have any size) `{0}` at {1}"
    )]
    UnknownSizeOfVariableType(String, Info),
    #[error("unknown size of abstract type (i.e. size is dependant on context) `{0}` at {1}")]
    UnknownSizeOfAbstractType(String, Info),
    #[error(
        "unknown cardinality of abstract type (i.e. cardinality is dependant on context) `{0}` at {1}"
    )]
    UnknownCardOfAbstractType(String, Info),

    #[error("unknown cardinality of static pointers (i.e. static pointers could point at anything) at {0}")]
    StaticPointerCardinality(Info),

    #[error("impossible type, {0} at {1}")]
    TypeMismatch(String, Box<Val>, Info),
    #[error("type mismatch, arguments {0}, {1} vs {2} at {3}")]
    TypeMismatch2(String, Box<Val>, Box<Val>, Info),
    #[error("runtime requirement failed at {0}")]
    RequirementFailure(Info),

    #[error("parse failed, {0} at {1}")]
    ParseError(String, Info),
    #[error("internal error `{0}` at {1}")]
    InternalError(String, Info),

    #[error("Expected a let node, got `{0}`")]
    ExpectedLetNode(Box<Node>),

    #[error("failed to find type recorded for path `{0}` at {1}")]
    UnknownPath(String, Info),
}

impl From<std::fmt::Error> for TError {
    fn from(error: std::fmt::Error) -> Self {
        use TError::InternalError;
        InternalError(error.to_string(), Info::default())
    }
}

impl From<std::io::Error> for TError {
    fn from(error: std::io::Error) -> Self {
        use TError::InternalError;
        InternalError(error.to_string(), Info::default())
    }
}
impl From<std::num::ParseIntError> for TError {
    fn from(error: std::num::ParseIntError) -> Self {
        use TError::ParseError;
        ParseError(error.to_string(), Info::default())
    }
}
