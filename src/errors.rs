use crate::ast::Info;
use crate::ast::Node;
use crate::ast::Prim;
use crate::ast::Path;

use thiserror::Error;

use derivative::Derivative;
#[derive(Error, Derivative)]
#[derivative(Debug, PartialEq, Eq, Clone)]
pub enum TError {
    #[error("call to C++ compiler failed with error code\n{1:#?}. {0}")]
    CppCompilerError(String, Option<i32>),

    #[error("unknown symbol `{0}` at {1:?}, {2:?}")]
    UnknownSymbol(String, Info, Path),
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
    #[error("type mismatch, {0} vs {1} at {2}")]
    TypeMismatch2(String, Box<Prim>, Box<Prim>, Info),
    #[error("runtime requirement failed at {0:?}")]
    RequirementFailure(Info),

    #[error("parse failed, {0} at {1:?}")]
    FailedParse(String, Info),
    #[error("internal error `{0}` at {1}")]
    InternalError(String, Node),
}
