use crate::ast::{Info, Node, Path};
use crate::primitives::Val;
use crate::tokens::Token;
use specs::Entity;

use thiserror::Error;

use derivative::Derivative;
#[derive(Error, PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Derivative)]
#[derivative(Debug)]
pub enum TError {
    #[error("call to C++ compiler failed with error code: {1:?}\n{0}")]
    CppCompilerError(String, Option<i32>, Info),
    #[error("unknown token `{0:?}` in {2:?} at {1}")]
    UnknownToken(Token, Info, Path),
    #[error("unknown symbol `{0}` in {2} at {1}")]
    UnknownSymbol(String, Info, String),
    #[error("unknown entity `{0:?}` at {1}")]
    UnknownEntity(Entity, Info),
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
    #[error(
        "stack interpreter ran out of arguments for op {0:?}, expected {1} args, got {2:?} at {3}"
    )]
    StackInterpreterRanOutOfArguments(
        Entity,
        usize,
        Vec<crate::passes::stack_interpreter::StackValue>,
        Info,
    ),
    #[error("stack interpreter ran out of code at {0}")]
    StackInterpreterRanOutOfCode(Info),

    #[error("parse failed, {0} at {1}")]
    ParseError(String, Info),
    #[error("internal error `{0}` at {1}")]
    InternalError(String, Info),

    #[error("Expected a let node, got `{0}`")]
    ExpectedLetNode(Box<Node>),

    #[error("failed to find type recorded for path `{0}` at {1}")]
    UnknownPath(String, Info),

    #[error("matched failed `{0}` at {1}")]
    MatchError(MatchErr, Info),
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

use crate::matcher::MatchErr;
impl From<MatchErr> for TError {
    fn from(error: MatchErr) -> Self {
        use TError::MatchError;
        MatchError(error, Info::default())
    }
}

#[derive(Error, PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Derivative)]
#[derivative(Debug)]
pub enum RequirementError {
    #[error("\n  Found {0},\n  Expected None")]
    ExpectedNoComponent(String),
    #[error("\n  Found None,\n  Expected Some(_)")]
    ExpectedAnyComponent,
    #[error("\n  Found {1},\n  Expected {0}")]
    ExpectedComponent(String, String),
    #[error("\n  Found no component,\n  Expected {0}")]
    ExpectedComponentFoundNone(String),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub struct RequirementErrors {
    pub errs: Vec<RequirementError>,
}

impl std::fmt::Display for RequirementErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut first = true;
        for v in &self.errs {
            if !first {
                write!(f, ", ")?;
            } else {
                first = false;
            }
            write!(f, "{}", v)?;
        }
        Ok(())
    }
}
