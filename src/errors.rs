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
    UnknownToken(Token, Info, String),
    #[error("unknown symbol `{0}` in {2} at {1}")]
    UnknownSymbol(String, Info, String),
    #[error("out of scope type variable `{0}` at {1}")]
    OutOfScopeTypeVariable(String, Info),
    #[error("parse failed, {0} at {1}")]
    ParseError(String, Info),
    #[error("internal error: {0} at {1}")]
    InternalError(String, Info),
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
