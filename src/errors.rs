use crate::primitives::Val;
use specs::Entity;
use crate::location::Loc;
use thiserror::Error;
use derivative::Derivative;

#[derive(Error, PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Derivative)]
#[derivative(Debug)]
pub enum TError {
    #[error("call to C++ compiler failed with error code: {return_code}\n{error}")]
    CppCompilerError {
        error: String,
        return_code: Option<i32>
    },
    #[error("parse failed, {msg} at {loc}")]
    ParseError {
        msg: String,
        loc: Loc
    },
    #[error("internal error: {msg} at {loc}")]
    InternalError {
        msg: String,
        loc: Loc
    },
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
