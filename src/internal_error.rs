use crate::primitives::Val;
use crate::location::Location;
use thiserror::Error;

#[derive(Error, PartialEq, Eq, PartialOrd, Ord)]
pub enum TError {
    #[error("call to C++ compiler failed with error code: {return_code}\n{error}")]
    CppCompilerError {
        error: String,
        return_code: i32,
    },
    #[error("parse failed, {message} at {location}")]
    ParseError {
        message: String,
        location: Location
    },
    #[error("internal error: {message} at {location}")]
    InternalError {
        message: String,
        location: Location
    },
}

impl From<std::fmt::Error> for TError {
    fn from(error: std::fmt::Error) -> Self {
        TError::InternalError(error.to_string())
    }
}

impl From<std::io::Error> for TError {
    fn from(error: std::io::Error) -> Self {
        TError::InternalError(error.to_string())
    }
}

impl From<std::num::ParseIntError> for TError {
    fn from(error: std::num::ParseIntError) -> Self {
        TError::ParseError(error.to_string())
    }
}
