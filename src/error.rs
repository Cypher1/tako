use crate::location::{Location, UserFacingLocation};
use crate::concepts::*;
use thiserror::Error;

#[derive(Error, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum TError {
    CppCompilerError {
        error: String,
        return_code: i32,
    },
    ParseError {
        message: String,
        location: Option<Location>,
    },
    InternalError {
        message: String,
        location: Option<Location>
    },
}

impl<'a> std::fmt::Display for TError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Self as std::fmt::Debug>::fmt(self, f)
    }
}

impl From<std::fmt::Error> for TError {
    fn from(error: std::fmt::Error) -> Self {
        TError::InternalError{
            message: error.to_string(),
            location: None,
        }
    }
}

impl From<std::io::Error> for TError {
    fn from(error: std::io::Error) -> Self {
        TError::InternalError {
            message: error.to_string(),
            location: None,
        }
    }
}

impl From<std::num::ParseIntError> for TError {
    fn from(error: std::num::ParseIntError) -> Self {
        TError::ParseError {
            message: error.to_string(),
            location: None,
        }
    }
}

#[derive(Error, PartialEq, Eq, PartialOrd, Ord)]
pub struct UserFacingError<'a> {
    error: TError,
    location: UserFacingLocation,
}

impl<'a> UserFacingError<'a> {
    fn new(error: TError, file: &'a File, location: &'a Location) -> Self {
        let location = UserFacingLocation::from(error.file, error.location);
        Self {
            error,
            location,
        }
    }
}

impl<'a> std::fmt::Display for UserFacingError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Self as std::fmt::Debug>::fmt(self, f)
    }
}

impl<'a> std::fmt::Debug for UserFacingError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TError::*;
        write!(f, "Error in {}", &self.location)?;
        match self.error {
            CppCompilerError {
                error,
                return_code
            } => write!(f, "call to C++ compiler failed with error code: {return_code}\n{error}"),
            ParseError {
                msg,
                loc,
            } => write!(f, "parse failed, {msg} at {loc}"),
            InternalError {
                msg,
                loc,
            } => write!(f, "internal error: {msg} at {loc}"),
        }
    }
}
