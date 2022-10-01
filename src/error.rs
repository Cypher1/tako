use crate::concepts::*;
use crate::location::{Location, UserFacingLocation};
use thiserror::Error;

#[derive(Error, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
        location: Option<Location>,
    },
}

impl std::fmt::Display for TError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Self as std::fmt::Debug>::fmt(self, f)
    }
}

impl From<std::fmt::Error> for TError {
    fn from(error: std::fmt::Error) -> Self {
        TError::InternalError {
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
pub struct UserFacingError {
    error: TError,
    location: Option<UserFacingLocation>,
}

impl UserFacingError {
    fn new(error: TError, file: &File) -> Self {
        use TError::*;
        let location = match &error {
            CppCompilerError { .. } => None,
            ParseError { location, .. } => location.as_ref(),
            InternalError { location, .. } => location.as_ref(),
        };
        let location = location.map(|location| UserFacingLocation::from(file, location));
        Self { error, location }
    }
}

impl std::fmt::Display for UserFacingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Self as std::fmt::Debug>::fmt(self, f)
    }
}

impl std::fmt::Debug for UserFacingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TError::*;
        write!(f, "Error")?;
        if let Some(location) = &self.location {
            write!(f, " in {}", &location)?;
        }
        match &self.error {
            CppCompilerError { error, return_code } => write!(
                f,
                "call to C++ compiler failed with error code: {return_code}\n{error}"
            ),
            ParseError {
                message,
                location: _,
            } => write!(f, "parse failed, {message}"),
            InternalError {
                message,
                location: _,
            } => write!(f, "internal error: {message}"),
        }
    }
}
