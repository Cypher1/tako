use crate::utils::typed_index::TypedIndex;
use crate::location::{Location, UserFacingLocation};
use thiserror::Error;

#[derive(Error, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

#[derive(Error, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Error {
    pub source: TError,
    location: Option<UserFacingLocation>,
}
pub type ErrorId = TypedIndex<Error>;

impl Error {
    // TODO: Use a builder for this.
    pub fn new(
        source: TError,
        path: Option<&str>,
        contents: Option<&str>,
        module: Option<()>,
    ) -> Self {
        let location = match &source {
            TError::CppCompilerError { .. } => None,
            TError::ParseError { location, .. } => location.as_ref(),
            TError::InternalError { location, .. } => location.as_ref(),
        };
        let location = match (path, contents, location, module) {
            (Some(path), Some(contents), Some(location), _module) => {
                Some(UserFacingLocation::from(path, contents, location))
            }
            _ => None, // TODO: There's more options here...
        };
        Self { source, location }
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Self as std::fmt::Debug>::fmt(self, f)
    }
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Error")?;
        if let Some(location) = &self.location {
            write!(f, " in {}", &location)?;
        }
        match &self.source {
            TError::CppCompilerError { error, return_code } => write!(
                f,
                "call to C++ compiler failed with error code: {return_code}\n{error}"
            ),
            TError::ParseError {
                message,
                location: _,
            } => write!(f, "parse failed, {message}"),
            TError::InternalError {
                message,
                location: _,
            } => write!(f, "internal error: {message}"),
        }
    }
}
