use std::path::PathBuf;

use crate::location::{Location, UserFacingLocation};
use crate::parser::ParseError;
use crate::utils::typed_index::TypedIndex;
use thiserror::Error;

#[derive(Error, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TError {
    ClangCompilerError {
        error: String,
        return_code: i32,
    },
    ParseError(ParseError),
    InternalError {
        message: String,
        location: Option<Location>,
    },
}

impl std::fmt::Display for TError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TError::*;
        match self {
            ClangCompilerError { error, return_code } => write!(
                f,
                "Clang failed with code {return_code} and error message: {error}"
            ),
            ParseError(e) => write!(f, "{}", e),
            InternalError { message, location } => {
                write!(f, "Internal error: {message}")?;
                if let Some(location) = location {
                    write!(f, "at {location:?}")?;
                }
                Ok(())
            }
        }
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
        if error.kind() == std::io::ErrorKind::NotFound {
            return TError::InternalError {
                message: "File not found".to_string(),
                location: None,
            };
        }
        TError::InternalError {
            message: error.to_string(),
            location: None,
        }
    }
}

// TODO: Remove
impl From<std::num::ParseIntError> for TError {
    fn from(error: std::num::ParseIntError) -> Self {
        TError::ParseError(ParseError::ParseIntError {
            message: error.to_string(),
        })
    }
}

#[derive(Error, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Error {
    pub source: TError,
    pub location: Option<UserFacingLocation>,
}
pub type ErrorId = TypedIndex<Error>;

impl Error {
    // TODO(clarity): Use a builder for this.
    pub fn new(
        source: TError,
        path: Option<&PathBuf>,
        contents: Option<&str>,
        module: Option<&()>,
    ) -> Self {
        let location = match &source {
            TError::ClangCompilerError { .. } => None,
            TError::ParseError(err) => err.location(),
            TError::InternalError { location, .. } => location.as_ref(),
        };
        let location = match (path, contents, location, module) {
            (Some(path), Some(contents), Some(location), _module) => {
                Some(UserFacingLocation::from(path, contents, location))
            }
            (Some(path), _, _, _) => Some(UserFacingLocation::from_path(path)),
            _ => None, // TODO(: There's more options here...
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
        match &self.source {
            TError::ClangCompilerError { error, return_code } => write!(
                f,
                "call to C++ compiler failed with error code: {return_code}\n{error}"
            )?,
            TError::ParseError(err) => write!(f, "{err}")?,
            TError::InternalError {
                message,
                location: _,
            } => write!(f, "{message}")?,
        }
        if let Some(location) = &self.location {
            write!(f, " {}", &location)?;
        }
        Ok(())
    }
}
