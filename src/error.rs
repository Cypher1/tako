use crate::free_standing::typed_index::TypedIndex;
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
        use TError::*;
        let location = match &source {
            FileNotLoadedError => None,
            FileNotLexedError => None,
            CppCompilerError { .. } => None,
            ParseError { location, .. } => location.as_ref(),
            InternalError { location, .. } => location.as_ref(),
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
        use TError::*;
        write!(f, "Error")?;
        if let Some(location) = &self.location {
            write!(f, " in {}", &location)?;
        }
        match &self.source {
            FileNotLoadedError => write!(f, "File was not loaded before parse time"),
            FileNotLexedError => write!(f, "File was not lexed before parse time"),
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
