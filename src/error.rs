use crate::location::Location;
use thiserror::Error;

#[derive(Error, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum TError {
    CppCompilerError {
        error: String,
        return_code: i32,
    },
    ParseError {
        message: String,
        location: Location
    },
    InternalError {
        message: String,
        location: Location
    },
}

impl<'a> std::fmt::Display for TError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Self as std::fmt::Debug>::write(self, f)
    }
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

#[derive(Error, PartialEq, Eq, PartialOrd, Ord)]
pub struct UserFacingError<'a> {
    error: Terror,
    file: &'a File,
    entry: &'a EntryPoint,
    location: &'a Location,
    user_facing_location: UserFacingLocation,
}

impl<'a> UserFacingError<'a> {
    fn new(file: &'a File, entry: &'a EntryPoint, location: &'a Location) -> Self {
        let user_facing_location = UserFacingLocation::from(self.file, self.location);
        Self {
            file,
            entry,
            location,
            user_facing_location,
        }
    }
}

impl<'a> std::fmt::Display for UserFacingError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Self as std::fmt::Debug>::write(self, f)
    }
}

impl<'a> std::fmt::Debug for UserFacingError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
