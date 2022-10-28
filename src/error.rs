use crate::primitives::Val;
use crate::location::Location;
use thiserror::Error;
use crate::internal_error::TError;

#[derive(Error, PartialEq, Eq, PartialOrd, Ord)]
pub struct UserFacingError<'a> {
    error: Terror,
    file: &'a File,
    entry: &'a EntryPoint,
    location: &'a Location,
    user_facing_location: UserFacingError,
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

impl std::fmt::Display for UserFacingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Self as std::fmt::Debug>::write(self, f)
    }
}

impl std::fmt::Debug for UserFacingError {
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
