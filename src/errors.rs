use super::ast::Info;
use super::ast::Node;
use super::ast::Prim;

use std::{fmt, io};
use thiserror::Error;

//impl<T: fmt::Debug> fmt::Debug for Tree<T> {
impl fmt::Display for Box<Prim> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", *self)
    }
}

use derivative::Derivative;
#[derive(Error, Derivative)]
#[derivative(Debug, PartialEq)]
pub enum TError {
    #[error("failed while performing io")]
    IOFailure(
        #[derivative(PartialEq = "ignore")]
        #[from]
        io::Error,
    ),

    #[error("unknown symbol `{0}` at {1:?}")]
    UnknownSymbol(String, Info),
    #[error("unknown infix operator `{0}` at {1:?}")]
    UnknownInfixOperator(String, Info),
    #[error("unknown prefix operator `{0}` at {1:?}")]
    UnknownPrefixOperator(String, Info),

    #[error("impossible type, {0} at {1}")]
    TypeMismatch(String, Box<Prim>, Info),
    #[error("type mismatch, {0} vs {1} at {2}")]
    TypeMismatch2(String, Box<Prim>, Box<Prim>, Info),
    #[error("runtime requirement failed at {0:?}")]
    RequirementFailure(Info),

    #[error("parse failed, {0} at {1:?}")]
    FailedParse(String, Info),
    #[error("internal error `{0}` at {1}")]
    InternalError(String, Node),
}
