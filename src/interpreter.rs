use std::path::PathBuf;

use crate::ast::*;
use crate::error::TError;
use crate::primitives::Prim;

pub fn run(_path: &PathBuf, _ast: &Ast) -> Result<Prim, TError> {
    // TODO: ???
    Ok(Prim::I32(0))
}
