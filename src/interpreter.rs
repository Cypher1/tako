use std::path::Path;

use crate::ast::*;
use crate::error::TError;
use crate::primitives::Prim;

pub fn run(_path: &Path, _ast: &Ast) -> Result<Prim, TError> {
    // TODO: ???
    Ok(Prim::I32(0))
}
