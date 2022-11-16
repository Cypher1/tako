use crate::ast::*;
use crate::primitives::Prim;
use crate::error::TError;

pub fn run(_path: &str, _ast: &Ast) -> Result<Prim, TError> {
    // TODO: ???
    Ok(Prim::I32(0))
}
