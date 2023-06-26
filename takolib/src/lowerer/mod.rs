use crate::ast::Ast;
use crate::ast::NodeId;
use crate::error::TError;
use std::path::Path;
use llamada_calculus::base_types::Empty;
use llamada_calculus::ext;
use llamada_calculus::{Expr, Llamada};

pub fn lower(_path: &Path, _ast: &Ast, _root: Option<NodeId>) -> Result<Llamada, TError> {
    // TODO: ???
    let /*mut*/ expr = Llamada::new(ext(24), Empty);
    Ok(expr)
}
