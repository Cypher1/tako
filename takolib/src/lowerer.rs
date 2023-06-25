use crate::ast::Ast;
use crate::ast::NodeId;
use crate::error::TError;
use std::path::Path;

pub fn lower(_path: &Path, ast: &Ast, _root: Option<NodeId>) -> Result<Ast, TError> {
    // TODO: ???
    Ok(ast.clone())
}
