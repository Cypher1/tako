use super::ast::Info;
use super::ast::Prim;

#[derive(Debug, PartialEq, Clone)]
pub enum TError {
    FailedSymbolLookup(String, Info),

    // Parse errors
    UnknownInfixOperator(String, Info),
    UnknownPrefixOperator(String, Info),
    UnknownSymbol(String, Info),
    FailedParse(String, Info),
    TypeMismatch(String, Box<Prim>, Info),
    TypeMismatch2(String, Box<Prim>, Box<Prim>, Info),
    RequirementFailure(Info),
}
