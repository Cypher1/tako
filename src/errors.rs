use super::ast::Info;
use super::ast::Prim;

#[derive(Debug, PartialEq, Clone)]
pub enum TError {
    // Undefined symbols
    UnknownSymbol(String, Info),
    UnknownInfixOperator(String, Info),
    UnknownPrefixOperator(String, Info),

    // Type errors
    TypeMismatch(String, Box<Prim>, Info), // Static type failure (impossible type)
    TypeMismatch2(String, Box<Prim>, Box<Prim>, Info), // Static type failure (mismatched type)
    RequirementFailure(Info), // Runtime requirement failure

    FailedParse(String, Info),
}
