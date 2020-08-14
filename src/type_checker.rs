use crate::ast::Node;
use crate::ast::{Info, Node::*, Prim::*, Sym, ToNode};
use crate::database::Compiler;
use crate::errors::TError;

pub fn infer(db: &dyn Compiler, expr: Node) -> Result<Node, TError> {
    // Infer that expression t has type A, t => A
    // See https://ncatlab.org/nlab/show/bidirectional+typechecking
    match expr {
        PrimNode(prim) => match prim {
            I32(_, _) => Ok(Sym {
                name: "I32".to_owned(),
                info: Info::default(),
            }
            .to_node()),
            Bool(_, _) => Ok(Sym {
                name: "Bool".to_owned(),
                info: Info::default(),
            }
            .to_node()),
            Str(_, _) => Ok(Sym {
                name: "String".to_owned(),
                info: Info::default(),
            }
            .to_node()),
            Lambda(node) => db.infer(*node), // TODO: abstraction
        },
        _ => panic!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Info;
    use crate::database::DB;
    use crate::parser::tests::parse_with_db;

    #[test]
    fn infer_type_of_i32() {
        let db = DB::default();
        let num = I32(23, Info::default()).to_node();
        assert_eq!(infer(&db, num), Ok(Sym{name: "I32".to_owned(), info: Info::default()}.to_node()));
    }

    // Disable for now #[test]
    fn infer_type_of_plus_expr() {
        let mut db = DB::default();
        let num = parse_with_db(&mut db, "12+32".to_string());
        assert_eq!(infer(&db, num), Ok(Sym{name: "I32".to_owned(), info: Info::default()}.to_node()));
    }
}