use crate::ast::{Path, PathRef};
use crate::errors::TError;
use crate::location::Loc;
use crate::primitives::Val;
use crate::DBStorage;
use specs::Entity;

#[derive(Debug, PartialEq, Eq, Clone, Hash, PartialOrd, Ord)]
pub struct DefinitionHead {
    pub name: Path,
    pub params: Option<Vec<Entity>>, // TODO: Restrict to valid def-args (variables and functions with optional default values)
    pub path: Path,
}

impl DefinitionHead {
    pub fn into_call(
        self,
        storage: &mut DBStorage,
        path: PathRef,
        loc: &Loc,
        ty: Option<Entity>,
    ) -> AstNode {
        let name = AstTerm::Symbol {
            name: self.name,
            context: self.path,
            value: None,
        }
        .into_node(loc, None);
        self.params.map_or(name.clone(), |args| {
            let inner = storage.store_node(name, path);
            AstTerm::Call { inner, args }.into_node(loc, ty)
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, PartialOrd, Ord)]
pub enum AstTerm {
    Value(Val),
    Symbol {
        name: Path,
        context: Path,
        value: Option<Val>,
    },
    Sequence(Vec<Entity>), // TODO: Inline the vec somehow? Use a non empty vec?
    Call {
        inner: Entity,
        args: Vec<Entity>,
    },
    Definition {
        head: DefinitionHead,
        implementations: Vec<Entity>,
    },
    DefinitionHead(DefinitionHead),
}

impl AstTerm {
    #[must_use]
    pub fn into_node(self, loc: &Loc, ty: Option<Entity>) -> AstNode {
        AstNode {
            term: self,
            loc: loc.clone(),
            ty,
        }
    }

    pub fn into_definition(
        self,
        _storage: &mut DBStorage,
        right: Entity,
        loc: &Loc,
    ) -> Result<AstNode, TError> {
        Ok(match self {
            AstTerm::Symbol {
                name,
                context,
                value: None,
            } => AstTerm::Definition {
                head: DefinitionHead {
                    name,
                    params: None,
                    path: context,
                },
                implementations: vec![right],
            },
            AstTerm::DefinitionHead(head) => AstTerm::Definition {
                head,
                implementations: vec![right],
            },
            _ => {
                return Err(TError::ParseError(
                    format!("Cannot assign to {:?}", self),
                    loc.clone().get_info(),
                ));
            }
        }
        .into_node(loc, None))
    }
}

#[derive(Debug, Clone)]
pub struct AstNode {
    pub term: AstTerm,
    pub loc: Loc,
    pub ty: Option<Entity>,
}

impl AstNode {
    pub fn into_definition(
        self,
        storage: &mut DBStorage,
        right: Entity,
        loc: &Loc,
    ) -> Result<AstNode, TError> {
        Ok(AstNode {
            ty: self.ty,
            ..self.term.into_definition(storage, right, loc)?
        })
    }
}
