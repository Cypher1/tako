use crate::ast::Path;
use crate::errors::TError;
use crate::location::Loc;
use crate::primitives::Val;
use specs::prelude::*;
use specs::Component;
use std::collections::BTreeSet;

#[derive(Component, Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[storage(VecStorage)]
pub struct DefinedAt(pub Option<Path>);

#[derive(Component, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[storage(VecStorage)]
pub struct InstancesAt(pub BTreeSet<Loc>);

impl std::fmt::Debug for InstancesAt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "InstancesAt(")?;
        let mut after = false;
        for item in self.0.iter() {
            write!(f, "{:?}", item)?;
            if after {
                write!(f, ", ")?;
            }
            after = true;
        }
        write!(f, ")")
    }
}

#[derive(Component, Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[storage(VecStorage)]
pub struct HasValue(pub Val);

#[derive(Component, Default, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[storage(NullStorage)]
pub struct Untyped;

#[derive(Component, Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[storage(VecStorage)]
pub struct HasType(pub Entity);

#[derive(Component, Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[storage(VecStorage)]
pub struct HasErrors(pub Vec<TError>);

#[derive(Component, Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[storage(VecStorage)]
pub struct Call(pub Entity, pub Vec<Entity>); // TODO: Short vec

#[derive(Component, Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[storage(VecStorage)]
pub struct Sequence(pub Vec<Entity>); // TODO: Short vec

#[derive(Component, Clone, Default, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[storage(VecStorage)]
pub struct SymbolRef {
    pub name: Path,
    pub context: Path,
}

#[derive(Component, Clone, Default, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[storage(NullStorage)]
pub struct IsAst;

#[derive(Component, Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[storage(VecStorage)]
pub struct Definition {
    pub names: Vec<Path>,
    pub params: Option<Vec<Entity>>,
    pub implementations: Vec<Entity>,
    pub path: Path,
}
