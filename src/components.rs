use specs::prelude::*;
use specs::Component;

use crate::ast::Path;
use crate::errors::TError;
use crate::primitives::Val;
use crate::tokens::TokenType;

#[derive(Component, Clone, Debug)]
#[storage(VecStorage)]
pub struct AtPath(pub Path);

#[derive(Component, Clone, Debug)]
#[storage(VecStorage)]
pub struct Token {
    pub token: TokenType,
    pub value: String,
}

#[derive(Component, Clone, Debug)]
#[storage(VecStorage)]
pub struct HasValue(pub Val);

#[derive(Component, Clone, Debug)]
#[storage(VecStorage)]
pub struct HasSymbol(pub String);

#[derive(Component, Default, Debug)]
#[storage(NullStorage)]
pub struct Untyped;

#[derive(Component, Clone, Debug)]
#[storage(VecStorage)]
pub struct Typed(pub Val);

#[derive(Component, Clone, Debug)]
#[storage(VecStorage)]
pub struct HasErrors(pub Vec<TError>);

#[derive(Component, Clone, Debug)]
#[storage(VecStorage)]
pub struct HasInner(pub Entity);

#[derive(Component, Clone, Debug)]
#[storage(VecStorage)]
pub struct HasChildren(pub Vec<Entity>); // TODO: Short vec

#[derive(Component, Clone, Debug)]
#[storage(VecStorage)]
pub struct HasArguments(pub Option<Vec<Entity>>); // TODO: Short vec

#[derive(Component, Clone, Default, Debug)]
#[storage(NullStorage)]
pub struct IsSymbol;

#[derive(Component, Clone, Default, Debug)]
#[storage(NullStorage)]
pub struct IsAst;

#[derive(Component, Clone, Default, Debug)]
#[storage(NullStorage)]
pub struct IsDefinition;
