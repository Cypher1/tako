use crate::database::{DBStorage, Requirement};
use specs::Entity;

use derivative::Derivative;
use thiserror::Error;
#[derive(Error, PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Derivative)]
#[derivative(Debug)]
enum MatchErr {
    #[error("Expected one match, but found none")]
    ExpectedOneFoundNone,
    #[error("Expected one match, but found more than one: {0:?}")]
    ExpectedOneFoundMany(Vec<Entity>),
    #[error("Expected no matches, but found some: {0:?}")]
    ExpectedNoneFoundSome(Vec<Entity>),
    #[error("Error during chaining, error in initial search: {0}")]
    ChainErrorInInitial(Box<MatchErr>),
    #[error("Error during chaining, error in follow up search: {0}")]
    ChainErrorInFollowUp(Box<MatchErr>),
    #[error("Error during pairing, error in left: {0}")]
    PairErrorInLeft(Box<MatchErr>),
    #[error("Error during pairing, error in right: {0}")]
    PairErrorInRight(Box<MatchErr>),
}

use MatchErr::*;

trait Matcher<Res = Vec<Entity>> {
    fn run(self: &Self, storage: &DBStorage) -> Result<Res, MatchErr>;
}

impl<T> dyn Matcher<T> {
    fn chain<'a, U>(self: Self, other: impl Fn(T) -> Box<dyn Matcher<U>> + 'a) -> Chain<'a, T, U> where Self: Sized {
        Chain {
            first: Box::new(self),
            second: Box::new(other),
        }
    }

    fn pair<'a, U>(self: Self, other: impl Matcher<U> + 'a) -> Pair<'a, T, U> where Self: Sized {
        Pair {
            first: Box::new(self),
            second: Box::new(other),
        }
    }
}

impl Matcher<Vec<Entity>> for Requirement {
    fn run(self: &Self, storage: &DBStorage) -> Result<Vec<Entity>, MatchErr> {
        Ok(storage.matches(self))
    }
}

impl Matcher<Entity> for Requirement {
    fn run(self: &Self, storage: &DBStorage) -> Result<Entity, MatchErr> {
        let res = storage.matches(self);
        if res.is_empty() {
            return Err(ExpectedOneFoundNone);
        }
        if res.len() > 1 {
            return Err(ExpectedOneFoundMany(res));
        }
        Ok(res[0])
    }
}

impl Matcher<()> for Requirement {
    fn run(self: &Self, storage: &DBStorage) -> Result<(), MatchErr> {
        let res = storage.matches(self);
        if !res.is_empty() {
            return Err(ExpectedNoneFoundSome(res));
        }
        Ok(())
    }
}

struct Chain<'a, T, U> {
    first: Box<dyn Matcher<T>>,
    second: Box<dyn Fn(T) -> Box<dyn Matcher<U>> + 'a>,
}

impl<'a, T, U> Matcher<U> for Chain<'a, T, U> {
    fn run(self: &Self, storage: &DBStorage) -> Result<U, MatchErr> {
        let res = self
            .first
            .run(storage)
            .map_err(|err| ChainErrorInInitial(Box::new(err)))?;
        (self.second)(res)
            .run(storage)
            .map_err(|err| ChainErrorInFollowUp(Box::new(err)))
    }
}

struct Pair<'a, T, U> {
    first: Box<dyn Matcher<T>>,
    second: Box<dyn Matcher<U> + 'a>,
}

impl<'a, T, U> Matcher<(T, U)> for Pair<'a, T, U> {
    fn run(self: &Self, storage: &DBStorage) -> Result<(T, U), MatchErr> {
        Ok((
            self.first
                .run(storage)
                .map_err(|err| PairErrorInLeft(Box::new(err)))?,
            self.second
                .run(storage)
                .map_err(|err| PairErrorInRight(Box::new(err)))?,
        ))
    }
}
