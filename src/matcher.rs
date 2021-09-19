use crate::database::{DBStorage, Requirement};
use crate::errors::{RequirementError, RequirementErrors};
use derivative::Derivative;
use specs::Entity;
use thiserror::Error;

#[derive(Error, PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Derivative)]
#[derivative(Debug)]
pub enum MatchErrReason {
    #[error("Expected one match, but found none")]
    ExpectedOneFoundNone,
    #[error("Expected one match, but found more than one: {0:?}")]
    ExpectedOneFoundMany(Vec<Entity>),
    #[error("Expected no matches, but found some: {0:?}")]
    ExpectedNoneFoundSome(Vec<Entity>),
}

impl MatchErrReason {
    fn because(self, errs: Vec<RequirementError>) -> MatchErr {
        Fail(self, RequirementErrors { errs })
    }
}

#[derive(Error, PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Derivative)]
#[derivative(Debug)]
pub enum MatchErr {
    #[error("{0}\n  Requirement not met: {1}")]
    Fail(MatchErrReason, RequirementErrors),
    #[error("Error in left: {0}")]
    PairErrorInLeft(Box<MatchErr>),
    #[error("Error in right: {0}")]
    PairErrorInRight(Box<MatchErr>),
    #[error("Error in initial search: {0}")]
    ChainErrorInInitial(Box<MatchErr>),
    #[error("Error in follow up search: {0}")]
    ChainErrorInFollowUp(Box<MatchErr>),
    #[error("No entities found to match expectations: {0}")]
    ExpectErrorInInitial(Box<MatchErr>),
    #[error("Entities did not match expectations: {0}")]
    ExpectErrorInFollowUp(Box<MatchErr>),
    #[error("Expectation not met: {0:?} vs {1:?}")]
    ExpectationNotMetVec(Vec<Entity>, Vec<Entity>),
    #[error("Expectations not met: {0:?} vs {1:?}")]
    ExpectationNotMet(Entity, Entity),
}

use MatchErr::*;
use MatchErrReason::*;

pub trait Matcher<Res = Vec<Entity>> {
    fn run(&self, storage: &DBStorage) -> Result<Res, MatchErr>;
}

impl<T> dyn Matcher<T> {
    pub fn chain<'a, U>(self, other: impl Fn(&T) -> Box<dyn Matcher<U>> + 'a) -> Chain<'a, T, U>
    where
        Self: Sized,
    {
        Chain {
            first: Box::new(self),
            second: Box::new(other),
        }
    }

    pub fn pair<'a, U>(self, other: impl Matcher<U> + 'a) -> Pair<'a, T, U>
    where
        Self: Sized,
    {
        Pair {
            first: Box::new(self),
            second: Box::new(other),
        }
    }
}

impl<T> dyn Matcher<T>
where
    T: Eq,
{
    pub fn expect<'a>(&'a self, other: impl Matcher<T> + 'a) -> Expect<'a, T> {
        Expect {
            first: self,
            second: Box::new(other),
        }
    }
}

impl Matcher<Vec<Entity>> for Requirement {
    fn run(&self, storage: &DBStorage) -> Result<Vec<Entity>, MatchErr> {
        let (res, _errs) = storage.matches(self);
        Ok(res)
    }
}

impl Matcher<Entity> for Requirement {
    fn run(&self, storage: &DBStorage) -> Result<Entity, MatchErr> {
        let (res, errs) = storage.matches(self);
        if res.is_empty() {
            return Err(ExpectedOneFoundNone.because(errs));
        }
        if res.len() > 1 {
            return Err(ExpectedOneFoundMany(res).because(errs));
        }
        Ok(res[0])
    }
}

pub struct NoMatches;

impl Matcher<NoMatches> for Requirement {
    fn run(&self, storage: &DBStorage) -> Result<NoMatches, MatchErr> {
        let (res, errs) = storage.matches(self);
        if !res.is_empty() {
            return Err(ExpectedNoneFoundSome(res).because(errs));
        }
        Ok(NoMatches)
    }
}

pub struct Pair<'a, T, U> {
    first: Box<dyn Matcher<T>>,
    second: Box<dyn Matcher<U> + 'a>,
}

impl<'a, T, U> Matcher<(T, U)> for Pair<'a, T, U> {
    fn run(&self, storage: &DBStorage) -> Result<(T, U), MatchErr> {
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

pub struct Chain<'a, T, U> {
    first: Box<dyn Matcher<T>>,
    second: Box<dyn Fn(&T) -> Box<dyn Matcher<U>> + 'a>,
}

impl<'a, T, U> Matcher<(T, U)> for Chain<'a, T, U> {
    fn run(&self, storage: &DBStorage) -> Result<(T, U), MatchErr> {
        let left = self
            .first
            .run(storage)
            .map_err(|err| ChainErrorInInitial(Box::new(err)))?;
        let right = (self.second)(&left)
            .run(storage)
            .map_err(|err| ChainErrorInFollowUp(Box::new(err)))?;
        Ok((left, right))
    }
}

pub struct Expect<'a, T: Eq> {
    first: &'a dyn Matcher<T>,
    second: Box<dyn Matcher<T> + 'a>,
}

impl<'a> Matcher<Vec<Entity>> for Expect<'a, Vec<Entity>> {
    fn run(&self, storage: &DBStorage) -> Result<Vec<Entity>, MatchErr> {
        let left = self
            .first
            .run(storage)
            .map_err(|err| ExpectErrorInInitial(Box::new(err)))?;
        let right = self
            .second
            .run(storage)
            .map_err(|err| ExpectErrorInFollowUp(Box::new(err)))?;
        if left != right {
            return Err(ExpectationNotMetVec(left, right));
        }
        Ok(left)
    }
}

impl<'a> Matcher<Entity> for Expect<'a, Entity> {
    fn run(&self, storage: &DBStorage) -> Result<Entity, MatchErr> {
        let left = self
            .first
            .run(storage)
            .map_err(|err| ExpectErrorInInitial(Box::new(err)))?;
        let right = self
            .second
            .run(storage)
            .map_err(|err| ExpectErrorInFollowUp(Box::new(err)))?;
        if left != right {
            return Err(ExpectationNotMet(left, right));
        }
        Ok(left)
    }
}
