use crate::components::InstancesAt;
use crate::database::DBStorage;
use crate::errors::{RequirementError, RequirementErrors};
use crate::location::Loc;
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
    #[error("Expectation not met: {0:?} vs {1:?}")]
    ExpectationNotMetVec(Vec<Entity>, Vec<Entity>),
    #[error("Expectations not met: {0:?} vs {1:?}")]
    ExpectationNotMet(Entity, Entity),
}

pub type Log = Vec<RequirementError>;

impl MatchErrReason {
    fn because(self, errs: Log) -> MatchErr {
        Fail(self, RequirementErrors { errs })
    }
}

#[derive(Error, PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Derivative)]
#[derivative(Debug)]
pub enum MatchErr {
    #[error("{0}\n  Requirement not met: {1}")]
    Fail(MatchErrReason, RequirementErrors),
    #[error("Error in left: {0}")]
    TupleErrorInLeft(Box<MatchErr>),
    #[error("Error in right: {0}")]
    TupleErrorInRight(Box<MatchErr>),
    #[error("Error in initial search: {0}")]
    ChainErrorInInitial(Box<MatchErr>),
    #[error("Error in follow up search: {0}")]
    ChainErrorInFollowUp(Box<MatchErr>),
    #[error("No entities found to match expectations: {0}")]
    ExpectErrorInInitial(Box<MatchErr>),
    #[error("Entities did not match expectations: {0}")]
    ExpectErrorInFollowUp(Box<MatchErr>),
}

use MatchErr::*;
use MatchErrReason::*;

pub trait Matcher {
    type Res;
    fn run(&self, storage: &DBStorage) -> Result<Self::Res, MatchErr> {
        let (res, _errs) = self.run_with_errs(storage)?;
        Ok(res)
    }

    fn run_with_errs(&self, storage: &DBStorage) -> Result<(Self::Res, Log), MatchErr>;

    fn one(self) -> One<Self>
    where
        Self: Sized + Matcher<Res = Vec<Entity>>,
    {
        One(self)
    }

    fn none(self) -> NoMatches<Self>
    where
        Self: Sized + Matcher<Res = Vec<Entity>>,
    {
        NoMatches(self)
    }

    fn chain<U: Matcher>(self, other: impl Fn(&Self::Res) -> U + 'static) -> Chain<Self, U>
    where
        Self: Sized,
    {
        Chain {
            first: self,
            second: Box::new(other),
        }
    }

    fn expect<U: Matcher<Res = Vec<Entity>>>(self, other: U) -> Expect<Self, U>
    where
        Self: Sized + Matcher<Res = Vec<Entity>>,
    {
        Expect {
            first: self,
            second: other,
        }
    }

    fn at(self, file_name: &str, line: i32, col: i32) -> Expect<Self, InstancesAt>
    where
        Self: Sized + Matcher<Res = Vec<Entity>>,
    {
        self.expect(InstancesAt(set!(Loc::new(file_name, line, col))))
    }
}

pub struct One<T: Matcher<Res = Vec<Entity>>>(T);

impl<T: Matcher<Res = Vec<Entity>>> Matcher for One<T> {
    type Res = Entity;

    fn run_with_errs(&self, storage: &DBStorage) -> Result<(Entity, Log), MatchErr> {
        let (res, errs) = self.0.run_with_errs(storage)?;
        if res.is_empty() {
            return Err(ExpectedOneFoundNone.because(errs));
        }
        if res.len() > 1 {
            return Err(ExpectedOneFoundMany(res).because(errs));
        }
        Ok((res[0], errs))
    }
}

pub struct NoMatches<T: Matcher<Res = Vec<Entity>>>(T);

impl<T: Matcher<Res = Vec<Entity>>> Matcher for NoMatches<T> {
    type Res = ();
    fn run_with_errs(&self, storage: &DBStorage) -> Result<(Self::Res, Log), MatchErr> {
        let (res, errs) = self.0.run_with_errs(storage)?;
        if !res.is_empty() {
            return Err(ExpectedNoneFoundSome(res).because(errs));
        }
        Ok(((), errs))
    }
}

impl<T: Matcher, U: Matcher> Matcher for (T, U) {
    type Res = (T::Res, U::Res);
    fn run_with_errs(&self, storage: &DBStorage) -> Result<(Self::Res, Log), MatchErr> {
        let (f, f_errs) = self
            .0
            .run_with_errs(storage)
            .map_err(|err| TupleErrorInLeft(Box::new(err)))?;
        let (s, s_errs) = self
            .1
            .run_with_errs(storage)
            .map_err(|err| TupleErrorInRight(Box::new(err)))?;
        let mut errs = f_errs;
        errs.extend(s_errs);
        Ok(((f, s), errs))
    }
}

pub struct Chain<T: Matcher, U: Matcher> {
    first: T,
    second: Box<dyn Fn(&T::Res) -> U>,
}

impl<T: Matcher, U: Matcher> Matcher for Chain<T, U> {
    type Res = (T::Res, U::Res);
    fn run_with_errs(&self, storage: &DBStorage) -> Result<(Self::Res, Log), MatchErr> {
        let (left, l_errs) = self
            .first
            .run_with_errs(storage)
            .map_err(|err| ChainErrorInInitial(Box::new(err)))?;
        let (right, r_errs) = (self.second)(&left)
            .run_with_errs(storage)
            .map_err(|err| ChainErrorInFollowUp(Box::new(err)))?;
        let mut errs = l_errs;
        errs.extend(r_errs);
        Ok(((left, right), errs))
    }
}

pub struct Expect<T: Matcher, U: Matcher> {
    first: T,
    second: U,
}

impl<T: Matcher<Res = Vec<Entity>>, U: Matcher<Res = Vec<Entity>>> Matcher for Expect<T, U> {
    type Res = T::Res;
    fn run_with_errs(&self, storage: &DBStorage) -> Result<(Self::Res, Log), MatchErr> {
        let (left, l_errs) = self
            .first
            .run_with_errs(storage)
            .map_err(|err| ExpectErrorInInitial(Box::new(err)))?;
        let (right, r_errs) = self
            .second
            .run_with_errs(storage)
            .map_err(|err| ExpectErrorInFollowUp(Box::new(err)))?;
        let mut errs = l_errs;
        errs.extend(r_errs);
        let mut failed = false;
        for l in left.iter() {
            if !right.contains(l) {
                // TODO: This is O(n), could be O(1)
                failed = true;
                break;
            }
        }
        if failed {
            return Err(ExpectationNotMetVec(left, right).because(errs));
        }
        Ok((left, errs))
    }
}
