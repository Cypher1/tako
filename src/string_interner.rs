use crate::utils::typed_index::TypedIndex;
use crate::keywords::KEYWORDS;
use std::convert::TryInto;
use std::iter::FromIterator;
use string_interner::{backend::BufferBackend, StringInterner, Symbol};

pub type StrInterner = StringInterner<BufferBackend<StrId>>;

pub fn get_new_interner() -> StrInterner {
    <StrInterner>::from_iter(KEYWORDS)
}

#[derive(Copy, Clone)]
pub struct Str {} // Place holder for &str with no lifetimes...

pub type StrId = TypedIndex<Str>;

impl Symbol for StrId {
    fn try_from_usize(index: usize) -> Option<Self> {
        index.try_into().map(Self::from_raw).ok()
    }

    fn to_usize(self) -> usize {
        self.raw_index() as usize
    }
}
