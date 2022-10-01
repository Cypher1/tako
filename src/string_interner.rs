use crate::keywords::KEYWORDS;
use crate::free_standing::typed_index::TypedIndex;
use string_interner::{Symbol, StringInterner, backend::BufferBackend};
use std::iter::FromIterator;
use std::convert::TryInto;

pub type StrInterner = StringInterner<BufferBackend<StrId>>;

pub fn get_new_interner() -> StrInterner {
    <StrInterner>::from_iter(KEYWORDS)
}

#[derive(Copy, Clone)]
pub struct Str {} // Place holder for &str with no lifetimes...

pub type StrId = TypedIndex<Str>;

impl Symbol for StrId {
    fn try_from_usize(index: usize) -> Option<Self> {
        index.try_into().map(Self::new).ok()
    }

    fn to_usize(self) -> usize {
        self.raw_index() as usize
    }
}
