use crate::keywords::KEYWORDS;
use string_interner::{Symbol, StringInterner, backend::BufferBackend};
use crate::free_standing::typed_index::TypedIndex;
pub type StrInterner = StringInterner<BufferBackend<StrId>>;

pub fn get_new_interner() -> StrInterner {
    <StrInterner>::from_iter(KEYWORDS)
}

#[derive(Copy, Clone)]
pub struct Str {} // Place holder for &str with no lifetimes...

pub type StrId = TypedIndex<Str>;

impl Symbol for StrId {
    fn try_from_usize(index: usize) -> Option<Self> {
        Some(Self::new(index))
    }

    fn to_usize(self) -> usize {
        self.index
    }
}
