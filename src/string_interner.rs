use crate::keywords::KEYWORDS;
use string_interner::{Symbol, StringInterner, backend::BufferBackend};
use crate::free_standing::typed_index::TypedIndex;
pub type Interner = StringInterner<BufferBackend<StrId>>;

pub fn get_new_interner() -> Interner {
    <StringInterner>::from_iter(KEYWORDS)
}

pub type StrId = TypedIndex<str>;

impl Symbol for StrId {
    fn try_from_usize(index: usize) -> Option<Self> {
        Some(Self::new(index))
    }

    fn to_usize(self) -> usize {
        self.index
    }
}
