use crate::keywords::KEYWORDS;
use string_interner::{StringInterner, backend::BufferBackend};
pub type Interner = StringInterner<BufferBackend>;
// TODO: Consider other character widths
// See: https://docs.rs/string-interner/latest/string_interner/

pub fn get_new_interner() -> Interner {
    <StringInterner>::from_iter(KEYWORDS)
}
