use super::location::IndexIntoFile;
use better_std::as_context;
use short_typed_index::TypedIndex;
use std::collections::BTreeMap;
use std::hash::{Hash, Hasher};
use std::sync::Arc;

type StringHash = u64;
// Ensures that str ids are unique per string but also stable across different files etc.
pub type StrId = TypedIndex<String, StringHash>;
pub type Identifier = StrId;

use static_assertions::assert_eq_size;
assert_eq_size!(Identifier, [u8; 8]);
assert_eq_size!([Identifier; 2], [u8; 16]);
// This means we can store two identifier references in the AST in the same
// memory as a &str.
#[cfg(not(target_family = "wasm"))]
assert_eq_size!([Identifier; 2], &str);
// And 3 in the space of a String.
#[cfg(not(target_family = "wasm"))]
assert_eq_size!([Identifier; 3], String);

#[derive(Clone, Default, Debug, Hash, PartialEq, Eq)]
pub struct StringInterner {
    // This ensures we can look up the string from the hash.
    // BT: We can also merge the hashes without losing any information.
    // TODO(perf): Look at alternatives to BTreeMap.
    // Consider indexes into
    // https://docs.rs/arrayvec/0.7.6/src/arrayvec/array_string.rs.html
    pub loc2string: Arc<BTreeMap<IndexIntoFile, StrId>>,
    pub strings: Arc<BTreeMap<StrId, String>>,
}

as_context!(StrId, StringInterner, InternedString);

impl std::fmt::Display for InternedString<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(s) = self.context().get_str(**self) {
            write!(f, "{s}")
        } else {
            write!(f, "<unknown symbol: {self:?}>")
        }
    }
}

impl StringInterner {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    #[must_use]
    pub fn register_str_by_loc(&mut self, name: &str, ind: IndexIntoFile) -> StrId {
        let id = self.register_str(name);
        Arc::make_mut(&mut self.loc2string).insert(ind, id);
        id
    }
    pub fn register_str(&mut self, name: &str) -> StrId {
        let mut hasher = fxhash::FxHasher::default();
        name.hash(&mut hasher);
        let str_hash = hasher.finish();
        let id = TypedIndex::from_raw(str_hash);
        if !self.strings.contains_key(&id) {
            Arc::make_mut(&mut self.strings).insert(id, name.to_string());
        }
        id
    }
    #[must_use]
    pub fn get_str(&self, s: StrId) -> Option<&str> {
        self.strings.get(&s).map(|ref_string| &**ref_string)
    }
    #[must_use]
    pub fn get_str_by_loc(&self, s: IndexIntoFile) -> Option<&str> {
        let s = self.loc2string.get(&s)?;
        self.get_str(*s)
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    fn setup() -> StringInterner {
        crate::ensure_initialized();
        StringInterner::new()
    }

    #[test]
    fn get_whole_str_round_trip() {
        let mut interner = setup();
        let id = interner.register_str_by_loc("123", 0);
        assert_eq!(interner.get_str(id), Some("123"));
        assert_eq!(interner.get_str_by_loc(0), Some("123"));
    }

    #[test]
    fn get_sub_str_by_loc_round_trip() {
        let mut interner = setup();
        let og = "....123....";
        let loc = 4;
        let len = 3;
        let word = &og[loc..loc + len];
        assert_eq!(word, "123");
        let id = interner.register_str_by_loc(word, loc as u16);
        assert_eq!(interner.get_str(id), Some("123"));
        assert_eq!(interner.get_str_by_loc(loc as u16), Some("123"));
    }
}
