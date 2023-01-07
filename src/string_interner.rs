use crate::keywords::KEYWORDS;
use crate::location::IndexIntoFile;
use crate::utils::typed_index::TypedIndex;
use std::collections::BTreeMap;
use std::hash::{Hash, Hasher};

type StringHash = u64;
// Ensures that str ids are unique per string but also stable across different files etc.
pub type StrId = TypedIndex<String, StringHash>;
pub type Identifier = StrId;

use static_assertions::*;
assert_eq_size!(Identifier, [u8; 8]);
assert_eq_size!([Identifier; 2], [u8; 16]);

#[derive(Clone, Default, Debug, Hash, PartialEq, Eq)]
pub struct StringInterner {
    // This ensures we can look up the string from the hash.
    // BUT: We can also merge the hashes without losing any information.
    pub loc2string: BTreeMap<IndexIntoFile, StrId>,
    pub strings: BTreeMap<StrId, String>,
}

impl StringInterner {
    pub fn new() -> Self {
        let mut n = Self::default();
        for key in KEYWORDS {
            n.register_str(key);
        }
        n
    }
    #[must_use]
    pub fn register_str_by_loc(&mut self, name: &str, ind: IndexIntoFile) -> StrId {
        let id = self.register_str(name);
        self.loc2string.insert(ind, id);
        id
    }
    pub fn register_str(&mut self, name: &str) -> StrId {
        let mut hasher = fxhash::FxHasher::default();
        name.hash(&mut hasher);
        let str_hash = hasher.finish();
        let id = TypedIndex::from_raw(str_hash);
        self.strings.entry(id).or_insert_with(|| name.to_string());
        id
    }
    pub fn get_str(&self, s: StrId) -> Option<&str> {
        self.strings.get(&s).map(|ref_string| &**ref_string)
    }
    pub fn get_str_by_loc(&self, s: IndexIntoFile) -> Option<&str> {
        let s = self.loc2string.get(&s)?;
        self.get_str(*s)
    }
}

#[derive(Debug)]
struct InContext<'a, T> {
    value: T,
    string_interner: &'a StringInterner,
}

impl<'a> std::fmt::Display for InContext<'a, Identifier> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(s) = self.string_interner.get_str(self.value) {
            write!(f, "{s}")
        } else {
            write!(f, "<unknown symbol: {self:?}>")
        }
    }
}
