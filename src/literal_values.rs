use std::hash::{Hash, Hasher};
use crate::utils::typed_index::TypedIndex;
use std::collections::BTreeMap;
use crate::location::IndexIntoFile;

type StringHash = u64;
// Ensures that str ids are unique per string but also stable across different files etc.
pub type StrId = TypedIndex<String, StringHash>;
pub type NamedSymbol = StrId;

use static_assertions::*;
assert_eq_size!(NamedSymbol, [u8; 8]);
assert_eq_size!([NamedSymbol; 2], [u8; 16]);

#[derive(Clone, Default, Debug, Hash, PartialEq, Eq)]
pub struct LiteralValues {
    // This ensures we can look up the string from the hash.
    // BUT: We can also merge the hashes without losing any information.
    pub loc2string: BTreeMap<IndexIntoFile, StrId>,
    pub strings: BTreeMap<StrId, String>,
}

impl LiteralValues {
    pub fn register_str_by_loc(&mut self, name: String, ind: IndexIntoFile) -> StrId {
        let id = self.register_str(name);
        self.loc2string.insert(ind, id);
        id
    }
    pub fn register_str(&mut self, name: String) -> StrId {
        let mut hasher = fxhash::FxHasher::default();
        name.hash(&mut hasher);
        let str_hash = hasher.finish();
        let id = TypedIndex::from_raw(str_hash);
        self.strings.entry(id).or_insert(name);
        id
    }
    pub fn get_str(&self, s: StrId) -> Option<&str> {
        self.strings
            .get(&s)
            .map(|ref_string| &**ref_string)
    }
    pub fn get_str_by_loc(&self, s: IndexIntoFile) -> Option<&str> {
        let s = self.loc2string
            .get(&s)?;
        self.get_str(*s)
    }
}

#[derive(Debug)]
struct InContext<'a, T> {
    value: T,
    literal_values: &'a LiteralValues,
}

impl<'a> std::fmt::Display for InContext<'a, NamedSymbol> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(s) = self.literal_values.get_str(self.value) {
            write!(f, "{s}")
        } else {
            write!(f, "<unknown symbol: {self:?}>")
        }
    }
}

