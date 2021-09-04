use crate::ast::{Entry, Symbol};
use crate::data_structures::tree::{to_hash_root, HashTree};

pub type Table = HashTree<Symbol, Entry>;

impl Default for Table {
    fn default() -> Table {
        to_hash_root(Entry::default())
    }
}

impl Table {
    #[must_use]
    pub fn find<'a>(self: &'a Table, path: &[Symbol]) -> Option<&'a Table> {
        // debug!("find in {:?}", self.value);
        if path.is_empty() {
            return Some(self);
        }
        self.children
            .get(&path[0])
            .and_then(|child| child.find(&path[1..]))
    }

    #[must_use]
    pub fn find_mut<'a>(self: &'a mut Table, path: &[Symbol]) -> Option<&'a mut Table> {
        // debug!("find in {:?}", self.value);
        if path.is_empty() {
            return Some(self);
        }
        self.children
            .get_mut(&path[0])
            .and_then(|child| child.find_mut(&path[1..]))
    }

    #[must_use]
    fn get_child_mut<'a>(self: &'a mut Table, find: &Symbol) -> &'a mut HashTree<Symbol, Entry> {
        self.children
            .entry(find.clone())
            .or_insert_with(Table::default)
    }

    pub fn get_mut<'a>(self: &'a mut Table, path: &[Symbol]) -> &'a mut HashTree<Symbol, Entry> {
        if path.is_empty() {
            return self;
        }
        self.get_child_mut(&path[0]).get_mut(&path[1..])
    }
}
