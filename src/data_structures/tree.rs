use std::collections::BTreeMap;
use std::fmt;

#[derive(Clone)]
pub struct Tree<T> {
    pub value: T,
    pub children: Vec<Tree<T>>,
}

impl<T: fmt::Debug> fmt::Debug for Tree<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.children.is_empty() {
            write!(f, "{:?}", self.value)
        } else {
            write!(f, "{:?} {:#?}", self.value, self.children)
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct HashTree<K, T>
where
    K: std::hash::Hash + std::cmp::Ord + PartialEq + Eq,
{
    pub value: T,
    pub children: BTreeMap<K, HashTree<K, T>>, // Only use btree map here to ensure ordering for debug
}

pub fn to_hash_root<K, T>(t: T) -> HashTree<K, T>
where
    K: std::hash::Hash + std::cmp::Ord + std::cmp::Eq,
{
    HashTree {
        value: t,
        children: BTreeMap::new(),
    }
}

impl<K, T: fmt::Debug> fmt::Debug for HashTree<K, T>
where
    K: fmt::Debug + std::hash::Hash + std::cmp::Ord + std::cmp::Eq,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.children.is_empty() {
            write!(f, "{:?}", self.value)
        } else {
            write!(f, "{:?} {:#?}", self.value, self.children)
        }
    }
}
