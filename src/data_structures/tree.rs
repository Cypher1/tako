use std::collections::HashMap;
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
    K: std::hash::Hash,
    K: PartialEq,
    K: Eq,
{
    pub value: T,
    pub children: HashMap<K, HashTree<K, T>>,
}

pub fn to_hash_root<K, T>(t: T) -> HashTree<K, T>
where
    K: std::hash::Hash,
    K: std::cmp::Eq,
{
    HashTree {
        value: t,
        children: HashMap::new(),
    }
}

impl<K, T: fmt::Debug> fmt::Debug for HashTree<K, T>
where
    K: fmt::Debug,
    K: std::hash::Hash,
    K: std::cmp::Eq,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.children.is_empty() {
            write!(f, "{:?}", self.value)
        } else {
            write!(f, "{:?} {:#?}", self.value, self.children)
        }
    }
}
