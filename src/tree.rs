use std::fmt;

#[derive(Clone)]
pub struct Tree<T> {
    pub value: T,
    pub children: Vec<Tree<T>>,
}

pub fn to_root<T>(t: T) -> Tree<T> {
    Tree {
        value: t,
        children: vec![],
    }
}

impl<T: fmt::Debug> fmt::Debug for Tree<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.children.is_empty() {
            write!(f, "{:?}", self.value)
        } else {
            write!(f, "{:?} {:?}", self.value, self.children)
        }
    }
}
