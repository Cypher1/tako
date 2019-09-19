use std::fmt;

#[derive(Clone)]
pub struct Tree<T> {
    pub value: T,
    pub children: Vec<Tree<T>>,
}

impl<T: fmt::Debug> fmt::Debug for Tree<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.children.len() == 0 {
            write!(f, "{:?}", self.value)
        } else {
            write!(f, "{:?} {:?}", self.value, self.children)
        }
    }
}
