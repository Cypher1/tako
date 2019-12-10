use std::fmt;

#[derive(Clone)]
pub struct Tree<T> {
    pub value: T,
    pub children: Vec<Tree<T>>,
}

pub fn to_tree<T: std::clone::Clone>(t: Vec<T>) -> Vec<Tree<T>> {
    let mut output = Vec::new();
    output.extend(t.iter().map(to_root));
    return output;
}

pub fn to_root<T: std::clone::Clone>(t: &T) -> Tree<T> {
    Tree {
        value: t.clone(),
        children: vec![],
    }
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

impl<T: fmt::Display> fmt::Display for Tree<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.children.len() == 0 {
            write!(f, "{}", self.value)
        } else {
            write!(f, "({}", self.value).ok();
            for ch in self.children.iter() {
                write!(f, " {}", ch).ok();
            }
            write!(f, ")")
        }
    }
}
