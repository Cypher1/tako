use crate::function::Function;

#[derive(Copy, Clone, Hash, Ord, Eq, PartialOrd, PartialEq)]
pub struct NodeId {
    id: usize,
}

impl NodeId {
    pub fn new(id: usize) -> Self {
        Self { id }
    }
}

impl std::fmt::Debug for NodeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "node#{}", self.id)
    }
}
impl std::fmt::Display for NodeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "node#{}", self.id)
    }
}

#[derive(Debug, Clone, Hash, Ord, Eq, PartialOrd, PartialEq)]
pub struct Node {
    // Author?
    // Permissions?
    pub content: Function,
}

impl Node {
    pub fn new(content: Function) -> Self {
        Self { content }
    }

    pub fn function(f: Function) -> Self {
        Self::new(f)
    }
}
