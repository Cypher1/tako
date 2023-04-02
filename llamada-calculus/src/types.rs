#[derive(Debug, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub struct Empty {}

impl std::fmt::Display for Empty {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Empty")
    }
}

#[derive(Debug, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub enum Never {}

impl std::fmt::Display for Never {
    fn fmt(&self, _f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            _ => unreachable!("Never can never be constructed"),
        }
    }
}
