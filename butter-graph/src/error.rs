use crate::function::{Function, Op};
use crate::node::NodeId;
use crate::value::Value;

#[derive(Debug, Hash, Ord, Eq, PartialOrd, PartialEq)]
pub enum GraphErr {
    FunctionCannotBeReversed(Function),
    TypeError(Op, Value, String),
    NodeMissing(NodeId),
    NoParent,
    NoParentForCommit,
    NoParentForExperiment,
    WriteErr(std::fmt::Error),
}

impl From<std::fmt::Error> for GraphErr {
    fn from(err: std::fmt::Error) -> Self {
        Self::WriteErr(err)
    }
}
