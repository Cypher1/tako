#![allow(unused)]
use crate::node::{NodeId, Node};
use crate::error::GraphErr;
use crate::function::{Function, FunctionBuilder};
use crate::value::Value;
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, Mutex};

pub trait Graphic {
    fn with_graph<T>(&self, f: impl FnOnce(&Graph) -> Result<T, GraphErr>) -> Result<T, GraphErr>;
}

impl Graphic for Graph {
    fn with_graph<T>(&self, f: impl FnOnce(&Graph) -> Result<T, GraphErr>) -> Result<T, GraphErr> {
        f(self)
    }
}

impl Graphic for Arc<Mutex<Graph>> {
    fn with_graph<T>(&self, f: impl FnOnce(&Graph) -> Result<T, GraphErr>) -> Result<T, GraphErr> {
        let g = self.lock().expect("Get g failed");
        f(&g)
    }
}

#[derive(Default, Debug)]
pub struct Watch {
}

#[derive(Debug)]
pub struct Graph {
    // Shared state / relations:
    parent: Option<Arc<Mutex<Graph>>>, // For temporary edits or parallel versions?
    node_ids: Arc<Mutex<usize>>,
    // Model:
    nodes: HashMap<NodeId, Node>,
    deleted_set: Vec<NodeId>, // Nodes to delete.

    // Cache / computation data:
    pub watch_set: HashMap<NodeId, Watch>,
    value_cache: HashMap<NodeId, Value>,
    dependencies: HashMap<NodeId, HashSet<NodeId>>,
    dirty_set: Vec<NodeId>, // Dependents need to be propagated.
}

impl std::fmt::Display for Graph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut nodes = Vec::new();
        nodes.extend(self.all_nodes().iter());
        nodes.sort(); // SORT?
        for node in nodes {
            write!(f, "{node}:")?;
            if let Some(deps) = self.dependencies.get(&node) {
                write!(f, " {:?}", deps)?;
            }
            write!(f, " {}", &self.get(node).unwrap().content)?;
            if let Some(_watch) = self.watch_set.get(&node) {
                write!(f, " => ")?;
                let value = self.compute(node, &[]);
                match value {
                    Ok(value) => write!(f, "{value}")?,
                    Err(err) => write!(f, "{err:?}")?,
                }
            }
            write!(f, "\n");
        }
        Ok(())
    }
}

impl Graph {
    pub fn new() -> Self {
        Self {
            parent: None,
            nodes: HashMap::new(),
            node_ids: Arc::new(Mutex::new(0)),
            dirty_set: Vec::new(),
            watch_set: HashMap::new(),
            value_cache: HashMap::new(),
            dependencies: HashMap::new(),
            deleted_set: Vec::new(),
        }
    }
    pub fn new_shared() -> Arc<Mutex<Self>> {
        Arc::new(Mutex::new(Graph::new()))
    }

    pub fn compute(&self, id: NodeId, bindings: &[Value]) -> Result<Value, GraphErr> {
        let node = self.get(id)?;
        node.content.compute(self, bindings)
    }

    pub fn all_nodes(&self) -> HashSet<NodeId> {
        let mut ids = self.with_parent(|parent| {
            Ok(parent.all_nodes())
        }).unwrap_or_default();
        ids.extend(self.nodes.keys());
        ids
    }

    fn with_parent<T>(
        &self,
        f: impl FnOnce(&mut Graph) -> Result<T, GraphErr>,
    ) -> Result<T, GraphErr> {
        if let Some(parent) = &self.parent {
            let mut parent = parent.lock().expect("Parent graph locking failed");
            f(&mut parent)
        } else {
            Err(GraphErr::NoParent)
        }
    }

    pub fn overlay(parent: &Arc<Mutex<Self>>) -> Self {
        let node_ids = {
            let parent = parent.lock().expect("Parent graph locking failed");
            parent.node_ids.clone()
        };
        Self {
            parent: Some(parent.clone()),
            node_ids,
            ..Self::new()
        }
    }

    pub fn commit(&mut self) -> Result<(), GraphErr> {
        // TODO: Require that the graph is in a good state?
        // Or at least as good a state as the parent??
        self.with_parent(|parent| {
            for (node, value) in &self.nodes {
                parent.set(*node, value.content.clone())?;
            }
            for id in &self.deleted_set {
                parent.remove(*id)?;
            }
            Ok(())
        }).map_err(|_e| GraphErr::NoParentForCommit)
    }

    fn with_node<T>(
        &self,
        id: NodeId,
        f: impl FnOnce(&Node) -> Result<T, GraphErr>,
    ) -> Result<T, GraphErr> {
        if let Some(node) = self.nodes.get(&id) {
            (f)(node)
        } else {
            self.with_parent(|parent| {
                parent.with_node(id, f)
            }).map_err(|_e| GraphErr::NodeMissing(id))
        }
    }

    fn with_node_mut<T>(
        &mut self,
        id: NodeId,
        f: impl FnOnce(&mut Node) -> Result<T, GraphErr>,
    ) -> Result<T, GraphErr> {
        if let Some(node) = self.nodes.get_mut(&id) {
            (f)(node)
        } else {
            self.with_parent(|parent| {
                parent.with_node_mut(id, f)
            }).map_err(|_e| GraphErr::NodeMissing(id))
        }
    }

    fn with_new_node<T>(
        &mut self,
        id: NodeId,
        default: Node,
        f: impl FnOnce(&mut Node) -> Result<T, GraphErr>,
    ) -> Result<T, GraphErr> {
        self.nodes.insert(id, default);
        self.with_node_mut(id, f)
    }

    pub fn get(&self, id: NodeId) -> Result<Node, GraphErr> {
        self.with_node(id, |node| Ok(node.clone()))
    }

    pub fn set(&mut self, id: NodeId, content: Function) -> Result<(), GraphErr> {
        self.dependencies.insert(id, content.dependencies()); // Update the dependency graph
        // TODO: Avoid duplication?
        self.with_new_node(id, Node::function(content.clone()), |node| {
            node.content = content;
            Ok(())
        })?;
        self.dirty_set.push(id);
        Ok(())
    }

    pub fn request_value(&mut self, id: NodeId, value: Value) -> Result<(), GraphErr> {
        let Some(_parent) = &self.parent else {
            return Err(GraphErr::NoParentForExperiment)
        };
        let fix = self.with_node_mut(id, |node| {
            let func = &node.content;
            let from = func.source()?;
            let value = func.reversed(value)?;
            Ok(Some((from, value)))
        })?;
        self.dirty_set.push(id);
        if let Some((changed_id, value)) = fix {
            // TODO: Check for cycles?
            self.request_value(changed_id, value)?;
            self.dirty_set.push(changed_id);
        }
        Ok(())
    }

    pub fn watch(&mut self, id: NodeId) {
        self.watch_set.insert(id, Watch::default());
    }

    pub fn unwatch(&mut self, id: NodeId) {
        self.watch_set.remove(&id);
    }

    fn get_node_id(&mut self) -> NodeId {
        let mut node_ids = self.node_ids.lock().expect("locking node_ids failed");
        *node_ids += 1;
        NodeId::new(*node_ids - 1)
    }

    pub fn add_fn(&mut self, node: impl FnOnce(&mut FunctionBuilder)) -> NodeId {
        self.add(Function::build(node))
    }
    pub fn add(&mut self, node: impl Into<Function>) -> NodeId {
        let id = self.get_node_id();
        self.set(id, node.into());
        id
    }
    pub fn remove(&mut self, id: NodeId) -> Result<Node, GraphErr> {
        self.deleted_set.push(id);
        // TODO: Check for dependants.
        let Some(value) = self.nodes.remove(&id) else {
            return Err(GraphErr::NodeMissing(id));
        };
        Ok(value)
    }
}
