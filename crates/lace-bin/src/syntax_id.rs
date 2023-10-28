use std::ops::{Deref, DerefMut};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeId(pub usize);

pub trait WithNodeId {
    fn with_node_id(self, span: NodeId) -> Identified<Self>
    where
        Self: Sized;
}

impl<T> WithNodeId for T {
    fn with_node_id(self, node_id: NodeId) -> Identified<Self> {
        Identified { node_id, value: self }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Identified<T> {
    pub node_id: NodeId,
    pub value: T,
}

impl<T> Deref for Identified<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> DerefMut for Identified<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

impl<T> Identified<T> {
    pub fn span(&self) -> NodeId {
        self.node_id
    }
}
