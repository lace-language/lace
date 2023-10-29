#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeId(pub usize);

pub trait WithNodeId {
    fn with_node_id(self, span: NodeId) -> Identified<Self>
    where
        Self: Sized;
}

impl<T> WithNodeId for T {
    fn with_node_id(self, node_id: NodeId) -> Identified<Self> {
        Identified {
            node_id,
            value: self,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Identified<T> {
    pub node_id: NodeId,
    pub value: T,
}
