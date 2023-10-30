#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MetadataId(pub usize);

pub trait WithNodeId {
    fn with_metadata(self, span: MetadataId) -> Metadata<Self>
    where
        Self: Sized;
}

impl<T> WithNodeId for T {
    fn with_metadata(self, metadata: MetadataId) -> Metadata<Self> {
        Metadata {
            metadata,
            value: self,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Metadata<T> {
    pub metadata: MetadataId,
    pub value: T,
}
