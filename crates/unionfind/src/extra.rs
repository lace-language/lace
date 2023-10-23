use crate::mapping::{GrowableMapping, RankMapping};
use std::convert::Infallible;
use std::error::Error;
use std::fmt::Debug;
use std::marker::PhantomData;

/// Trait that has to be implemented on types that want to be extra information for each
/// element of a [`GenericUnionFind`](crate::generic::UnionFind).
pub trait Extra<K, V> {
    type DefaultMappingErr: Error;

    fn default_mapping(elems: impl IntoIterator<Item = K>) -> Result<Self, Self::DefaultMappingErr>
    where
        Self: Sized;
}

/// () trivially implements Extra, which is the default when there is no extra info.
impl<K, V> Extra<K, V> for () {
    type DefaultMappingErr = Infallible;

    fn default_mapping(_elems: impl IntoIterator<Item = K>) -> Result<Self, Self::DefaultMappingErr>
    where
        Self: Sized,
    {
        Ok(())
    }
}

pub trait GrowableExtra<K, V> {
    type AddError: Error + Debug;

    fn add(&mut self, k: K, v: V) -> Result<(), Self::AddError>
    where
        Self: Sized;
}

/// () trivially implements GrowableExtra, which is the default when there is no extra info.
impl<K, V> GrowableExtra<K, V> for () {
    type AddError = Infallible;

    fn add(&mut self, _k: K, _v: V) -> Result<(), Self::AddError>
    where
        Self: Sized,
    {
        Ok(())
    }
}

#[derive(Debug)]
pub struct ByRank<M, T> {
    mapping: M,
    phantom: PhantomData<T>,
}

impl<M, T> ByRank<M, T>
where
    M: RankMapping<T>,
{
    pub fn new(elems: impl IntoIterator<Item = T>) -> Result<Self, <M as RankMapping<T>>::Err> {
        Ok(Self {
            mapping: M::zero_map(elems)?,
            phantom: Default::default(),
        })
    }
}

impl<M, T> ByRank<M, T>
where
    M: RankMapping<T>,
{
    pub fn rank(&self, elem: &T) -> Option<usize> {
        self.mapping.get(elem).cloned()
    }

    pub fn set_rank(&mut self, elem: T, rank: usize) {
        self.mapping.set(elem, rank)
    }
}

impl<M, T> Extra<T, usize> for ByRank<M, T>
where
    M: RankMapping<T>,
{
    type DefaultMappingErr = <M as RankMapping<T>>::Err;

    fn default_mapping(
        elems: impl IntoIterator<Item = T>,
    ) -> Result<Self, Self::DefaultMappingErr> {
        Ok(Self {
            mapping: M::zero_map(elems)?,
            phantom: Default::default(),
        })
    }
}

impl<M, T> GrowableExtra<T, usize> for ByRank<M, T>
where
    M: GrowableMapping<T, usize>,
{
    type AddError = <M as GrowableMapping<T, usize>>::AddError;

    fn add(&mut self, elem: T, value: usize) -> Result<(), Self::AddError> {
        self.mapping.add(elem, value)
    }
}
