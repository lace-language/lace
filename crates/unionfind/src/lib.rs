#![doc=include_str!("../README.md")]
//! # Usage
//! Generally, there are two types of union supported by this library.
//! 1. Union by rank: useful if you want to use a union find simply as a
//! disjoint set with logarithmic lookup time
//! 2. Custom unioning: likely what you want when you are, for example, implementing a type checker.
//!
//! This library abstracts over the backing storage of the union find. There are three options
//! 1. [`Vec`]: very fast lookups as long as keys are usize. Other keys are not supported.
//! 2. [`HashMap`]: works for any key that implements [`Hash`](std::hash::Hash) and [`Eq`]
//! 3. [`BTreeMap`]: works for any key that implements [`Ord`]
//!
//! Internally, a union find contains one ore more of these datastructures. One if
//! you use custom unioning, and two if you use union by rank (the ranks are stored separately).
//! However, even with custom unioning you could have a second HashMap for custom additional
//! information stored alongside the union find keys, similar to how union by rank works.
//!
//! Path shortening is optionally performed during find operations by calling
//! [`find_shorten`](UnionFind::find_shorten) instead of [`find`](UnionFind::find).
//! By using [`find_shorten`](UnionFind::find_shorten), subsequent finds become faster than the first.
//! However, an advantage to [`find`](UnionFind::find) is that it does not need mutable access to the datastructure

use crate::extra::ByRank;
use crate::generic::UnionFind;
use std::collections::{BTreeMap, HashMap};

pub mod extra;
pub mod generic;
pub mod mapping;
pub mod union;

#[cfg(test)]
mod tests;

pub type VecUnionFind<T> = UnionFind<T, (), Vec<T>, ()>;
pub type VecUnionFindByRank<T> = UnionFind<T, usize, Vec<T>, ByRank<Vec<T>, T>>;

pub type HashUnionFind<T> = UnionFind<T, (), HashMap<T, T>, ()>;
pub type HashUnionFindByRank<T> = UnionFind<T, usize, HashMap<T, T>, ByRank<HashMap<T, usize>, T>>;

pub type BTreeUnionFind<T> = UnionFind<T, (), BTreeMap<T, T>, ()>;
pub type BTreeUnionFindByRank<T> =
    UnionFind<T, usize, BTreeMap<T, T>, ByRank<BTreeMap<T, usize>, T>>;
