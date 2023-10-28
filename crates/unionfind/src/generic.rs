use crate::extra::{ByRank, Extra, GrowableExtra};
use crate::mapping::{
    GrowableIdentityMapping, GrowableMapping, Mapping, ParentMapping, RankMapping,
};
use crate::union::Union;
use std::cmp::Ordering;
use std::marker::PhantomData;
use thiserror::Error;

/// A union find data structure. Note that this implementation clones elements a lot.
/// Generally, you should use the data structure with small, preferably [`Copy`]able types,
/// like integers. However, arbitrary [`Clone`]+[`PartialEq`] types are possible.
#[derive(Debug, Clone)]
pub struct UnionFind<T, V, M, E = ()> {
    /// A mapping from some key to a parent key, for every key.
    /// When a key is in a class on its own, its parent is itself. Once
    /// unions start happening, multiple keys might get the same parent indicating
    /// they are unioned.
    parent: M,
    /// An optional array of extra information for each key.
    /// Under union by rank this is a `Mapping<T, usize>` to assign a rank to each element
    /// in the union find.
    extra: E,
    phantom: PhantomData<(T, V)>,
}

#[derive(Debug, Error, PartialEq)]
pub enum NewUnionFindError<P, E> {
    #[error("couldn't construct parent mapping")]
    Parent(#[source] P),

    #[error("couldn't construct extra mapping")]
    Extra(#[source] E),
}

type NewUnionFindErrorSimple<T, V, M, E> =
    NewUnionFindError<<M as ParentMapping<T>>::Err, <E as Extra<T, V>>::DefaultMappingErr>;

impl<T, V, M, E> UnionFind<T, V, M, E>
where
    M: ParentMapping<T>,
    T: Clone,
    E: Extra<T, V>,
{
    /// Constructs a new union find, allowing you to specify all type parameters.
    pub fn new(
        elems: impl IntoIterator<Item = T> + Clone,
    ) -> Result<Self, NewUnionFindErrorSimple<T, V, M, E>> {
        Ok(Self {
            parent: M::identity_map(elems.clone())
                .map_err(NewUnionFindErrorSimple::<T, V, M, E>::Parent)?,
            extra: E::default_mapping(elems)
                .map_err(NewUnionFindErrorSimple::<T, V, M, E>::Extra)?,
            phantom: Default::default(),
        })
    }
}

impl<T: PartialEq, V, M: Mapping<T, T>, E> UnionFind<T, V, M, E> {
    /// Find an element in the union find. Performs no path shortening,
    /// but can be used through an immutable reference.
    ///
    /// Use [`find_shorten`](UnionFind::find_shorten) for a more efficient find.
    pub fn find(&self, elem: &T) -> Option<T>
    where
        T: Clone,
    {
        let parent = self.parent.get(elem)?.clone();
        if &parent == elem {
            Some(parent)
        } else {
            let new_parent = self.find(&parent)?;
            Some(new_parent)
        }
    }

    /// Find an element in the union find. Performs path shortening,
    /// which means you need mutable access to the union find.
    ///
    /// Use [`find`](UnionFind::find) for an immutable version.
    pub fn find_shorten(&mut self, elem: &T) -> Option<T>
    where
        T: Clone,
    {
        let parent = self.parent.get(elem)?.clone();
        if &parent == elem {
            Some(parent)
        } else {
            let new_parent = self.find_shorten(&parent)?;
            // path shortening
            self.parent.set(elem.clone(), new_parent.clone());
            Some(new_parent)
        }
    }
}

#[derive(Error, Debug)]
pub enum UnionOrAddError<Err, T, V, M: GrowableMapping<T, T>, E: GrowableExtra<T, V>> {
    #[error(transparent)]
    AddError(AddErrorSimple<T, V, M, E>),

    #[error("could not union elements")]
    NotUnionable(Err),
}

impl<T: Clone + PartialEq, V, M, E> UnionFind<T, V, M, E>
where
    M: GrowableIdentityMapping<T>,
    E: GrowableExtra<T, V>,
    V: Default,
{
    /// Find an element in the union find. Performs no path shortening,
    /// but can be used through an immutable reference.
    /// If the element was not present in the unionfind previously, add it.
    ///
    /// Use [`find_shorten`](UnionFind::find_shorten_or_add) for a more efficient find.
    pub fn find_or_add(&mut self, elem: &T) -> Result<T, AddErrorSimple<T, V, M, E>>
    where
        T: Clone,
    {
        match self.find(elem) {
            Some(i) => Ok(i),
            None => {
                self.add(elem.clone())?;
                Ok(elem.clone())
            }
        }
    }

    /// Find an element in the union find. Performs path shortening,
    /// which means you need mutable access to the union find.
    /// If the element was not present in the unionfind previously, add it.
    ///
    /// Use [`find`](UnionFind::find) for an immutable version.
    pub fn find_shorten_or_add(&mut self, elem: &T) -> Result<T, AddErrorSimple<T, V, M, E>>
    where
        T: Clone,
    {
        match self.find_shorten(elem) {
            Some(i) => Ok(i),
            None => {
                self.add(elem.clone())?;
                Ok(elem.clone())
            }
        }
    }

    /// Union two elements in the union find. Try to add the elements if they were not already in.
    pub fn union_by_or_add<U: Union<T>>(
        &mut self,
        elem1: &T,
        elem2: &T,
        union: U,
    ) -> Result<UnionStatus, UnionOrAddError<U::Err, T, V, M, E>>
    where
        T: Clone,
    {
        let parent1 = self
            .find_shorten_or_add(elem1)
            .map_err(UnionOrAddError::AddError)?;
        let parent2 = self
            .find_shorten_or_add(elem2)
            .map_err(UnionOrAddError::AddError)?;

        self.union_helper(parent1, parent2, union)
            .map_err(UnionOrAddError::NotUnionable)
    }
}

impl<T: Clone + PartialEq, V, M, R> UnionFind<T, V, M, ByRank<R, T>>
where
    M: GrowableIdentityMapping<T>,
    V: Default,
    R: RankMapping<T>,
    ByRank<R, T>: GrowableExtra<T, V>,
{
    /// Union two elements in the union find by rank. Try to add the elements if they were not already in.
    pub fn union_by_rank_or_add(
        &mut self,
        elem1: &T,
        elem2: &T,
    ) -> Result<UnionStatus, AddErrorSimple<T, V, M, ByRank<R, T>>>
    where
        T: Clone,
    {
        let parent1 = self.find_shorten_or_add(elem1)?;
        let parent2 = self.find_shorten_or_add(elem2)?;

        Ok(self
            .union_by_rank_helper(parent1, parent2)
            .expect("this should never go wrong since we just added these"))
    }
}

#[derive(Error, Debug)]
pub enum UnionError<Err> {
    #[error("the first element given as an argument to union was not found in the union find")]
    Elem1NotFound,

    #[error("the second element given as an argument to union was not found in the union find")]
    Elem2NotFound,

    #[error("could not union elements")]
    NotUnionable(Err),
}

/// When a union is made, there is a possibility that the two classes
/// were already unioned before. This enum is returned to disambiguate the two cases.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum UnionStatus {
    /// Two unioned elements were already unioned in the past
    AlreadyEquivalent,
    /// Two unioned elements were previously not unioned
    PerformedUnion,
}

impl<T: PartialEq, V, M, E> UnionFind<T, V, M, E>
where
    M: Mapping<T, T>,
{
    fn union_helper<U: Union<T>>(
        &mut self,
        parent1: T,
        parent2: T,
        mut union: U,
    ) -> Result<UnionStatus, U::Err>
    where
        T: Clone,
    {
        if parent1 == parent2 {
            return Ok(UnionStatus::AlreadyEquivalent);
        }

        let res = union.union(parent1.clone(), parent2.clone())?;

        self.parent.set(parent1, res.clone());
        self.parent.set(parent2, res);

        Ok(UnionStatus::PerformedUnion)
    }

    /// union two elements in the union find
    pub fn union_by<U: Union<T>>(
        &mut self,
        elem1: &T,
        elem2: &T,
        union: U,
    ) -> Result<UnionStatus, UnionError<U::Err>>
    where
        T: Clone,
    {
        let parent1 = self.find_shorten(elem1).ok_or(UnionError::Elem1NotFound)?;
        let parent2 = self.find_shorten(elem2).ok_or(UnionError::Elem2NotFound)?;

        self.union_helper(parent1, parent2, union)
            .map_err(UnionError::NotUnionable)
    }
}

#[derive(Error, Debug)]
pub enum UnionByRankError {
    #[error("the first element given as an argument to union was not found in the union find")]
    Elem1NotFound,

    #[error("the second element given as an argument to union was not found in the union find")]
    Elem2NotFound,
}

impl<T, V, M, R> UnionFind<T, V, M, ByRank<R, T>>
where
    M: Mapping<T, T>,
    R: RankMapping<T>,
    T: Clone + PartialEq,
{
    /// union two elements in the union find by rank
    pub fn union_by_rank(&mut self, elem1: &T, elem2: &T) -> Result<UnionStatus, UnionByRankError> {
        let parent1 = self
            .find_shorten(elem1)
            .ok_or(UnionByRankError::Elem1NotFound)?;
        let parent2 = self
            .find_shorten(elem2)
            .ok_or(UnionByRankError::Elem2NotFound)?;

        self.union_by_rank_helper(parent1, parent2)
    }

    fn union_by_rank_helper(
        &mut self,
        parent1: T,
        parent2: T,
    ) -> Result<UnionStatus, UnionByRankError>
    where
        T: Clone,
    {
        if parent1 == parent2 {
            return Ok(UnionStatus::AlreadyEquivalent);
        }

        let rank1 = self
            .extra
            .rank(&parent1)
            .ok_or(UnionByRankError::Elem1NotFound)?;
        let rank2 = self
            .extra
            .rank(&parent2)
            .ok_or(UnionByRankError::Elem2NotFound)?;

        match rank1.cmp(&rank2) {
            Ordering::Less => {
                self.parent.set(parent1, parent2);
            }
            Ordering::Equal => {
                self.parent.set(parent1, parent2.clone());
                self.extra.set_rank(parent2, rank2 + 1);
            }
            Ordering::Greater => {
                self.parent.set(parent2, parent1);
            }
        }

        Ok(UnionStatus::PerformedUnion)
    }
}

#[derive(Debug, Error, PartialEq)]
pub enum AddError<E, P> {
    #[error("couldn't add element to parent mapping")]
    Parent(P),

    #[error("couldn't add element to extra mapping")]
    Extra(#[source] E),
}

type AddErrorSimple<T, V, M, E> =
    AddError<<E as GrowableExtra<T, V>>::AddError, <M as GrowableMapping<T, T>>::AddError>;

impl<T: Clone, V, M, E> UnionFind<T, V, M, E>
where
    M: GrowableIdentityMapping<T>,
    E: GrowableExtra<T, V>,
    V: Default,
{
    pub fn add(&mut self, elem: T) -> Result<(), AddErrorSimple<T, V, M, E>> {
        self.parent
            .add_identity(elem.clone())
            .map_err(AddError::Parent)?;
        self.extra
            .add(elem, Default::default())
            .map_err(AddError::Extra)?;
        Ok(())
    }
}

impl<T: Clone, V, M, E> UnionFind<T, V, M, E>
where
    M: GrowableIdentityMapping<T>,
    E: GrowableExtra<T, V>,
{
    pub fn add_with_extra(&mut self, elem: T, extra: V) -> Result<(), AddErrorSimple<T, V, M, E>> {
        self.parent
            .add_identity(elem.clone())
            .map_err(AddError::Parent)?;
        self.extra.add(elem, extra).map_err(AddError::Extra)?;
        Ok(())
    }
}
