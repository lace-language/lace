use crate::generic::{AddError, NewUnionFindError, UnionStatus};
use crate::mapping::NotInOrder;
use crate::{
    BTreeUnionFind, BTreeUnionFindByRank, HashUnionFind, HashUnionFindByRank, VecUnionFind,
    VecUnionFindByRank,
};

#[test]
pub fn simple() {
    macro_rules! simple_uf {
        ($ty: path) => {{
            type T = $ty;

            let uf = T::new([0, 1, 2, 3, 4]).unwrap();

            assert_eq!(uf.find(&0), Some(0));
            assert_eq!(uf.find(&1), Some(1));
            assert_eq!(uf.find(&2), Some(2));
            assert_eq!(uf.find(&3), Some(3));
            assert_eq!(uf.find(&4), Some(4));
        }};
    }

    simple_uf!(HashUnionFind::<usize>);
    simple_uf!(BTreeUnionFind::<usize>);
    simple_uf!(VecUnionFind::<usize>);
}

#[test]
fn not_in_order() {
    // vec union finds should start at 0
    assert_eq!(
        VecUnionFind::new([1, 2, 3]).unwrap_err(),
        NewUnionFindError::Parent(NotInOrder)
    );

    let mut uf = VecUnionFind::new([0, 1, 2, 3]).unwrap();
    assert_eq!(uf.add(8), Err(AddError::Parent(NotInOrder)));
}

#[test]
pub fn union() {
    let mut uf = VecUnionFindByRank::new([0, 1, 2, 3, 4]).unwrap();
    uf.union_by_rank(&1, &2).unwrap();

    assert_eq!(uf.find(&0), Some(0));
    assert_eq!(uf.find(&1), uf.find(&2));
    assert_eq!(uf.find(&3), Some(3));
    assert_eq!(uf.find(&4), Some(4));
}

#[test]
pub fn double_union() {
    macro_rules! test_double_union {
        ($ty1: path; $ty2: path) => {{
            type T1 = $ty1;
            type T2 = $ty2;

            let mut uf = T1::new([0, 1]).unwrap();
            assert_eq!(
                uf.union_by(&0, &1, std::cmp::max).unwrap(),
                UnionStatus::PerformedUnion
            );
            assert_eq!(
                uf.union_by(&0, &1, std::cmp::max).unwrap(),
                UnionStatus::AlreadyEquivalent
            );

            let mut uf = T2::new([0, 1]).unwrap();
            assert_eq!(
                uf.union_by_rank(&0, &1).unwrap(),
                UnionStatus::PerformedUnion
            );
            assert_eq!(
                uf.union_by_rank(&0, &1).unwrap(),
                UnionStatus::AlreadyEquivalent
            );
        }};
    }
    test_double_union!(VecUnionFind::<usize>; VecUnionFindByRank::<usize>);
    test_double_union!(HashUnionFind::<usize>; HashUnionFindByRank::<usize>);
    test_double_union!(BTreeUnionFind::<usize>; BTreeUnionFindByRank::<usize>);
}

#[test]
pub fn union_by() {
    let mut uf = VecUnionFind::new([0, 1, 2, 3, 4]).unwrap();
    uf.union_by(&1, &2, |a, _b| a).unwrap();

    assert_eq!(uf.find(&0), Some(0));
    assert_eq!(uf.find(&1), Some(1));
    assert_eq!(uf.find(&2), Some(1));
    assert_eq!(uf.find(&3), Some(3));
    assert_eq!(uf.find(&4), Some(4));
}

#[test]
pub fn grow() {
    macro_rules! grow_test {
        ($ty: path) => {{
            type T = $ty;
            let mut uf = T::new([0, 1, 2]).unwrap();
            uf.union_by_rank(&0, &2).unwrap();

            assert_eq!(uf.find(&0), uf.find(&2));
            assert_eq!(uf.find(&1), Some(1));

            uf.add(3).unwrap();

            assert_eq!(uf.find(&0), uf.find(&2));
            assert_eq!(uf.find(&1), Some(1));

            uf.union_by_rank(&3, &1).unwrap();

            assert_eq!(uf.find(&0), uf.find(&2));
            assert_eq!(uf.find(&1), uf.find(&3));
        }};
    }

    grow_test!(VecUnionFindByRank::<usize>);
    grow_test!(HashUnionFindByRank::<usize>);
    grow_test!(BTreeUnionFindByRank::<usize>);
}

#[test]
pub fn grow_non_consecutive() {
    macro_rules! grow_test {
        ($ty: path) => {{
            type T = $ty;
            let mut uf = T::new([8, 1, 2]).unwrap();
            uf.union_by_rank(&8, &2).unwrap();

            assert_eq!(uf.find(&8), uf.find(&2));
            assert_eq!(uf.find(&1), Some(1));

            uf.add(9).unwrap();

            assert_eq!(uf.find(&8), uf.find(&2));
            assert_eq!(uf.find(&1), Some(1));

            uf.union_by_rank(&9, &1).unwrap();

            assert_eq!(uf.find(&8), uf.find(&2));
            assert_eq!(uf.find(&1), uf.find(&9));
        }};
    }

    grow_test!(HashUnionFindByRank::<usize>);
    grow_test!(BTreeUnionFindByRank::<usize>);
}

#[test]
pub fn union_by_rank() {
    macro_rules! by_rank_test {
        ($ty: path) => {{
            type T = $ty;
            let mut uf = T::new(0..20).unwrap();
            uf.union_by_rank(&0, &1).unwrap();
            uf.union_by_rank(&2, &0).unwrap();
            uf.union_by_rank(&0, &3).unwrap();

            assert_eq!(uf.find(&1), uf.find(&3));
            assert_ne!(uf.find(&2), uf.find(&8));
            assert_ne!(uf.find(&6), uf.find(&8));

            uf.union_by_rank(&5, &6).unwrap();
            uf.union_by_rank(&7, &8).unwrap();
            uf.union_by_rank(&5, &7).unwrap();

            assert_eq!(uf.find(&8), uf.find(&6));

            uf.union_by_rank(&10, &11).unwrap();
            uf.union_by_rank(&12, &13).unwrap();
            uf.union_by_rank(&11, &13).unwrap();

            assert_eq!(uf.find(&10), uf.find(&12));

            uf.union_by_rank(&14, &15).unwrap();
            uf.union_by_rank(&16, &17).unwrap();
            uf.union_by_rank(&14, &17).unwrap();

            assert_eq!(uf.find(&15), uf.find(&16));
        }};
    }

    by_rank_test!(VecUnionFindByRank::<usize>);
    by_rank_test!(HashUnionFindByRank::<usize>);
    by_rank_test!(BTreeUnionFindByRank::<usize>);
}

#[test]
fn or_add() {
    macro_rules! or_add_test {
        ($ty: path) => {{
            type T = $ty;

            let mut uf = T::new(0..10).unwrap();
            assert_ne!(uf.find(&0), uf.find(&1));
            assert_ne!(uf.find(&0), uf.find(&10));
            assert_eq!(uf.find(&10), None);

            uf.union_by(&0, &11, std::cmp::max).unwrap_err();
            assert_ne!(uf.find(&0), uf.find(&10));
            assert_eq!(uf.find(&10), None);

            uf.union_by_or_add(&0, &10, std::cmp::max).unwrap();
            assert_eq!(uf.find(&0), uf.find(&10));
            assert!(uf.find(&10).is_some());

            assert_eq!(uf.find(&11), None);
            assert_eq!(uf.find_or_add(&11), Ok(11));
            assert_eq!(uf.find_or_add(&11), Ok(11));
            assert_eq!(uf.find(&12), None);
            assert_eq!(uf.find_shorten_or_add(&12), Ok(12));
            assert_eq!(uf.find_shorten_or_add(&12), Ok(12));
        }};
    }
    or_add_test!(VecUnionFind::<usize>);
    or_add_test!(HashUnionFind::<usize>);
    or_add_test!(BTreeUnionFind::<usize>);
}

#[test]
fn or_add_by_rank() {
    macro_rules! or_add_test {
        ($ty: path) => {{
            type T = $ty;

            let mut uf = T::new(0..10).unwrap();
            assert_ne!(uf.find(&0), uf.find(&1));
            assert_ne!(uf.find(&0), uf.find(&10));
            assert_eq!(uf.find(&10), None);

            uf.union_by_rank(&0, &11).unwrap_err();
            assert_ne!(uf.find(&0), uf.find(&10));
            assert_eq!(uf.find(&10), None);

            uf.union_by_rank_or_add(&0, &10).unwrap();
            assert_eq!(uf.find(&0), uf.find(&10));
            assert!(uf.find(&10).is_some());
        }};
    }
    or_add_test!(VecUnionFindByRank::<usize>);
    or_add_test!(HashUnionFindByRank::<usize>);
    or_add_test!(BTreeUnionFindByRank::<usize>);
}

#[derive(Debug, PartialOrd, PartialEq, Ord, Eq, Copy, Clone, Hash)]
pub enum CustomType {
    A,
    B,
    C,
    D,
}

#[test]
fn custom_uf() {
    use CustomType::*;

    let mut uf = HashUnionFindByRank::new([A, B, C, D]).unwrap();
    uf.union_by_rank(&A, &B).unwrap();
    uf.union_by_rank(&C, &D).unwrap();
    assert_ne!(uf.find(&B), uf.find(&D));
    assert_eq!(uf.find(&A), uf.find(&B));
    assert_eq!(uf.find(&C), uf.find(&D));
    uf.union_by_rank(&A, &C).unwrap();
    assert_eq!(uf.find(&B), uf.find(&D));

    let mut uf = BTreeUnionFindByRank::new([A, B, C, D]).unwrap();
    uf.union_by_rank(&A, &B).unwrap();
    uf.union_by_rank(&C, &D).unwrap();
    assert_ne!(uf.find(&B), uf.find(&D));
    assert_eq!(uf.find(&A), uf.find(&B));
    assert_eq!(uf.find(&C), uf.find(&D));
    uf.union_by_rank(&A, &C).unwrap();
    assert_eq!(uf.find(&B), uf.find(&D));
}
