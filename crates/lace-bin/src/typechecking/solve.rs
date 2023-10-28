use crate::typechecking::ty::TypeOrVariable;
use unionfind::VecUnionFind;

pub struct SolvedTypes<'a> {
    uf: VecUnionFind<TypeOrVariable<'a>>,
}
