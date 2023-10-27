
pub enum ConcreteType<'a> {
    Int,
    Bool,
    Function {
        params: &'a [ConcreteType<'a>],
        ret: &'a ConcreteType<'a>
    },
}

