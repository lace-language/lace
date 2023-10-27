#[derive(Copy, Clone)]
pub enum PartialType<'a> {
    Int,
    Bool,
    Function {
        params: &'a [TypeOrVariable<'a>],
        ret: &'a TypeOrVariable<'a>
    },
    Tuple(&'a [TypeOrVariable<'a>]),
    String,
}

impl<'a> PartialType<'a> {
    #[allow(non_upper_case_globals)]
    pub const Unit: Self = Self::Tuple(&[]);
}

#[derive(Debug, Copy, Clone)]
pub struct TypeVariable(u64);

pub struct TypeVariableGenerator {
    curr: u64
}

impl TypeVariableGenerator {
    pub fn new() -> Self {
        Self {
            curr: 0,
        }
    }

    pub fn next(&mut self) -> TypeVariable {
        let old = self.curr;
        self.curr += 1;
        TypeVariable(old)
    }
}

#[derive(Copy, Clone)]
#[must_use]
pub enum TypeOrVariable<'a> {
    Concrete(PartialType<'a>),
    Variable(TypeVariable)
}

impl<'a> From<PartialType<'a>> for TypeOrVariable<'a> {
    fn from(value: PartialType<'a>) -> Self {
        Self::Concrete(value)
    }
}

impl<'a> From<TypeVariable> for TypeOrVariable<'a> {
    fn from(value: TypeVariable) -> Self {
        Self::Variable(value)
    }
}

