
#[derive(Debug)]
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

