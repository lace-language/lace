use int::BinaryValue;

mod int;

pub struct Variable(usize);
pub struct Label(usize);
pub struct FunctionName(usize);

pub enum Value {
    Variable(Variable),
    Simple(BinaryValue),
}

pub enum Expr {
    Add(Value, Value),
    Sub(Value, Value),
    Mul(Value, Value),
    Div(Value, Value),
    Value(Value),
}

pub enum Statement {
    Assignment {
        expr: Expr,
        name: Variable,
    },
    Call {
        name: FunctionName, /* todo: more */
    },
}

pub enum Terminator {
    /// Unconditionally go to another basic block.
    Goto(Label),

    /// Make a choice what basic block we're going to.
    /// Both match and if desugar to this.
    Switch {
        on: Value,
        cases: Vec<(BinaryValue, Label)>,
        otherwise: Label,
    },

    /// Just stop. Right now.
    Abort,

    /// assumes the return value has been put
    /// into the right place already.
    Return,

    /// Like `unreachable_unchecked!()`, also inserted when the compiler
    /// can prove unreachableness. Becomes LLVM poison.
    Unreachable,
}

pub struct BasicBlock {
    /// A basic block has a name
    name: Label,
    /// Some statements
    stmts: Vec<Statement>,
    /// And then some last statement that might bring us to another basic block.
    terminator: Terminator,
}

pub struct Function {
    name: FunctionName,
    blocks: Vec<BasicBlock>,
}

pub struct Lir {
    pub functions: Vec<Function>,
}
