use crate::lowering::variable::VariableDeclaration;
use derive_more::From;
use int::BinaryValue;

pub mod fmt;
pub mod int;

#[derive(Debug, PartialEq, Eq, From, Copy, Clone, Hash, Ord, PartialOrd)]
pub struct Variable(usize);
#[derive(Debug, PartialEq, Eq, From, Copy, Clone, Hash, Ord, PartialOrd)]
pub struct Label(usize);
// TODO: also include original function name for debugging
#[derive(Debug, PartialEq, Eq, From, Copy, Clone, Hash, Ord, PartialOrd)]
pub struct FunctionName(usize);

/// A place is a loction in which the outcome of an expression can be stored.
/// The left side of an assignment Statement. The right side of an assignment
/// can also contain Places, but also constants for example. Those are represented
/// by a [`Value`]
#[derive(Debug, Clone)]
pub enum Place {
    Variable(Variable),
    #[allow(unused)] // TODO: remove
    Void,
}

impl From<Variable> for Place {
    fn from(value: Variable) -> Self {
        Place::Variable(value)
    }
}

/// A value is something used in the right side of an expression. This can be a [`Place`],
/// or a constant [`BinaryValue`] such as an integer
#[derive(Debug, Clone)]
pub enum Value {
    Place(Place),
    Binary(BinaryValue),
}

impl From<BinaryValue> for Value {
    fn from(value: BinaryValue) -> Self {
        Self::Binary(value)
    }
}

impl From<Place> for Value {
    fn from(value: Place) -> Self {
        Self::Place(value)
    }
}

impl From<Variable> for Value {
    fn from(value: Variable) -> Self {
        Self::Place(value.into())
    }
}

pub enum Expr {
    Add(Value, Value),
    Sub(Value, Value),
    Mul(Value, Value),
    Div(Value, Value),

    Gt(Value, Value),
    Gte(Value, Value),
    Lt(Value, Value),
    Lte(Value, Value),
    Eq(Value, Value),
    Neq(Value, Value),

    Not(Value),
    Neg(Value),
    Value(Value),

    #[allow(unused)] // TODO: remove
    Call {
        function_name: Label,
    },
}

impl From<Value> for Expr {
    fn from(value: Value) -> Self {
        Self::Value(value)
    }
}

impl From<Place> for Expr {
    fn from(value: Place) -> Self {
        Self::Value(value.into())
    }
}

pub enum Statement {
    /// assign expr into place
    Assignment { expr: Expr, place: Place },
}

#[allow(unused)] // TODO: remove
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
    pub name: Label,
    /// Some statements
    pub stmts: Vec<Statement>,
    /// And then some last statement that might bring us to another basic block.
    pub terminator: Terminator,
}

pub struct Function {
    pub name: FunctionName,
    pub variables: Vec<VariableDeclaration>,
    pub blocks: Vec<BasicBlock>,
}

pub struct Lir {
    pub functions: Vec<Function>,
}
