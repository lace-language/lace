use crate::parser::ast::{BinaryOp, UnaryOp};
use crate::syntax_id::NodeId;
use crate::typechecking::constraint_generation::BlockReturn;

pub enum ConstraintMetadata<'a> {
    /// Always a LICE later on in the compiler, but sometimes convenient
    #[allow(clippy::enum_variant_names)]
    NoConstraintMetadata,

    /// Generated for binary operators
    BinaryOp(NodeId, NodeId, BinaryOp),
    /// Generated for unary operators
    UnaryOp(NodeId, UnaryOp),

    /// Generated for the condition of ifs and whiles
    BlockCondition(NodeId),

    /// Generated for if/else blocks which return (the types should match, and we want to report this)
    IfReturn(NodeId, NodeId),

    /// generated for function calls
    Call {
        call_expr: NodeId,
    },

    /// generated for types which have explicitly gotten a type specified
    TypeSpec {
        spec: NodeId,
        name: NodeId,
    },

    /// generated when an expression is assigned to a name. `name` contains the ID of the name.
    Assignment {
        name: NodeId,
        value: NodeId,
    },

    /// Generated when a function is defined
    FunctionDefinition {
        value: NodeId,
    },

    /// Generated for return types of functions
    FunctionReturn {
        function: NodeId,
        ret: BlockReturn,
    },

    /// Generated when one variable references another variable,
    /// constraining the two in some way. Since these are variables, the node ids
    /// are stored in the NameMapping already
    NameRef,

    /// The Unify constraints are generated during solving, and their
    /// metadata always depends on another constraint previously generated during
    /// either solving or constraint generation
    TupleUnify {
        orig: &'a ConstraintMetadata<'a>,
    },
    FunctionParamUnify {
        orig: &'a ConstraintMetadata<'a>,
    },
    FunctionReturnUnify {
        orig: &'a ConstraintMetadata<'a>,
    },
}
