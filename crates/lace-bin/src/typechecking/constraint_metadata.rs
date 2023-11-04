use crate::ast_metadata::MetadataId;
use crate::parser::ast::{BinaryOp, UnaryOp};
use crate::typechecking::constraint_generation::BlockReturn;

#[derive(Debug)]
pub enum ConstraintMetadata<'a> {
    /// Always a LICE later on in the compiler, but sometimes convenient
    #[allow(clippy::enum_variant_names)]
    NoConstraintMetadata,

    /// Generated for binary operators
    BinaryOp(MetadataId, MetadataId, BinaryOp),
    /// Generated for unary operators
    UnaryOp(MetadataId, UnaryOp),

    /// Generated for the condition of ifs and whiles
    BlockCondition(MetadataId),

    /// Generated for if/else blocks which return (the types should match, and we want to report this)
    IfReturn(MetadataId, MetadataId),

    /// generated for function calls
    Call {
        call_expr: MetadataId,
    },

    /// generated for types which have explicitly gotten a type specified
    TypeSpec {
        spec: MetadataId,
        name: MetadataId,
    },

    /// generated when an expression is assigned to a name. `name` contains the ID of the name.
    Assignment {
        name: MetadataId,
        value: MetadataId,
    },

    /// Generated when a function is defined
    FunctionDefinition {
        value: MetadataId,
    },

    /// Generated for return types of functions
    FunctionReturn {
        function: MetadataId,
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
    ParamLength,
    TupleLength,
}
