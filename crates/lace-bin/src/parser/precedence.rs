use crate::parser::ast::BinaryOp;
use std::cmp::Ordering;

/// Precedence of binary operators
///
/// The order of the variants of this enum is significant, because it defines
/// derived implementation of `PartialOrd`/`Ord`.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Precedence {
    /// The precedence of logical disjunction.
    Disjunction,

    /// The precedence of logical conjunction.
    Conjunction,

    /// The precedence of comparison operators.
    Comparison,

    /// The precedence of addition and subtraction.
    AddSub,

    /// The precedence of multiplication and division.
    MulDiv,
}

/// Associativity of binary operator.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Associativity {
    /// Left associativity.
    ///
    /// `<x> o <y> o <z>` is parsed as `(<x> o <y>) o <z>`.
    Left,

    /// Right associativity.
    ///
    /// `<x> o <y> o <z>` is parsed as `<x> o (<y> o <z>)`.
    #[allow(unused)]
    Right,

    /// Incompatible operators
    Not,
}

/// Specify whether an operator is compatible with another operator while parsing at a certain precedence level.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Compatibility {
    /// The operator is compatible with the previous, continue parsing at this level
    Continue,
    /// The operator is not compatible with the previous, stop parsing at this level
    Stop,
    /// The operator is so incompatible with the previous it's an error to write this at any level
    Incompatible,
}

impl BinaryOp {
    fn precedence(&self) -> Precedence {
        match self {
            Self::LogicalOr => Precedence::Disjunction,
            Self::LogicalAnd => Precedence::Conjunction,
            Self::Eq | Self::Neq | Self::Lt | Self::Lte | Self::Gt | Self::Gte => {
                Precedence::Comparison
            }
            Self::Mul | Self::Div => Precedence::MulDiv,
            Self::Add | Self::Sub => Precedence::AddSub,
        }
    }

    fn associativity(&self) -> Associativity {
        match self.precedence() {
            Precedence::Disjunction
            | Precedence::Conjunction
            | Precedence::AddSub
            | Precedence::MulDiv => Associativity::Left,
            Precedence::Comparison => Associativity::Not,
        }
    }

    pub fn compatibility(&self, other: &Self) -> Compatibility {
        match (
            self.precedence().cmp(&other.precedence()),
            self.associativity(),
        ) {
            // if the precedence of the left operator is less than the right operator
            // then stop parsing this part of the expression and go back to the previous
            // precedence level
            (Ordering::Less, _) => Compatibility::Stop,
            // if the precedence of the left operator is greater than the right operator,
            // we should keep parsing at this precedence level
            (Ordering::Greater, _) => Compatibility::Continue,
            // if we the two operators have equal precedence levels, and the operators are
            // a) not associative: error!
            (Ordering::Equal, Associativity::Not) => Compatibility::Incompatible,
            // b) right associative: continue!
            (Ordering::Equal, Associativity::Right) => Compatibility::Continue,
            // c) left associative: stop parsing at this level!
            (Ordering::Equal, Associativity::Left) => Compatibility::Stop,
        }
    }
}
