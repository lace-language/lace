macro_rules! metadata {
    ($x:pat) => {
        $crate::ast_metadata::Metadata { value: $x, .. }
    };
}

macro_rules! int {
    ($i:literal) => {
        metadata!(ExprKind::Lit(Lit::Int(stringify!($i))))
    };
}

macro_rules! string {
    ($i:literal) => {
        metadata!(ExprKind::Lit(Lit::String($i)))
    };
}

macro_rules! ident_expr {
    ($i:ident) => {
        metadata!(ExprKind::Ident(metadata!(Ident {
            string: stringify!($i),
        })))
    };
}

macro_rules! ident {
    ($i:ident) => {
        metadata!(Ident {
            string: stringify!($i),
        })
    };
}

macro_rules! bool {
    ($i:literal) => {
        metadata!(ExprKind::Lit(Lit::Bool($i)))
    };
}

macro_rules! neg {
    ($x:pat) => {
        metadata!(ExprKind::UnaryOp(metadata!(UnaryOp::Neg), $x))
    };
}

macro_rules! not {
    ($x:pat) => {
        metadata!(ExprKind::UnaryOp(metadata!(UnaryOp::Not), $x))
    };
}

macro_rules! and {
    ($x:pat, $y:pat) => {
        metadata!(ExprKind::BinaryOp(metadata!(BinaryOp::LogicalAnd), $x, $y))
    };
}

macro_rules! or {
    ($x:pat, $y:pat) => {
        metadata!(ExprKind::BinaryOp(metadata!(BinaryOp::LogicalOr), $x, $y))
    };
}

macro_rules! add {
    ($x:pat, $y:pat) => {
        metadata!(ExprKind::BinaryOp(metadata!(BinaryOp::Add), $x, $y))
    };
}

macro_rules! sub {
    ($x:pat, $y:pat) => {
        metadata!(ExprKind::BinaryOp(metadata!(BinaryOp::Sub), $x, $y))
    };
}

macro_rules! mul {
    ($x:pat, $y:pat) => {
        metadata!(ExprKind::BinaryOp(metadata!(BinaryOp::Mul), $x, $y))
    };
}

macro_rules! div {
    ($x:pat, $y:pat) => {
        metadata!(ExprKind::BinaryOp(metadata!(BinaryOp::Div), $x, $y))
    };
}

macro_rules! gt {
    ($x:pat, $y:pat) => {
        metadata!(ExprKind::BinaryOp(metadata!(BinaryOp::Gt), $x, $y))
    };
}

macro_rules! gte {
    ($x:pat, $y:pat) => {
        metadata!(ExprKind::BinaryOp(metadata!(BinaryOp::Gte), $x, $y))
    };
}

macro_rules! lt {
    ($x:pat, $y:pat) => {
        metadata!(ExprKind::BinaryOp(metadata!(BinaryOp::Lt), $x, $y))
    };
}

macro_rules! lte {
    ($x:pat, $y:pat) => {
        metadata!(ExprKind::BinaryOp(metadata!(BinaryOp::Lte), $x, $y))
    };
}

macro_rules! eq {
    ($x:pat, $y:pat) => {
        metadata!(ExprKind::BinaryOp(metadata!(BinaryOp::Eq), $x, $y))
    };
}

macro_rules! neq {
    ($x:pat, $y:pat) => {
        metadata!(ExprKind::BinaryOp(metadata!(BinaryOp::Neq), $x, $y))
    };
}

macro_rules! paren {
    ($x:pat) => {
        metadata!(ExprKind::Paren($x))
    };
}

macro_rules! tuple {
    ($($x:pat),*) => {
        metadata!(ExprKind::Tuple(&[$($x),*]))
    };
}

macro_rules! stmt {
    (let: $x:ident, $ty: pat, $exp:pat) => {
        Statement::Let(
            metadata!(Ident {
                string: stringify!($x),
            }),
            Some($ty),
            $exp,
        )
    };
    (let: $x:ident, $exp:pat) => {
        Statement::Let(
            metadata!(Ident {
                string: stringify!($x),
            }),
            None,
            $exp,
        )
    };
    ($x:pat) => {
        Statement::Expr($x)
    };
}

macro_rules! call {
    ($callee: pat => [$($arg: pat),*]) => {
        metadata!(ExprKind::Call($callee, metadata!(&[$($arg),*])))
    };
}

macro_rules! block {
    ($($stmts:pat),*) => {
        metadata!(Block { stmts: &[$($stmts),*], last: None, .. })
    };
    ($($stmts:pat),* => $exp:pat) => {
        metadata!(Block { stmts: &[$($stmts),*], last: Some($exp), .. })
    };
}

macro_rules! file {
    ($($items:pat),*) => {
        File {
            items: &[$($items),*]
        }
    };
}

macro_rules! item {
    (func: $pat: pat) => {
        Item::Function(metadata!($pat))
    };
}

macro_rules! type_spec {
    (name: $name: ident) => {
        metadata!(TypeSpec::Name(ident!($name)))
    };
}

macro_rules! function {
    (fn $name: ident ($($arg:ident:$ty:pat),*) -> $ret:pat => $block: pat) => {
        Function {
            name: ident!($name),
            parameters: &[$(Parameter {
                name: ident!($arg),
                type_spec: $ty,
            }),*],
            ret: Some($ret),
            block: $block,
        }
    };

    (fn $name: ident ($($arg:ident:$ty:pat),*) => $block: pat) => {
        Function {
            name: ident!($name),
            parameters: &[$(Parameter {
                name: ident!($arg),
                type_spec: $ty,
            }),*],
            ret: None,
            block: $block,
        }
    };
}

macro_rules! block_expr {
    ($($tok:tt)*) =>  {
        metadata!(ExprKind::Block(block!{$($tok)*}))
    }
}

macro_rules! if_ {
    ($cond:pat, $then:pat) => {
        metadata!(ExprKind::If($cond, $then, None))
    };
    ($cond:pat, $then:pat, $else:pat) => {
        metadata!(ExprKind::If($cond, $then, Some($else)))
    };
}
