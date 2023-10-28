macro_rules! identified {
    ($x:pat) => {
        $crate::syntax_id::Identified { value: $x, .. }
    };
}

macro_rules! int {
    ($i:literal) => {
        identified!(ExprKind::Lit(Lit::Int(stringify!($i))))
    };
}

macro_rules! string {
    ($i:literal) => {
        identified!(ExprKind::Lit(Lit::String($i)))
    };
}

macro_rules! ident_expr {
    ($i:ident) => {
        identified!(ExprKind::Ident(identified!(Ident {
            string: stringify!($i),
        })))
    };
}

macro_rules! ident {
    ($i:ident) => {
        identified!(Ident {
            string: stringify!($i),
        })
    };
}

macro_rules! bool {
    ($i:literal) => {
        identified!(ExprKind::Lit(Lit::Bool($i)))
    };
}

macro_rules! neg {
    ($x:pat) => {
        identified!(ExprKind::UnaryOp(identified!(UnaryOp::Neg), $x))
    };
}

macro_rules! not {
    ($x:pat) => {
        identified!(ExprKind::UnaryOp(identified!(UnaryOp::Not), $x))
    };
}

macro_rules! and {
    ($x:pat, $y:pat) => {
        identified!(ExprKind::BinaryOp(
            identified!(BinaryOp::LogicalAnd),
            $x,
            $y
        ))
    };
}

macro_rules! or {
    ($x:pat, $y:pat) => {
        identified!(ExprKind::BinaryOp(identified!(BinaryOp::LogicalOr), $x, $y))
    };
}

macro_rules! add {
    ($x:pat, $y:pat) => {
        identified!(ExprKind::BinaryOp(identified!(BinaryOp::Add), $x, $y))
    };
}

macro_rules! sub {
    ($x:pat, $y:pat) => {
        identified!(ExprKind::BinaryOp(identified!(BinaryOp::Sub), $x, $y))
    };
}

macro_rules! mul {
    ($x:pat, $y:pat) => {
        identified!(ExprKind::BinaryOp(identified!(BinaryOp::Mul), $x, $y))
    };
}

macro_rules! div {
    ($x:pat, $y:pat) => {
        identified!(ExprKind::BinaryOp(identified!(BinaryOp::Div), $x, $y))
    };
}

macro_rules! gt {
    ($x:pat, $y:pat) => {
        identified!(ExprKind::BinaryOp(identified!(BinaryOp::Gt), $x, $y))
    };
}

macro_rules! gte {
    ($x:pat, $y:pat) => {
        identified!(ExprKind::BinaryOp(identified!(BinaryOp::Gte), $x, $y))
    };
}

macro_rules! lt {
    ($x:pat, $y:pat) => {
        identified!(ExprKind::BinaryOp(identified!(BinaryOp::Lt), $x, $y))
    };
}

macro_rules! lte {
    ($x:pat, $y:pat) => {
        identified!(ExprKind::BinaryOp(identified!(BinaryOp::Lte), $x, $y))
    };
}

macro_rules! eq {
    ($x:pat, $y:pat) => {
        identified!(ExprKind::BinaryOp(identified!(BinaryOp::Eq), $x, $y))
    };
}

macro_rules! neq {
    ($x:pat, $y:pat) => {
        identified!(ExprKind::BinaryOp(identified!(BinaryOp::Neq), $x, $y))
    };
}

macro_rules! paren {
    ($x:pat) => {
        identified!(ExprKind::Paren($x))
    };
}

macro_rules! tuple {
    ($($x:pat),*) => {
        identified!(ExprKind::Tuple(&[$($x),*]))
    };
}

macro_rules! stmt {
    (let: $x:ident, $ty: pat, $exp:pat) => {
        Statement::Let(
            identified!(Ident {
                string: stringify!($x),
            }),
            Some($ty),
            $exp,
        )
    };
    (let: $x:ident, $exp:pat) => {
        Statement::Let(
            identified!(Ident {
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
        identified!(ExprKind::Call($callee, identified!(&[$($arg),*])))
    };
}

macro_rules! block {
    ($($stmts:pat),*) => {
        identified!(Block { stmts: &[$($stmts),*], last: None, .. })
    };
    ($($stmts:pat),* => $exp:pat) => {
        identified!(Block { stmts: &[$($stmts),*], last: Some($exp), .. })
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
        Item::Function(identified!($pat))
    };
}

macro_rules! type_spec {
    (name: $name: ident) => {
        identified!(TypeSpec::Name(ident!($name)))
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
        identified!(ExprKind::Block(block!{$($tok)*}))
    }
}

macro_rules! if_ {
    ($cond:pat, $then:pat) => {
        identified!(ExprKind::If($cond, $then, None))
    };
    ($cond:pat, $then:pat, $else:pat) => {
        identified!(ExprKind::If($cond, $then, Some($else)))
    };
}
