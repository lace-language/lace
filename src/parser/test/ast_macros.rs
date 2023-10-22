macro_rules! spanned {
    ($x:pat) => {
        $crate::parser::span::Spanned { value: $x, .. }
    };
}

macro_rules! int {
    ($i:literal) => {
        spanned!(ExprKind::Lit(Lit::Int(stringify!($i))))
    };
}

macro_rules! string {
    ($i:literal) => {
        spanned!(ExprKind::Lit(Lit::String($i)))
    };
}

macro_rules! ident_expr {
    ($i:ident) => {
        spanned!(ExprKind::Ident(Ident {
            string: stringify!($i),
        }))
    };
}

macro_rules! ident {
    ($i:ident) => {
        spanned!(Ident {
            string: stringify!($i),
        })
    };
}

macro_rules! bool {
    ($i:literal) => {
        spanned!(ExprKind::Lit(Lit::Bool($i)))
    };
}

macro_rules! neg {
    ($x:pat) => {
        spanned!(ExprKind::UnaryOp(spanned!(UnaryOp::Neg), $x))
    };
}

macro_rules! not {
    ($x:pat) => {
        spanned!(ExprKind::UnaryOp(spanned!(UnaryOp::Not), $x))
    };
}

macro_rules! and {
    ($x:pat, $y:pat) => {
        spanned!(ExprKind::BinaryOp(spanned!(BinaryOp::LogicalAnd), $x, $y))
    };
}

macro_rules! or {
    ($x:pat, $y:pat) => {
        spanned!(ExprKind::BinaryOp(spanned!(BinaryOp::LogicalOr), $x, $y))
    };
}

macro_rules! add {
    ($x:pat, $y:pat) => {
        spanned!(ExprKind::BinaryOp(spanned!(BinaryOp::Add), $x, $y))
    };
}

macro_rules! sub {
    ($x:pat, $y:pat) => {
        spanned!(ExprKind::BinaryOp(spanned!(BinaryOp::Sub), $x, $y))
    };
}

macro_rules! mul {
    ($x:pat, $y:pat) => {
        spanned!(ExprKind::BinaryOp(spanned!(BinaryOp::Mul), $x, $y))
    };
}

macro_rules! div {
    ($x:pat, $y:pat) => {
        spanned!(ExprKind::BinaryOp(spanned!(BinaryOp::Div), $x, $y))
    };
}

macro_rules! gt {
    ($x:pat, $y:pat) => {
        spanned!(ExprKind::BinaryOp(spanned!(BinaryOp::Gt), $x, $y))
    };
}

macro_rules! gte {
    ($x:pat, $y:pat) => {
        spanned!(ExprKind::BinaryOp(spanned!(BinaryOp::Gte), $x, $y))
    };
}

macro_rules! lt {
    ($x:pat, $y:pat) => {
        spanned!(ExprKind::BinaryOp(spanned!(BinaryOp::Lt), $x, $y))
    };
}

macro_rules! lte {
    ($x:pat, $y:pat) => {
        spanned!(ExprKind::BinaryOp(spanned!(BinaryOp::Lte), $x, $y))
    };
}

macro_rules! eq {
    ($x:pat, $y:pat) => {
        spanned!(ExprKind::BinaryOp(spanned!(BinaryOp::Eq), $x, $y))
    };
}

macro_rules! neq {
    ($x:pat, $y:pat) => {
        spanned!(ExprKind::BinaryOp(spanned!(BinaryOp::Neq), $x, $y))
    };
}

macro_rules! paren {
    ($x:pat) => {
        spanned!(ExprKind::Paren($x))
    };
}

macro_rules! tuple {
    ($($x:pat),*) => {
        spanned!(ExprKind::Tuple(&[$($x),*]))
    };
}

macro_rules! stmt {
    (let: $x:ident, $ty: pat, $exp:pat) => {
        Statement::Let(
            spanned!(Ident {
                string: stringify!($x),
            }),
            Some($ty),
            $exp,
        )
    };
    (let: $x:ident, $exp:pat) => {
        Statement::Let(
            spanned!(Ident {
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
        spanned!(ExprKind::Call($callee, spanned!(&[$($arg),*])))
    };
}

macro_rules! block {
    ($($stmts:pat),*) => {
        spanned!(Block { stmts: &[$($stmts),*], last: None, .. })
    };
    ($($stmts:pat),* => $exp:pat) => {
        spanned!(Block { stmts: &[$($stmts),*], last: Some($exp), .. })
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
        Item::Function(spanned!($pat))
    };
}

macro_rules! type_spec {
    (name: $name: ident) => {
        spanned!(TypeSpec::Name(ident!($name)))
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
        spanned!(ExprKind::Block(block!{$($tok)*}))
    }
}

macro_rules! if_ {
    ($cond:pat, $then:pat) => {
        spanned!(ExprKind::If($cond, $then, None))
    };
    ($cond:pat, $then:pat, $else:pat) => {
        spanned!(ExprKind::If($cond, $then, Some($else)))
    };
}
