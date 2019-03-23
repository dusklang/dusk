use crate::error::Error;
use crate::hir::{Program, ExprKind};
use crate::mir::{Type, TypeVar, TypeKind, IntWidth, FloatWidth};

const DEFAULT_INT_TY: Type = Type {
    kind: TypeKind::Int {
        width: IntWidth::W64,
        is_signed: true,
    },
    id: None,
};

const DEFAULT_FLOAT_TY: Type = Type {
    kind: TypeKind::Float(FloatWidth::W64),
    id: None,
};

struct TypeChecker {
    prog: Program,
    types: Vec<Type>,
    type_vars: Vec<TypeVar>,
}

pub fn type_check(prog: Program) -> Vec<Error> {
    let mut tc = TypeChecker {
        prog,
        types: Vec::new(),
        type_vars: Vec::new(),
    };
    let mut errs = Vec::new();
    tc.type_vars.resize_with(self.prog.num_expressions, Default::default);

    for level in tc.prog.expressions {
        for expr in level {
            use ExprKind::*;
            match expr.kind {
                IntLit => {
                    tc.types[expr.id] = Type {
                        kind: TypeKind::IntVar,
                        id: Some(tc.type_vars.len()),
                    };
                    tc.type_vars.push(TypeVar::new(expr.id, DEFAULT_INT_TY));
                },
                DecLit => {
                    tc.types[expr.id] = Type {
                        kind: TypeKind::Float,
                        id: Some(tc.type_vars.len()),
                    };
                    tc.type_vars.push(TypeVar::new(expr.id, DEFAULT_FLOAT_TY));
                },
                BinOp { op, lhs, rhs } => {
                    use BinOp::*;
                    let (lhsTy, rhsTy) = 
                    match op {
                        Mult => match (&tc.types)
                    }
                }
            }
        }
    }
    errs
}
