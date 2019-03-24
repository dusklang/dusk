use crate::error::Error;
use crate::hir::{Program, ExprKind, BinOp};
use crate::mir::{Type, IntWidth, FloatWidth};

struct Overload {
    param_tys: Vec<Type>,
    ret_ty: Type,
}

struct OverloadConstraints {
    param_tys: Vec<usize>,
    ret_ty: usize,
    viable_overloads: Vec<Overload>,
}

enum TypeConstraint {
    IsIntLit,
    IsDecLit,
    OneOf(Vec<Type>),
    ConvertibleTo(Type),
    Is(Type),
}

const DEFAULT_INT_TY: Type = Type::Int {
    width: IntWidth::W64,
    is_signed: true,
};

const DEFAULT_FLOAT_TY: Type = Type::Float(FloatWidth::W64);

struct TypeChecker {
    /// The input HIR program
    prog: Program,
    /// The type of each expression
    types: Vec<Type>,
    /// The constraints on each expression's type
    constraints: Vec<Vec<TypeConstraint>>,
    /// The constraints on each function call or operator expression's overload choices
    overload_constraints: Vec<OverloadConstraints>,
    /// The selected overload for each function call or operator expression
    overloads: Vec<Overload>,
}

/*
pub fn type_check(prog: Program) -> Vec<Error> {
    let mut tc = TypeChecker {
        prog,
        types: Vec::new(),
        type_vars: Vec::new(),
    };
    let mut errs = Vec::new();
    tc.types.resize_with(tc.prog.num_expressions, Default::default);

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
                        kind: TypeKind::FloatVar,
                        id: Some(tc.type_vars.len()),
                    };
                    tc.type_vars.push(TypeVar::new(expr.id, DEFAULT_FLOAT_TY));
                },
                BinOp { op, lhs, rhs } => {
                    use crate::hir::BinOp::*;
                    assert_ne!(lhs, rhs);
                    assert!(lhs < tc.types.len() && rhs < tc.types.len());
                    let buf = tc.types.as_mut_ptr();
                    // This is safe because:
                    //     - we asserted above that lhs and rhs are both in range
                    //     - we asserted above that lhs are rhs are different indices
                    //     - we know that tc.types outlives the scope of this function, and 
                    //       will never grow
                    // TODO: It would be nice to refactor the code to not require this pattern
                    let (lhs_ty, rhs_ty) = unsafe { (&mut *buf.offset(lhs as isize), &*buf.offset(rhs as isize)) };
                    macro_rules! both {
                        
                    }
                    let ty = match op {
                        Mult => match (&lhs_ty.kind, &rhs_ty.kind) {
                            both!(TypeKind::Int { .. })
                            both!(TypeKind::Float(_))
                            //both!(TypeKind::IntVar) => 
                            _ => Type::default(),
                        },
                        _ => Type::default(),
                    };
                }
            }
        }
    }
    errs
}
*/