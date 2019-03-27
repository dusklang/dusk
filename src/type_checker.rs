use crate::error::Error;
use crate::hir::{Program, ExprKind, BinOp};
use crate::mir::{Type, IntWidth, FloatWidth};

struct Overload {
    param_tys: Vec<Type>,
    ret_ty: Type,
}

#[derive(Debug)]
enum Constraint {
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
    constraints: Vec<Vec<Constraint>>,
    /// The constraints on each function call or operator expression's overload choices
    overloads: Vec<Vec<Overload>>,
    /// The selected overload for each function call or operator expression
    selected_overloads: Vec<Overload>,
}

impl Overload {
    fn bin_op(lhs_ty: Type, rhs_ty: Type, ret_ty: Type) -> Self {
        Self {
            param_tys: vec![lhs_ty, rhs_ty],
            ret_ty
        }
    }
}

impl Default for Overload {
    fn default() -> Self {
        Self {
            param_tys: Vec::new(),
            ret_ty: Type::Error,
        }
    }
}

pub fn type_check(prog: Program) -> Vec<Error> {
    let mut tc = TypeChecker {
        prog,
        types: Vec::new(),
        constraints: Vec::new(),
        overloads: Vec::new(),
        selected_overloads: Vec::new(),
    };
    let mut errs = Vec::new();
    tc.types.resize_with(tc.prog.num_expressions, Default::default);
    tc.constraints.resize_with(tc.prog.num_expressions, Default::default);
    tc.overloads.resize_with(tc.prog.num_operator_exprs, Default::default);
    tc.selected_overloads.resize_with(tc.prog.num_operator_exprs, Default::default);

    for level in &tc.prog.expressions {
        for expr in level {
            use ExprKind::*;
            match expr.kind {
                IntLit => {
                    tc.constraints[expr.id].push(Constraint::IsIntLit);
                },
                DecLit => {
                    tc.constraints[expr.id].push(Constraint::IsDecLit);
                },
                BinOp { op, lhs, rhs } => {
                    use crate::hir::BinOp::*;

                    let mut overloads = match op {
                        Mult | Div | Mod | Add | Sub => vec![
                            Overload::bin_op(Type::i8(), Type::i8(), Type::i8()),
                            Overload::bin_op(Type::i16(), Type::i16(), Type::i16()),
                            Overload::bin_op(Type::i32(), Type::i32(), Type::i32()),
                            Overload::bin_op(Type::i64(), Type::i64(), Type::i64()),
                            Overload::bin_op(Type::u8(), Type::u8(), Type::u8()),
                            Overload::bin_op(Type::u16(), Type::u16(), Type::u16()),
                            Overload::bin_op(Type::u32(), Type::u32(), Type::u32()),
                            Overload::bin_op(Type::u64(), Type::u64(), Type::u64()),
                            Overload::bin_op(Type::f32(), Type::f32(), Type::f32()),
                            Overload::bin_op(Type::f64(), Type::f64(), Type::f64()),
                        ],
                        MultAssign | DivAssign | ModAssign | AddAssign | SubAssign => vec![
                            Overload::bin_op(Type::i8(), Type::i8(), Type::Void),
                            Overload::bin_op(Type::i16(), Type::i16(), Type::Void),
                            Overload::bin_op(Type::i32(), Type::i32(), Type::Void),
                            Overload::bin_op(Type::i64(), Type::i64(), Type::Void),
                            Overload::bin_op(Type::u8(), Type::u8(), Type::Void),
                            Overload::bin_op(Type::u16(), Type::u16(), Type::Void),
                            Overload::bin_op(Type::u32(), Type::u32(), Type::Void),
                            Overload::bin_op(Type::u64(), Type::u64(), Type::Void),
                            Overload::bin_op(Type::f32(), Type::f32(), Type::Void),
                            Overload::bin_op(Type::f64(), Type::f64(), Type::Void),
                        ],
                        Less | LessOrEq | Greater | GreaterOrEq => vec![
                            Overload::bin_op(Type::i8(), Type::i8(), Type::Bool),
                            Overload::bin_op(Type::i16(), Type::i16(), Type::Bool),
                            Overload::bin_op(Type::i32(), Type::i32(), Type::Bool),
                            Overload::bin_op(Type::i64(), Type::i64(), Type::Bool),
                            Overload::bin_op(Type::u8(), Type::u8(), Type::Bool),
                            Overload::bin_op(Type::u16(), Type::u16(), Type::Bool),
                            Overload::bin_op(Type::u32(), Type::u32(), Type::Bool),
                            Overload::bin_op(Type::u64(), Type::u64(), Type::Bool),
                            Overload::bin_op(Type::f32(), Type::f32(), Type::Bool),
                            Overload::bin_op(Type::f64(), Type::f64(), Type::Bool),
                        ],
                        Eq | NotEq => vec![
                            Overload::bin_op(Type::i8(), Type::i8(), Type::Bool),
                            Overload::bin_op(Type::i16(), Type::i16(), Type::Bool),
                            Overload::bin_op(Type::i32(), Type::i32(), Type::Bool),
                            Overload::bin_op(Type::i64(), Type::i64(), Type::Bool),
                            Overload::bin_op(Type::u8(), Type::u8(), Type::Bool),
                            Overload::bin_op(Type::u16(), Type::u16(), Type::Bool),
                            Overload::bin_op(Type::u32(), Type::u32(), Type::Bool),
                            Overload::bin_op(Type::u64(), Type::u64(), Type::Bool),
                            Overload::bin_op(Type::f32(), Type::f32(), Type::Bool),
                            Overload::bin_op(Type::f64(), Type::f64(), Type::Bool),
                            Overload::bin_op(Type::Bool, Type::Bool, Type::Bool),
                        ],
                        BitwiseAnd | BitwiseOr => vec![
                            Overload::bin_op(Type::i8(), Type::i8(), Type::i8()),
                            Overload::bin_op(Type::i16(), Type::i16(), Type::i16()),
                            Overload::bin_op(Type::i32(), Type::i32(), Type::i32()),
                            Overload::bin_op(Type::i64(), Type::i64(), Type::i64()),
                            Overload::bin_op(Type::u8(), Type::u8(), Type::u8()),
                            Overload::bin_op(Type::u16(), Type::u16(), Type::u16()),
                            Overload::bin_op(Type::u32(), Type::u32(), Type::u32()),
                            Overload::bin_op(Type::u64(), Type::u64(), Type::u64()),
                        ],
                        BitwiseAndAssign | BitwiseOrAssign => vec![
                            Overload::bin_op(Type::i8(), Type::i8(), Type::Void),
                            Overload::bin_op(Type::i16(), Type::i16(), Type::Void),
                            Overload::bin_op(Type::i32(), Type::i32(), Type::Void),
                            Overload::bin_op(Type::i64(), Type::i64(), Type::Void),
                            Overload::bin_op(Type::u8(), Type::u8(), Type::Void),
                            Overload::bin_op(Type::u16(), Type::u16(), Type::Void),
                            Overload::bin_op(Type::u32(), Type::u32(), Type::Void),
                            Overload::bin_op(Type::u64(), Type::u64(), Type::Void),
                        ],
                        LogicalAnd | LogicalOr => vec![Overload::bin_op(Type::Bool, Type::Bool, Type::Bool)],
                        Assign => vec![
                            Overload::bin_op(Type::i8(), Type::i8(), Type::Void),
                            Overload::bin_op(Type::i16(), Type::i16(), Type::Void),
                            Overload::bin_op(Type::i32(), Type::i32(), Type::Void),
                            Overload::bin_op(Type::i64(), Type::i64(), Type::Void),
                            Overload::bin_op(Type::u8(), Type::u8(), Type::Void),
                            Overload::bin_op(Type::u16(), Type::u16(), Type::Void),
                            Overload::bin_op(Type::u32(), Type::u32(), Type::Void),
                            Overload::bin_op(Type::u64(), Type::u64(), Type::Void),
                            Overload::bin_op(Type::f32(), Type::f32(), Type::Void),
                            Overload::bin_op(Type::f64(), Type::f64(), Type::Void),
                            Overload::bin_op(Type::Bool, Type::Bool, Type::Void),
                        ],
                    };

                    overloads.retain(|overload| {
                        for (constraints, ty) in [&tc.constraints[lhs], &tc.constraints[rhs]].iter().zip(&overload.param_tys) {
                            for constraint in *constraints {
                                match (constraint, ty) {
                                    (Constraint::IsIntLit, x) if !x.expressible_by_int_lit()    => return false,
                                    (Constraint::IsDecLit, x) if !x.expressible_by_dec_lit()    => return false,
                                    (Constraint::OneOf(tys), x) if !tys.contains(x)             => return false,
                                    (Constraint::ConvertibleTo(ty), x) if !x.convertible_to(ty) => return false,
                                    (Constraint::Is(ty), x) if ty != x                          => return false,
                                    _ => {},
                                }
                            }
                        }
                        true
                    });

                    tc.constraints[expr.id].push(
                        Constraint::OneOf(
                            overloads.iter().map(|overload| overload.ret_ty.clone()).collect()
                        )
                    );
                }
            }
        }
    }
    println!("Constraints: {:#?}", tc.constraints);
    errs
}