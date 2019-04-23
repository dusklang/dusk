use crate::error::Error;
use crate::hir::{Program, ItemKind, BinOp};
use crate::mir::{Type, IntWidth, FloatWidth};

struct Overload {
    param_tys: Vec<Type>,
    ret_ty: Type,
}

#[derive(Debug)]
enum LiteralType { Int, Dec }

#[derive(Debug, Default)]
struct ConstraintList {
    literal: Option<LiteralType>,
    one_of: Vec<Type>,
}

const DEFAULT_INT_TY: Type = Type::Int {
    width: IntWidth::W64,
    is_signed: true,
};

const DEFAULT_FLOAT_TY: Type = Type::Float(FloatWidth::W64);

struct TypeChecker {
    /// The input HIR program
    prog: Program,
    /// The type of each item
    types: Vec<Type>,
    /// The constraints on each items's type
    constraints: Vec<ConstraintList>,
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
    tc.types.resize_with(tc.prog.num_items, Default::default);
    tc.constraints.resize_with(tc.prog.num_items, Default::default);
    tc.overloads.resize_with(tc.prog.num_operator_exprs, Default::default);
    tc.selected_overloads.resize_with(tc.prog.num_operator_exprs, Default::default);

    for level in tc.prog.items.levels() {
        for item in tc.prog.items.get_level(level) {
            use ItemKind::*;
            match &item.kind {
                IntLit => tc.constraints[item.id].literal = Some(LiteralType::Int),
                DecLit => tc.constraints[item.id].literal = Some(LiteralType::Dec),
                &StoredDecl { ref name, root_expr } => {
                    let constraints = &tc.constraints[root_expr];
                    let mut guess = match constraints.literal {
                        Some(LiteralType::Int) => Type::i32(),
                        Some(LiteralType::Dec) => Type::f64(),
                        None => {
                            assert!(!constraints.one_of.is_empty());
                            constraints.one_of[0].clone()
                        }
                    };
                    tc.constraints[item.id].one_of = vec![guess];
                }
                &BinOp { op, lhs, rhs } => {
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

                    // Filter overloads that don't match the constraints of the parameters.
                    overloads.retain(|overload| {
                        // For each parameter:
                        for (constraints, ty) in [&tc.constraints[lhs], &tc.constraints[rhs]].iter().zip(&overload.param_tys) {
                            // Verify all the constraints match the parameter type.
                            match constraints.literal {
                                Some(LiteralType::Int) => if !ty.expressible_by_int_lit() { return false },
                                Some(LiteralType::Dec) => if !ty.expressible_by_dec_lit() { return false },
                                None => if !constraints.one_of.contains(ty) { return false },
                            }
                        }
                        true
                    });

                    tc.constraints[item.id].one_of = overloads.iter()
                        .map(|overload| overload.ret_ty.clone())
                        .collect();
                }
            }
        }
    }
    println!("Constraints: {:#?}", tc.constraints);
    errs
}