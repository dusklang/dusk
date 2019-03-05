#include "TypeChecker.h"
#include "MIR.h"
#include "Diagnostics.h"

namespace {
    using namespace mir;
    struct TypeChecker {
        hir::Program const& program;
        Array<Type> types;
        Array<TypeVariable> typeVariables;
    };
}

const mir::Type DEFAULT_INT_TYPE = { mir::IntWidth::W64, true };
const mir::Type DEFAULT_FLOAT_TYPE = mir::FloatWidth::W64;

void typeCheck(hir::Program const& program) {
    TypeChecker tc { program };
    tc.types.fill(mir::Type(), program.numExpressions);
    for(auto const& level: program.expressions) {
        for(auto const& expr: level) {
            using hir::ExprKind;
            switch(expr.kind) {
                case ExprKind::IntLit: {
                    auto& ty = tc.types[expr.id.id];
                    ty = TypeKind::UnknownInt;
                    TypeVariableID id(tc.typeVariables.count());
                    tc.typeVariables.append({&ty, DEFAULT_INT_TYPE});
                    ty.id = id;
                } break;
                case ExprKind::DecLit: {
                    auto& ty = tc.types[expr.id.id];
                    ty = TypeKind::UnknownFloat;
                    TypeVariableID id(tc.typeVariables.count());
                    tc.typeVariables.append({&ty, DEFAULT_FLOAT_TYPE});
                    ty.id = id;
                } break;
                case ExprKind::BinOp: {
                    auto& ty = tc.types[expr.id.id];
                    auto& lhsTy = tc.types[expr.binOp.lhs.id];
                    auto& rhsTy = tc.types[expr.binOp.rhs.id];
                    using hir::BinOp;
                    auto bothAre = [&](TypeKind kind) -> bool {
                        return lhsTy.kind == kind && rhsTy.kind == kind;
                    };
                    enum Order {
                        None, InOrder, OutOfOrder
                    };
                    auto asymmetricallyAre = [&](TypeKind a, TypeKind b) -> Order {
                        if(lhsTy.kind == a && rhsTy.kind == b) return InOrder;
                        if(lhsTy.kind == b && rhsTy.kind == a) return OutOfOrder;
                        return None;
                    };
                    auto unifyOperands = [&](TypeKind kind) {
                        auto& lhsVar = tc.typeVariables[lhsTy.id.id];
                        auto& rhsVar = tc.typeVariables[rhsTy.id.id];
                        for(auto loc: rhsVar.locations) {
                            lhsVar.locations.append(loc);
                        }
                        rhsVar.locations.clear();
                        rhsTy.id = lhsTy.id;
                        if(kind == TypeKind::UnknownInt) {
                            lhsVar.type = DEFAULT_INT_TYPE;
                        } else if(kind == TypeKind::UnknownFloat) {
                            lhsVar.type = DEFAULT_FLOAT_TYPE;
                        } else {
                            panic("unrecognized variable type");
                        }
                    };
                    auto unify = [&](TypeKind kind) {
                        unifyOperands(kind);

                        ty = kind;
                        ty.id = lhsTy.id;
                        tc.typeVariables[lhsTy.id.id].locations.append(&ty);
                    };
                    auto unifyOperandsConcretely = [&](TypeKind concrete, TypeKind unknown) -> bool {
                        switch(asymmetricallyAre(concrete, unknown)) {
                            case None: return false;
                            case InOrder:
                                tc.typeVariables[rhsTy.id.id].type = lhsTy;
                                return true;
                            case OutOfOrder:
                                tc.typeVariables[lhsTy.id.id].type = rhsTy;
                                return true;
                        }
                    };
                    auto unifyConcretely = [&](TypeKind concrete, TypeKind unknown) -> bool {
                        unifyOperandsConcretely(concrete, unknown);
                        switch(asymmetricallyAre(concrete, unknown)) {
                            case None: return false;
                            case InOrder:
                                ty = lhsTy;
                                return true;
                            case OutOfOrder:
                                ty = rhsTy;
                                return true;
                        }
                    };
                    switch(expr.binOp.op) {
                        case BinOp::Mult: {
                            if(bothAre(TypeKind::Int) && lhsTy == rhsTy) {
                                ty = lhsTy;
                            } else if(bothAre(TypeKind::Float) && lhsTy == rhsTy) {
                                ty = lhsTy;
                            } else if(bothAre(TypeKind::UnknownInt)) {
                                unify(TypeKind::UnknownInt);
                            } else if(bothAre(TypeKind::UnknownFloat)) {
                                unify(TypeKind::UnknownFloat);
                            } else if(asymmetricallyAre(TypeKind::UnknownInt, TypeKind::UnknownFloat)) {
                                unify(TypeKind::UnknownFloat);
                            }
                            else if(unifyConcretely(TypeKind::Int, TypeKind::UnknownInt));
                            else if(unifyConcretely(TypeKind::Float, TypeKind::UnknownInt));
                            else if(unifyConcretely(TypeKind::Float, TypeKind::UnknownFloat));
                            else {
                                ERR("can't multiply values of different types!", program.file).report();
                            }
                        } break;
                        case BinOp::Div: {
                            if(bothAre(TypeKind::Int) && lhsTy == rhsTy) {
                                ty = lhsTy;
                            } else if(bothAre(TypeKind::Float) && lhsTy == rhsTy) {
                                ty = lhsTy;
                            } else if(bothAre(TypeKind::UnknownInt)) {
                                unify(TypeKind::UnknownInt);
                            } else if(bothAre(TypeKind::UnknownFloat)) {
                                unify(TypeKind::UnknownFloat);
                            } else if(asymmetricallyAre(TypeKind::UnknownInt, TypeKind::UnknownFloat)) {
                                unify(TypeKind::UnknownFloat);
                            }
                            else if(unifyConcretely(TypeKind::Int, TypeKind::UnknownInt));
                            else if(unifyConcretely(TypeKind::Float, TypeKind::UnknownInt));
                            else if(unifyConcretely(TypeKind::Float, TypeKind::UnknownFloat));
                            else {
                                ERR("can't divide values of different types!", program.file).report();
                            }
                        } break;
                        case BinOp::Mod: {
                            if(bothAre(TypeKind::Int) && lhsTy == rhsTy) {
                                ty = lhsTy;
                            } else if(bothAre(TypeKind::Float) && lhsTy == rhsTy) {
                                ty = lhsTy;
                            } else if(bothAre(TypeKind::UnknownInt)) {
                                unify(TypeKind::UnknownInt);
                            } else if(bothAre(TypeKind::UnknownFloat)) {
                                unify(TypeKind::UnknownFloat);
                            } else if(asymmetricallyAre(TypeKind::UnknownInt, TypeKind::UnknownFloat)) {
                                unify(TypeKind::UnknownFloat);
                            }
                            else if(unifyConcretely(TypeKind::Int, TypeKind::UnknownInt));
                            else if(unifyConcretely(TypeKind::Float, TypeKind::UnknownInt));
                            else if(unifyConcretely(TypeKind::Float, TypeKind::UnknownFloat));
                            else {
                                ERR("can't modulo values of different types!", program.file).report();
                            }
                        } break;
                        case BinOp::Add: {
                            if(bothAre(TypeKind::Int) && lhsTy == rhsTy) {
                                ty = lhsTy;
                            } else if(bothAre(TypeKind::Float) && lhsTy == rhsTy) {
                                ty = lhsTy;
                            } else if(bothAre(TypeKind::UnknownInt)) {
                                unify(TypeKind::UnknownInt);
                            } else if(bothAre(TypeKind::UnknownFloat)) {
                                unify(TypeKind::UnknownFloat);
                            } else if(asymmetricallyAre(TypeKind::UnknownInt, TypeKind::UnknownFloat)) {
                                unify(TypeKind::UnknownFloat);
                            }
                            else if(unifyConcretely(TypeKind::Int, TypeKind::UnknownInt));
                            else if(unifyConcretely(TypeKind::Float, TypeKind::UnknownInt));
                            else if(unifyConcretely(TypeKind::Float, TypeKind::UnknownFloat));
                            else {
                                ERR("can't add values of different types!", program.file).report();
                            }
                        } break;
                        case BinOp::Sub: {
                            if(bothAre(TypeKind::Int) && lhsTy == rhsTy) {
                                ty = lhsTy;
                            } else if(bothAre(TypeKind::Float) && lhsTy == rhsTy) {
                                ty = lhsTy;
                            } else if(bothAre(TypeKind::UnknownInt)) {
                                unify(TypeKind::UnknownInt);
                            } else if(bothAre(TypeKind::UnknownFloat)) {
                                unify(TypeKind::UnknownFloat);
                            } else if(asymmetricallyAre(TypeKind::UnknownInt, TypeKind::UnknownFloat)) {
                                unify(TypeKind::UnknownFloat);
                            }
                            else if(unifyConcretely(TypeKind::Int, TypeKind::UnknownInt));
                            else if(unifyConcretely(TypeKind::Float, TypeKind::UnknownInt));
                            else if(unifyConcretely(TypeKind::Float, TypeKind::UnknownFloat));
                            else {
                                ERR("can't subtract values of different types!", program.file).report();
                            }
                        } break;
                        case BinOp::Less: {
                            ty = TypeKind::Bool;
                            if(bothAre(TypeKind::UnknownInt)) {
                                unifyOperands(TypeKind::UnknownInt);
                            } else if(bothAre(TypeKind::UnknownFloat)) {
                                unifyOperands(TypeKind::UnknownFloat);
                            } else if(asymmetricallyAre(TypeKind::UnknownInt, TypeKind::UnknownFloat)) {
                                unifyOperands(TypeKind::UnknownFloat);
                            }
                            else if(unifyOperandsConcretely(TypeKind::Int, TypeKind::UnknownInt));
                            else if(unifyOperandsConcretely(TypeKind::Float, TypeKind::UnknownInt));
                            else if(unifyOperandsConcretely(TypeKind::Float, TypeKind::UnknownFloat));
                            else if(bothAre(TypeKind::Int) && lhsTy == rhsTy);
                            else if(bothAre(TypeKind::Float) && lhsTy == rhsTy);
                            else {
                                ERR("can't compare values of different types!", program.file).report();
                            }
                        } break;
                        case BinOp::LessOrEq: {
                            ty = TypeKind::Bool;
                            if(bothAre(TypeKind::UnknownInt)) {
                                unifyOperands(TypeKind::UnknownInt);
                            } else if(bothAre(TypeKind::UnknownFloat)) {
                                unifyOperands(TypeKind::UnknownFloat);
                            } else if(asymmetricallyAre(TypeKind::UnknownInt, TypeKind::UnknownFloat)) {
                                unifyOperands(TypeKind::UnknownFloat);
                            }
                            else if(unifyOperandsConcretely(TypeKind::Int, TypeKind::UnknownInt));
                            else if(unifyOperandsConcretely(TypeKind::Float, TypeKind::UnknownInt));
                            else if(unifyOperandsConcretely(TypeKind::Float, TypeKind::UnknownFloat));
                            else if(bothAre(TypeKind::Int) && lhsTy == rhsTy);
                            else if(bothAre(TypeKind::Float) && lhsTy == rhsTy);
                            else {
                                ERR("can't compare values of different types!", program.file).report();
                            }
                        } break;
                        case BinOp::Greater: {
                            ty = TypeKind::Bool;
                            if(bothAre(TypeKind::UnknownInt)) {
                                unifyOperands(TypeKind::UnknownInt);
                            } else if(bothAre(TypeKind::UnknownFloat)) {
                                unifyOperands(TypeKind::UnknownFloat);
                            } else if(asymmetricallyAre(TypeKind::UnknownInt, TypeKind::UnknownFloat)) {
                                unifyOperands(TypeKind::UnknownFloat);
                            }
                            else if(unifyOperandsConcretely(TypeKind::Int, TypeKind::UnknownInt));
                            else if(unifyOperandsConcretely(TypeKind::Float, TypeKind::UnknownInt));
                            else if(unifyOperandsConcretely(TypeKind::Float, TypeKind::UnknownFloat));
                            else if(bothAre(TypeKind::Int) && lhsTy == rhsTy);
                            else if(bothAre(TypeKind::Float) && lhsTy == rhsTy);
                            else {
                                ERR("can't compare values of different types!", program.file).report();
                            }
                        } break;
                        case BinOp::GreaterOrEq: {
                            ty = TypeKind::Bool;
                            if(bothAre(TypeKind::UnknownInt)) {
                                unifyOperands(TypeKind::UnknownInt);
                            } else if(bothAre(TypeKind::UnknownFloat)) {
                                unifyOperands(TypeKind::UnknownFloat);
                            } else if(asymmetricallyAre(TypeKind::UnknownInt, TypeKind::UnknownFloat)) {
                                unifyOperands(TypeKind::UnknownFloat);
                            }
                            else if(unifyOperandsConcretely(TypeKind::Int, TypeKind::UnknownInt));
                            else if(unifyOperandsConcretely(TypeKind::Float, TypeKind::UnknownInt));
                            else if(unifyOperandsConcretely(TypeKind::Float, TypeKind::UnknownFloat));
                            else if(bothAre(TypeKind::Int) && lhsTy == rhsTy);
                            else if(bothAre(TypeKind::Float) && lhsTy == rhsTy);
                            else {
                                ERR("can't compare values of different types!", program.file).report();
                            }
                        } break;
                        case BinOp::Eq: {
                            ty = TypeKind::Bool;
                            if(bothAre(TypeKind::UnknownInt)) {
                                unifyOperands(TypeKind::UnknownInt);
                            } else if(bothAre(TypeKind::UnknownFloat)) {
                                unifyOperands(TypeKind::UnknownFloat);
                            } else if(asymmetricallyAre(TypeKind::UnknownInt, TypeKind::UnknownFloat)) {
                                unifyOperands(TypeKind::UnknownFloat);
                            }
                            else if(unifyOperandsConcretely(TypeKind::Int, TypeKind::UnknownInt));
                            else if(unifyOperandsConcretely(TypeKind::Float, TypeKind::UnknownInt));
                            else if(unifyOperandsConcretely(TypeKind::Float, TypeKind::UnknownFloat));
                            else if(bothAre(TypeKind::Int) && lhsTy == rhsTy);
                            else if(bothAre(TypeKind::Float) && lhsTy == rhsTy);
                            else {
                                ERR("can't compare values of different types!", program.file).report();
                            }
                        } break;
                        case BinOp::NotEq: {
                            ty = TypeKind::Bool;
                            if(bothAre(TypeKind::UnknownInt)) {
                                unifyOperands(TypeKind::UnknownInt);
                            } else if(bothAre(TypeKind::UnknownFloat)) {
                                unifyOperands(TypeKind::UnknownFloat);
                            } else if(asymmetricallyAre(TypeKind::UnknownInt, TypeKind::UnknownFloat)) {
                                unifyOperands(TypeKind::UnknownFloat);
                            }
                            else if(unifyOperandsConcretely(TypeKind::Int, TypeKind::UnknownInt));
                            else if(unifyOperandsConcretely(TypeKind::Float, TypeKind::UnknownInt));
                            else if(unifyOperandsConcretely(TypeKind::Float, TypeKind::UnknownFloat));
                            else if(bothAre(TypeKind::Int) && lhsTy == rhsTy);
                            else if(bothAre(TypeKind::Float) && lhsTy == rhsTy);
                            else {
                                ERR("can't compare values of different types!", program.file).report();
                            }
                        } break;
                        case BinOp::BitwiseAnd: {
                            if(bothAre(TypeKind::UnknownInt)) {
                                unify(TypeKind::UnknownInt);
                            } else if(bothAre(TypeKind::Int) && lhsTy == rhsTy) {
                                ty = lhsTy;
                            }
                            else if(unifyConcretely(TypeKind::Int, TypeKind::UnknownInt));
                            else {
                                ERR("can't and values of different types!", program.file).report();
                            }
                        } break;
                        case BinOp::BitwiseOr: {
                            if(bothAre(TypeKind::UnknownInt)) {
                                unify(TypeKind::UnknownInt);
                            } else if(bothAre(TypeKind::Int) && lhsTy == rhsTy) {
                                ty = lhsTy;
                            }
                            else if(unifyConcretely(TypeKind::Int, TypeKind::UnknownInt));
                            else {
                                ERR("can't or values of different types!", program.file).report();
                            }
                        } break;
                        case BinOp::LogicalAnd: {
                            ty = TypeKind::Bool;
                            if(!bothAre(TypeKind::Bool)) {
                                ERR("can't logical and values of non-bool types!", program.file).report();
                            }
                        } break;
                        case BinOp::LogicalOr: {
                            ty = TypeKind::Bool;
                            if(!bothAre(TypeKind::Bool)) {
                                ERR("can't logical or values of non-bool types!", program.file).report();
                            }
                        } break;
                            
                        case BinOp::Assignment:
                        case BinOp::MultAssignment:
                        case BinOp::DivAssignment:
                        case BinOp::ModAssignment:
                        case BinOp::AddAssignment:
                        case BinOp::SubAssignment:
                        case BinOp::BitwiseAndAssignment:
                        case BinOp::BitwiseOrAssignment:
                            panic("Assignment is not yet supported");
                    }
                } break;
            }
        }
    }
    for(auto const& var: tc.typeVariables) {
        for(auto location: var.locations) {
            *location = var.type;
        }
    }

//    for(auto [i, level]: zip(program.expressions.indices(), program.expressions)) {
//        std::cout << "Level " << i << ":\n";
//        for(auto expr: level) {
//            std::cout << expr.id.id << ": ";
//            using hir::ExprKind;
//            switch(expr.kind) {
//                case ExprKind::IntLit: {
//                    std::cout << "integer literal";
//                } break;
//                case ExprKind::DecLit: {
//                    std::cout << "decimal literal";
//                } break;
//                case ExprKind::BinOp: {
//                    std::cout << "#" << expr.binOp.lhs.id << ' ';
//                    std::cout << symbol(expr.binOp.op);
//                    std::cout << " #" << expr.binOp.rhs.id;
//                } break;
//            }
//            std::cout << " : ";
//            Type const& ty = tc.types[expr.id.id];
//            switch(ty.kind) {
//                case mir::TypeKind::Error:
//                    panic("FOUND ERROR TYPE");
//                case mir::TypeKind::UnknownInt:
//                    panic("FOUND UNKNOWN INT");
//                case mir::TypeKind::UnknownFloat:
//                    panic("FOUND UNKNOWN FLOAT");
//                case mir::TypeKind::Int: {
//                    std::cout << (ty.intTy.isSigned ? 'i' : 'u');
//                    switch(ty.intTy.width) {
//                        case mir::IntWidth::W8: {
//                            std::cout << '8';
//                        } break;
//                        case mir::IntWidth::W16: {
//                            std::cout << "16";
//                        } break;
//                        case mir::IntWidth::W32: {
//                            std::cout << "32";
//                        } break;
//                        case mir::IntWidth::W64: {
//                            std::cout << "64";
//                        } break;
//                    }
//                } break;
//                case mir::TypeKind::Float: {
//                    std::cout << 'f';
//                    switch(ty.floatTy) {
//                        case mir::FloatWidth::W32: {
//                            std::cout << "32";
//                        } break;
//                        case mir::FloatWidth::W64: {
//                            std::cout << "64";
//                        } break;
//                    }
//                } break;
//                case mir::TypeKind::Bool: {
//                    std::cout << "bool";
//                } break;
//                case mir::TypeKind::Void: {
//                    std::cout << "void";
//                } break;
//            }
//            std::cout << '\n';
//        }
//    }
}
