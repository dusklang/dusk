//  Copyright © 2019 Zach Wolfe. All rights reserved.

#include <utility>
#include "TypeChecker.h"
#include "General/Array.h"

#include "mpark/patterns.hpp"

using namespace mpark::patterns;

#define ERR(msg) Diagnostic(Diagnostic::Error, file, msg)
#define WARN(msg) Diagnostic(Diagnostic::Warning, file, msg)

void TypeChecker::visitDeclPrototype(Decl* decl) {
    switch(decl->protoState) {
        case Decl::Unresolved:
            decl->protoState = Decl::Resolving;
            break;
        case Decl::Resolving:
            reportDiag(ERR("cyclic decl prototype dependency")
                       .primaryRange(decl->name.range));
            break;
        case Decl::Resolved:
            return;
    }

    if(decl->type != ErrorTy()) {
        visitType(&decl->type);
    }
    for(auto param: decl->paramList) {
        visitType(&param->type);
    }

    if(decl->hasDefinition()) {
        if(decl->isComputed()) {
            // Add void type to computed declaration if it doesn't have one.
            if(decl->type == ErrorTy()) {
                decl->type = VoidTy();
            } else {
                visitType(&decl->type);
            }
        } else {
            visitExpr(decl->expression());
            if(decl->type == ErrorTy()) {
                decl->type = decl->expression()->type;
            } else {
                if(!decl->expression()->type.isConvertibleTo(decl->type)) {
                    reportDiag(ERR("cannot initialize declaration of type `" +
                                   decl->type.name() + "` with value of type `" +
                                   decl->expression()->type.name())
                               .primaryRange(decl->expression()->totalRange()));
                }
            }
            if(decl->type == VoidTy() || decl->type == NeverTy()) {
                reportDiag(ERR("stored declarations can not have type `" + decl->type.name() + "`")
                           .primaryRange(decl->type.range));
            }
        }
    } else {
        if(decl->type == ErrorTy()) {
            reportDiag(ERR("standalone decl prototype needs an explicit type").primaryRange(decl->protoRange()));
        }
    }
    decl->protoState = Decl::Resolved;
}
void TypeChecker::visitTopLevel(Array<ASTNode*> nodes) {
    for(auto node: nodes) {
        if(auto decl = dynamic_cast<Decl*>(node)) {
            declLists.last()->append(decl);
        } else if(auto decl = dynamic_cast<StructDecl*>(node)) {
            for(auto other: structs) {
                if(decl->name == other->name) {
                    reportDiag(ERR("re-declaration of structure `" + decl->name.getText() + "`")
                               .range(other->structRange + other->name.range, "previous declaration here")
                               .primaryRange(decl->structRange + decl->name.range));
                }
            }
            structs.append(decl);
        }
    }
    for(auto node: nodes) {
        visit(node);
    }
}
void TypeChecker::visitType(Type *type, bool shouldResolveStructDecls) {
    match(type->data)(
        pattern(as<StructTy>(arg)) = [&](auto& structTy) {
            if(!structTy.decl) {
                for(auto decl: structs) {
                    if(structTy.name == decl->name) {
                        structTy.decl = decl;
                        break;
                    }
                }
                if(!structTy.decl) {
                    reportDiag(ERR("reference to undeclared type `" + structTy.name.getText() + "`")
                               .primaryRange(type->range));
                }
            }
            if(shouldResolveStructDecls) {
                visitStructDecl(structTy.decl);
            }
        },
        pattern(as<PointerTy>(ds(arg))) = [&](auto pointedTy) {
            visitType(pointedTy, false);
        },
        pattern(_) = [] {}
    );
}
void TypeChecker::visitDecl(Decl* decl) {
    visitDeclPrototype(decl);
    if(decl->hasDefinition()) {
        declLists.append(Array<Decl*>());
        for(auto param: decl->paramList) {
            declLists.last()->append(param);
        }
        if(decl->isComputed()) {
            returnTypeStack.append(decl->type);
            auto body = decl->body();
            visitScope(body);
            auto terminalExpr = body->terminalExpr;
            auto returnType = decl->type;
            if(returnType != VoidTy()) {
                if(terminalExpr && !terminalExpr->type.isConvertibleTo(returnType)) {
                    reportDiag(ERR("cannot accept terminal expression of type `" + terminalExpr->type.name() + "` in computed declaration of type `" + returnType.name() + "`")
                               .primaryRange(terminalExpr->totalRange())
                               .range(returnType.range, "declared as `" + returnType.name() + "` here"));
                } else if(!terminalExpr) {
                    reportDiag(ERR("non-void computed declaration `" + decl->name.getText() + "` must return a value")
                               .primaryRange(returnType.range, "`" + decl->name.getText() + "` type declared here"));
                }
            } else {
                if(terminalExpr && terminalExpr->type != NeverTy()) {
                    reportDiag(ERR("unused terminal expression of type `" + terminalExpr->type.name() + ", try writing \"do *expression*\" or changing the return type of `" + decl->name.getText() + "`")
                               .primaryRange(terminalExpr->totalRange()));
                }
            }

            returnTypeStack.removeLast();
        }
        // End the scope for referencing parameters.
        declLists.removeLast();
    } else {
        if(!decl->isExtern()) {
            reportDiag(ERR("non-`extern` declaration needs a definition").primaryRange(decl->protoRange()));
        }
    }
}

void TypeChecker::visitStructDecl(StructDecl* decl) {
    switch(decl->state) {
        case StructDecl::Unresolved:
            decl->state = StructDecl::Resolving;
            break;
        case StructDecl::Resolving:
            reportDiag(ERR("cyclic struct dependency")
                       .primaryRange(decl->name.range));
            break;
        case StructDecl::Resolved:
            return;
    }
    for(auto field: decl->fields) {
        visitType(&field->type);
    }
    decl->state = StructDecl::Resolved;
}
void TypeChecker::visitScope(Scope* scope) {
    // Start a new namespace for declarations inside the scope.
    declLists.append(Array<Decl*>());

    std::optional<size_t> indexToRemoveFrom;
    for(size_t i: scope->nodes.indices()) {
        auto node = scope->nodes[i];
        if(auto expr = dynamic_cast<Expr*>(node)) {
            visitExpr(expr);
            if(expr->type == NeverTy()) {
                scope->terminalExpr = expr;
                indexToRemoveFrom = i + 1;
                continue;
            } else if(!indexToRemoveFrom && *scope->nodes.last() == node && expr->type != VoidTy()) {
                scope->terminalExpr = expr;
            } else if(expr->type != VoidTy() && expr->type != NeverTy()) {
                reportDiag(ERR("unused expression, try writing \"do *expression*\"").primaryRange(expr->totalRange()));
            }
        } else if(auto decl = dynamic_cast<Decl*>(node)) {
            if(decl->isComputed()) {
                declLists.last()->append(decl);
            }
            visitDecl(decl);
            if(decl->isStored()) {
                declLists.last()->append(decl);
            }
        } else if(auto decl = dynamic_cast<StructDecl*>(node)) {
            reportDiag(ERR("struct declarations may not yet be nested").primaryRange(decl->structRange + decl->name.range));
        } else {
            visit(node);
        }
    }
    // If there's a `never` expression in the middle of a scope, we typecheck the rest of the scope, but also remove it so
    // code generation won't have to worry about it. Also, report a warning.
    if(indexToRemoveFrom) {
        if(*indexToRemoveFrom != scope->nodes.count()) {
            auto range = scope->nodes[*indexToRemoveFrom]->totalRange();
            for(size_t i: makeRange(*indexToRemoveFrom, scope->nodes.count())) {
                range += scope->nodes[i]->totalRange();
            }
            reportDiag(WARN("code will never be executed")
                       .primaryRange(range));
        }
        scope->nodes.removeRange(makeRange(*indexToRemoveFrom));
    }

    // End the new namespace.
    declLists.removeLast();
}
void TypeChecker::visitIntegerLiteralExpr(IntegerLiteralExpr* expr) {
    expr->type = IntTy::I32();
}
void TypeChecker::visitDecimalLiteralExpr(DecimalLiteralExpr* expr) {
    expr->type = DoubleTy();
}
void TypeChecker::visitBooleanLiteralExpr(BooleanLiteralExpr* expr) {
    expr->type = BoolTy();
}
void TypeChecker::visitCharLiteralExpr(CharLiteralExpr* expr) {
    expr->type = IntTy::I8();
}
void TypeChecker::visitStringLiteralExpr(StringLiteralExpr* expr) {
    expr->type = PointerTy::get(IntTy::I8());
}
void TypeChecker::visitPreOpExpr(PreOpExpr* expr) {
    visitExpr(expr->operand);
    // FIXME: Ensure the given operator is valid for the operand type.
    switch(expr->op) {
        case PreOp::Deref:
            visitType(expr->operand->type.pointeeType());
            expr->type = *expr->operand->type.pointeeType();
            break;
        case PreOp::AddrOf:
            expr->type = PointerTy::get(expr->operand->type);
            break;
        default:
            expr->type = expr->operand->type;
            break;
    }
}
void TypeChecker::visitBinOpExpr(BinOpExpr* expr) {
    visitExpr(expr->lhs);
    visitExpr(expr->rhs);
    bool pointerAdd = false;
    expr->type = expr->lhs->type;
    if(expr->lhs->type != expr->rhs->type) {
        // Allow +, +=, - and -= between pointers and integers.
        match(expr->lhs->type.data, expr->rhs->type.data)(
            pattern(as<PointerTy>(_), as<IntTy>(_)) = [&] {
                WHEN(expr->op == BinOp::Add || expr->op == BinOp::AddAssignment)
                {
                    pointerAdd = true;
                };
            },
            pattern(_, _) = [&] {
                reportDiag(ERR("mismatched types `" + expr->lhs->type.name() + "` and `" + expr->rhs->type.name() + "` in binary operator expression")
                           .primaryRange(expr->lhs->totalRange())
                           .primaryRange(expr->rhs->totalRange()));
            }
        );
    }
    // FIXME: Check that operators are valid on the operand types.
    switch(expr->op) {
        case BinOp::Add:        break;
        case BinOp::Sub:        break;
        case BinOp::Mult:       break;
        case BinOp::Div:        break;
        case BinOp::Mod:        break;
        case BinOp::BitwiseOr:  break;
        case BinOp::Or:         break;
        case BinOp::BitwiseAnd: break;
        case BinOp::And:        break;
        case BinOp::Equal:
        case BinOp::NotEqual:
        case BinOp::LessThan:
        case BinOp::LessThanOrEqual:
        case BinOp::GreaterThan:
        case BinOp::GreaterThanOrEqual:
            expr->type = BoolTy();
            break;
        case BinOp::Assignment:
        case BinOp::AddAssignment:
        case BinOp::SubAssignment:
        case BinOp::MultAssignment:
        case BinOp::DivAssignment:
        case BinOp::ModAssignment:
        case BinOp::AndAssignment:
        case BinOp::OrAssignment: {
            if(!expr->lhs->isMutable()) {
                reportDiag(ERR("cannot assign to expression").primaryRange(expr->lhs->totalRange(), "expression is immutable"));
            }
            expr->type = VoidTy();
        }
    }
}
void TypeChecker::visitCastExpr(CastExpr* expr) {
    visitExpr(expr->operand);
    visitType(&expr->destType);
    expr->type = expr->destType;
}
void TypeChecker::visitDeclRefExpr(DeclRefExpr* expr) {
    // Type-check arguments.
    for(auto arg: expr->argList) {
        visitExpr(arg);
    }

    // Find the prototype to reference.
    Array<Decl*> nameMatches;
    for(auto& declList: reverse(declLists)) {
        for(auto decl: declList) {
            if(decl->name != expr->name) continue;
            nameMatches.append(decl);
            if(decl->paramList.count() != expr->argList.count()) continue;
            visitDeclPrototype(decl);
            for(auto [param, arg]: zip(decl->paramList, expr->argList)) {
                if(!arg->type.isConvertibleTo(param->type)) goto failedToFindMatchInCurrentList;
            }
            // We must have succeeded! Add the decl's prototype and type to the declRefExpr and return.
            expr->decl = decl;
            expr->type = decl->type;
            return;

        failedToFindMatchInCurrentList: continue;
        }
    }
    // We must have failed.
    auto errorMessage = ERR("invalid reference to identifier `" + expr->name.getText() + "`")
        .primaryRange(expr->name.range);
    if(!nameMatches.isEmpty()) {
        for(auto match: nameMatches) {
            errorMessage.range(match->name.range, "differs only in parameter types");
        }
    }
    reportDiag(errorMessage);
}
void TypeChecker::visitMemberRefExpr(MemberRefExpr* expr) {
    visitExpr(expr->root);

    if_let(pattern(as<PointerTy>(_)) = expr->root->type.data) = [&] {
        reportDiag(ERR("cannot yet reference a member of a pointer type")
                       .primaryRange(expr->dotRange + expr->name.range)
                       .range(expr->root->totalRange(), "expression is of type `" + expr->root->type.name() + "`")
                );
    };
    match(expr->root->type.data)(
        pattern(as<StructTy>(arg)) = [&](auto structTy) {
            std::optional<size_t> fieldDecl = std::nullopt;

            auto& fields = structTy.decl->fields;
            for(size_t field: fields.indices()) {
                if(expr->name == fields[field]->name) {
                    fieldDecl = field;
                }
            }
            if(!fieldDecl) {
                reportDiag(ERR("struct `" + structTy.name.getText() + "` has no member `" + expr->name.getText() + "`")
                               .primaryRange(expr->dotRange + expr->name.range));
            }
            expr->declIndex = *fieldDecl;
            expr->type = fields[*fieldDecl]->type;
        },
        pattern(_) = [&] {
            reportDiag(ERR("cannot reference member `" + expr->name.getText() + "` of non-struct type `" + expr->type.name() + "`")
                       .primaryRange(expr->dotRange + expr->name.range));
        }
    );
}
void TypeChecker::visitReturnExpr(ReturnExpr* expr) {
    expr->type = NeverTy();
    if(expr->value) {
        visitExpr(expr->value);
    }
    // Handle returning value in a void computed decl.
    if(*returnTypeStack.last() == VoidTy()) {
        if(expr->value) {
            reportDiag(ERR("cannot return value from void computed declaration")
                       .primaryRange(expr->value->totalRange(), "delet this"));
        } else {
            return;
        }
    }

    // Handle returning no value in a non-void computed decl.
    if(!expr->value) {
        reportDiag(ERR("computed declaration must return a value").primaryRange(expr->returnRange, "add a return value"));
    }

    // Handle returning a value of a type not equal to the computed decl's type.
    if(expr->value->type != *returnTypeStack.last()) {
        reportDiag(ERR("cannot return value of type `" + expr->value->type.name() + "` from computed declaration of type `" + returnTypeStack.last()->name())
                   .primaryRange(expr->value->totalRange()));
    }
}
void TypeChecker::visitIfExpr(IfExpr* expr) {
    visitExpr(expr->condition);
    if(expr->condition->type != BoolTy()) {
        reportDiag(ERR("conditions in if expressions must be of type `bool`")
                   .primaryRange(expr->condition->totalRange(), "expression is of type `" + expr->condition->type.name() + "`"));
    }
    visitScope(expr->thenScope);
    Type thenTy = expr->thenScope->terminalType();
    Type elseTy = VoidTy();
    if(auto scope = expr->elseScope) {
        visitScope(scope);
        elseTy = scope->terminalType();
    }
    if(thenTy.isConvertibleTo(elseTy)) {
        expr->type = elseTy;
    } else if(elseTy.isConvertibleTo(thenTy)) {
        expr->type = thenTy;
    } else {
        // FIXME: We should have a way better error message here.
        reportDiag(ERR("if expression branches have incompatible types").primaryRange(expr->ifRange));
    }
}
void TypeChecker::visitWhileExpr(WhileExpr* expr) {
    expr->type = VoidTy();
    visitExpr(expr->condition);
    if(expr->condition->type != BoolTy()) {
        reportDiag(ERR("conditions in while expressions must be of type `bool`")
                   .primaryRange(expr->condition->totalRange(), "expression is of type `" + expr->condition->type.name() + "`"));
    }
    visitScope(expr->thenScope);
}
void TypeChecker::visitDoExpr(DoExpr* expr) {
    expr->type = match(expr->value)(
        pattern(as<Expr*>(arg)) = [&](auto innerExpr) -> Type {
            visitExpr(innerExpr);
            if(innerExpr->type == VoidTy() || innerExpr->type == NeverTy()) {
                reportDiag(WARN("use of do expression on a non-value expression doesn't do anything")
                           .primaryRange(expr->doRange)
                           .range(innerExpr->totalRange(), "expression is of type `" + innerExpr->type.name() + "`"));
            }
            return VoidTy();
        },
        pattern(as<Scope*>(arg)) = [&](auto scope) -> Type {
            visitScope(scope);
            if(scope->terminalExpr) {
                return scope->terminalExpr->type;
            } else {
                return VoidTy();
            }
        }
    );
}
