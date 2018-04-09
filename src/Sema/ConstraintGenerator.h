//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <iostream>
#include <stack>

#include "AST/ASTVisitor.h"
#include "Constraint.h"
#include "llvm/ADT/Optional.h"

using llvm::Optional;
using llvm::None;

class ConstraintGenerator: public ASTVisitor<ConstraintGenerator> {
private:
    // FIXME: Name lookup will be super slow; figure out how to hash function overloads.
    std::vector<std::vector<std::shared_ptr<Decl>>> declLists;
    // The return type of the current computed decl, or None if we are not currently in one.
    // FIXME: switch to a stack once nested computed decls are a thing.
    std::stack<Type> returnTypeStack;
    int typeVariableCount = 0;

    void constrain(Constraint constraint) { constraints.push_back(constraint); }

    Type newTypeVariable() {
        return Type::TypeVariable(typeVariableCount++);
    }
    Type newIntTypeVariable() {
        return Type::TypeVariable(typeVariableCount++, Type::Variable::Integer);
    }
    Type newDecimalTypeVariable() {
        return Type::TypeVariable(typeVariableCount++, Type::Variable::Decimal);
    }

    template<typename Node>
    void reportError(std::string message, std::shared_ptr<Node> node) {
        std::cout << "CONSTRAINT-GENERATION ERROR: " << message << '\n';
        std::cout << "Offending area: " << node->range.getSubstring() << "\n\n";
        // TODO: Support multiple errors per file.
        exit(1);
    }

    template<typename Node>
    void reportWarning(std::string message, std::shared_ptr<Node> node) {
        std::cout << "CONSTRAINT-GENERATION WARNING: " << message << '\n';
        std::cout << "Offending area: " << node->range.getSubstring() << "\n\n";
    }
public:
    ConstraintGenerator() {
        declLists.push_back(std::vector<std::shared_ptr<Decl>>());
    }

    void dumpConstraints(std::ostream& stream) {
        for(auto& constraint: constraints) {
            constraint.dump(stream);
            stream << '\n';
        }
    }

    std::vector<Constraint> constraints;

    void visitDecl(std::shared_ptr<Decl> decl);
    void visitScope(std::shared_ptr<Scope> scope);
    void visitArgument(std::shared_ptr<Argument> argument) {}
    void visitIntegerLiteralExpr(std::shared_ptr<IntegerLiteralExpr> expr);
    void visitDecimalLiteralExpr(std::shared_ptr<DecimalLiteralExpr> expr);
    void visitBooleanLiteralExpr(std::shared_ptr<BooleanLiteralExpr> expr);
    void visitCharLiteralExpr(std::shared_ptr<CharLiteralExpr> expr);
    void visitStringLiteralExpr(std::shared_ptr<StringLiteralExpr> expr);
    void visitDeclRefExpr(std::shared_ptr<DeclRefExpr> expr);

    void visitReturnStmt(std::shared_ptr<ReturnStmt> stmt);
    void visitAssignmentStmt(std::shared_ptr<AssignmentStmt> stmt);
    void visitIfStmt(std::shared_ptr<IfStmt> stmt);
    void visitWhileStmt(std::shared_ptr<WhileStmt> stmt);
};
