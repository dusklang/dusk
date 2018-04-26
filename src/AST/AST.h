//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <string>
#include <vector>
#include <variant>
#include <map>
#include <optional>
#include "llvm/IR/Value.h"
#include "General/SourceLoc.h"

struct Expr;

enum class NodeKind {
    #define AST_NODE(name) name,
    #include "ASTNodes.def"
    NUM_NODES
};

#define AST_NODE_CTOR(name, args...) name(SourceRange range, args) : ASTNode(NodeKind::name, range)
#define AST_NODE_CTOR_NOARG(name) name(SourceRange range) : ASTNode(NodeKind::name, range)

// Abstract class from which each node in the AST inherits.
struct ASTNode {
    NodeKind kind;
    SourceRange range;
    ASTNode(NodeKind kind, SourceRange range) : kind(kind), range(range) {}
    virtual ~ASTNode() {}
};

struct Type final {
    struct IntegerTy {
        int bitWidth;
        bool isSigned;

        bool operator==(IntegerTy other) { return bitWidth == other.bitWidth && isSigned == other.isSigned; }
    };
    struct PointerTy {
        std::shared_ptr<Type> pointedTy;
    };
    struct VoidTy {};
    struct BoolTy {};
    struct FloatTy {};
    struct DoubleTy {};
    struct ErrorTy {};
    struct Variable {
        enum Kind {
            General, Integer, Decimal
        };
        int num;
        Kind kind;

        bool operator==(Variable const& other) const {
            return num == other.num && kind == other.kind;
        };
    };

    typedef std::variant<IntegerTy, PointerTy, Variable, VoidTy, BoolTy, FloatTy, DoubleTy, ErrorTy> DataType;

    DataType data;
    std::optional<SourceRange> sourceRange;
private:
    struct EqualityVisitor {
        bool operator()(IntegerTy lhs, IntegerTy rhs) const { return lhs == rhs; }
        bool operator()(PointerTy lhs, PointerTy rhs) const {
            return *lhs.pointedTy == *rhs.pointedTy;
        }
        bool operator()(Variable lhs, Variable rhs) const { return lhs == rhs; }
        bool operator()(VoidTy, VoidTy) const { return true; }
        bool operator()(BoolTy, BoolTy) const { return true; }
        bool operator()(FloatTy, FloatTy) const { return true; }
        bool operator()(DoubleTy, DoubleTy) const { return true; }
        bool operator()(ErrorTy, ErrorTy) const { return true; }

        template<typename T, typename U>
        bool operator()(T, U) const { return false; }
    };
    struct SubstitutionVisitor {
        std::map<int, Type> const& solution;
        SubstitutionVisitor(std::map<int, Type> const& solution) : solution(solution) {}

        Type operator()(Type::Variable var) const {
            for(auto const& solution: this->solution) {
                if(solution.first == var.num) {
                    return solution.second;
                }
            }
            return Type::TypeVariable(var.num, var.kind);
        }

        template<typename T>
        Type operator()(T ty) const { return Type(ty); }
    };
public:
    Type(DataType data,
         std::optional<SourceRange> sourceRange = std::nullopt)
         : data(data), sourceRange(sourceRange) {}
    static Type Integer(int bitWidth, bool isSigned, std::optional<SourceRange> sourceRange = std::nullopt) {
        return Type(IntegerTy { bitWidth, isSigned }, sourceRange);
    }
    static Type I8(std::optional<SourceRange> sourceRange = std::nullopt) {
        return Integer(8, true, sourceRange);
    }
    static Type I16(std::optional<SourceRange> sourceRange = std::nullopt) {
        return Integer(16, true, sourceRange);
    }
    static Type I32(std::optional<SourceRange> sourceRange = std::nullopt) {
        return Integer(32, true, sourceRange);
    }
    static Type I64(std::optional<SourceRange> sourceRange = std::nullopt) {
        return Integer(64, true, sourceRange);
    }
    static Type U8(std::optional<SourceRange> sourceRange = std::nullopt) {
        return Integer(8, false, sourceRange);
    }
    static Type U16(std::optional<SourceRange> sourceRange = std::nullopt) {
        return Integer(16, false, sourceRange);
    }
    static Type U32(std::optional<SourceRange> sourceRange = std::nullopt) {
        return Integer(32, false, sourceRange);
    }
    static Type U64(std::optional<SourceRange> sourceRange = std::nullopt) {
        return Integer(64, false, sourceRange);
    }
    static Type Error() { return Type(ErrorTy()); }

    static Type Pointer(Type pointedTy, std::optional<SourceRange> sourceRange = std::nullopt) {
        auto pointed = std::make_shared<Type>(pointedTy);
        return Type(PointerTy { pointed }, sourceRange);
    }
    static Type TypeVariable(int number, Variable::Kind kind = Variable::General) {
        return Type(Variable { number });
    }
    static Type Void(std::optional<SourceRange> sourceRange = std::nullopt) {
        return Type(VoidTy(), sourceRange);
    }
    static Type Bool(std::optional<SourceRange> sourceRange = std::nullopt) {
        return Type(BoolTy(), sourceRange);
    }
    static Type Float(std::optional<SourceRange> sourceRange = std::nullopt) {
        return Type(FloatTy(), sourceRange);
    }
    static Type Double(std::optional<SourceRange> sourceRange = std::nullopt) {
        return Type(DoubleTy(), sourceRange);
    }

    ~Type() = default;
    Type(Type const& other) = default;
    Type& operator=(Type const& other) = default;

    bool operator==(Type other) const {
        return std::visit(EqualityVisitor(), data, other.data);
    }
    bool operator!=(Type other) const { return !(*this == other); }

    std::string name() const;

    Type substituting(std::map<int, Type> const& solution) const {
        return std::visit(SubstitutionVisitor{solution}, this->data);
    }
    void substitute(std::map<int, Type> const& solution) {
        *this = substituting(solution);
    }
};

struct Argument final : public ASTNode {
    std::shared_ptr<Expr> value;

    AST_NODE_CTOR(Argument, std::shared_ptr<Expr> value), value(value) {}
};

// A scope node represents a collection of other nodes.
struct Scope final : public ASTNode {
    std::vector<std::shared_ptr<ASTNode>> nodes;

    AST_NODE_CTOR(Scope, std::vector<std::shared_ptr<ASTNode>> const& nodes), nodes(nodes) {}
};
