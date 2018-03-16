//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <string>
#include <vector>
#include "llvm/ADT/Optional.h"
#include "llvm/IR/Value.h"
#include "General/SourceLoc.h"
#include "boost/variant.hpp"

using llvm::Optional;
using llvm::None;

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
    struct IntProperties {
        int bitWidth;
        bool isSigned;

        bool operator==(IntProperties other) { return bitWidth == other.bitWidth && isSigned == other.isSigned; }
    };
    struct PointerTy {
        std::shared_ptr<Type> pointedTy;
    };
    struct VoidTy {};
    struct BoolTy {};
    struct FloatTy {};
    struct DoubleTy {};
    struct ErrorTy {};

    typedef boost::variant<IntProperties, PointerTy, int, VoidTy, BoolTy, FloatTy, DoubleTy, ErrorTy> DataType;

    DataType data;
    Optional<SourceRange> sourceRange;
private:
    struct EqualityVisitor: public boost::static_visitor<bool> {
        bool operator()(IntProperties lhs, IntProperties rhs) const { return lhs == rhs; }
        bool operator()(PointerTy lhs, PointerTy rhs) const {
            return *lhs.pointedTy == *rhs.pointedTy;
        }
        bool operator()(std::string lhs, std::string rhs) const { return lhs == rhs; }
        bool operator()(VoidTy, VoidTy) const { return true; }
        bool operator()(BoolTy, BoolTy) const { return true; }
        bool operator()(FloatTy, FloatTy) const { return true; }
        bool operator()(DoubleTy, DoubleTy) const { return true; }
        bool operator()(ErrorTy, ErrorTy) const { return true; }

        template<typename T, typename U>
        bool operator()(T, U) const { return false; }
    };
public:
    Type(DataType data,
         Optional<SourceRange> sourceRange = None)
         : data(data), sourceRange(sourceRange) {}
    static Type Integer(int bitWidth, bool isSigned, Optional<SourceRange> sourceRange = None) {
        return Type(IntProperties { bitWidth, isSigned }, sourceRange);
    }
    static Type I8(Optional<SourceRange> sourceRange = None) {
        return Integer(8, true, sourceRange);
    }
    static Type I16(Optional<SourceRange> sourceRange = None) {
        return Integer(16, true, sourceRange);
    }
    static Type I32(Optional<SourceRange> sourceRange = None) {
        return Integer(32, true, sourceRange);
    }
    static Type I64(Optional<SourceRange> sourceRange = None) {
        return Integer(64, true, sourceRange);
    }
    static Type U8(Optional<SourceRange> sourceRange = None) {
        return Integer(8, false, sourceRange);
    }
    static Type U16(Optional<SourceRange> sourceRange = None) {
        return Integer(16, false, sourceRange);
    }
    static Type U32(Optional<SourceRange> sourceRange = None) {
        return Integer(32, false, sourceRange);
    }
    static Type U64(Optional<SourceRange> sourceRange = None) {
        return Integer(64, false, sourceRange);
    }
    static Type Error() { return Type(ErrorTy()); }

    static Type Pointer(Type pointedTy, Optional<SourceRange> sourceRange = None) {
        auto pointed = std::make_shared<Type>(pointedTy);
        return Type(PointerTy { pointed }, sourceRange);
    }
    static Type TypeVariable(int number) {
        return Type(number);
    }
    static Type Void(Optional<SourceRange> sourceRange = None) {
        return Type(VoidTy(), sourceRange);
    }
    static Type Bool(Optional<SourceRange> sourceRange = None) {
        return Type(BoolTy(), sourceRange);
    }
    static Type Float(Optional<SourceRange> sourceRange = None) {
        return Type(FloatTy(), sourceRange);
    }
    static Type Double(Optional<SourceRange> sourceRange = None) {
        return Type(DoubleTy(), sourceRange);
    }

    ~Type() = default;
    Type(const Type& other) = default;
    Type& operator=(const Type& other) = default;

    bool operator==(Type other) const {
        return boost::apply_visitor(EqualityVisitor(), data, other.data);
    }
    bool operator!=(Type other) const { return !(*this == other); }

    std::string name() const;
};

struct Argument final : public ASTNode {
    std::shared_ptr<Expr> value;

    AST_NODE_CTOR(Argument, std::shared_ptr<Expr> value), value(value) {}
};

// A scope node represents a collection of other nodes.
struct Scope final : public ASTNode {
    std::vector<std::shared_ptr<ASTNode>> nodes;

    AST_NODE_CTOR(Scope, const std::vector<std::shared_ptr<ASTNode>>& nodes), nodes(nodes) {}
};
