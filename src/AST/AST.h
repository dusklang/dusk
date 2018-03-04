//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <string>
#include <vector>
#include "llvm/ADT/Optional.h"
#include "llvm/IR/Value.h"
#include "General/SourceLoc.h"
#include "boost/variant.hpp"

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
    struct VoidTy {};
    struct BoolTy {};
    struct FloatTy {};
    struct DoubleTy {};
    struct ErrorTy {};

    typedef boost::variant<IntProperties, std::shared_ptr<Type>, std::string, VoidTy, BoolTy, FloatTy, DoubleTy, ErrorTy> DataType;
private:
    DataType data;
    llvm::Optional<SourceRange> sourceRange;

    struct EqualityVisitor: public boost::static_visitor<bool> {
        bool operator()(IntProperties lhs, IntProperties rhs) const { return lhs == rhs; }
        bool operator()(std::shared_ptr<Type> lhs, std::shared_ptr<Type> rhs) const {
            return *lhs == *rhs;
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

    Type(DataType data,
         llvm::Optional<SourceRange> sourceRange = llvm::None)
        : data(data), sourceRange(sourceRange) {}
public:
    static Type Integer(int bitWidth, bool isSigned, llvm::Optional<SourceRange> sourceRange = llvm::None) {
        return Type(IntProperties { bitWidth, isSigned }, sourceRange);
    }
    static Type I8(llvm::Optional<SourceRange> sourceRange = llvm::None) {
        return Integer(8, true, sourceRange);
    }
    static Type I16(llvm::Optional<SourceRange> sourceRange = llvm::None) {
        return Integer(16, true, sourceRange);
    }
    static Type I32(llvm::Optional<SourceRange> sourceRange = llvm::None) {
        return Integer(32, true, sourceRange);
    }
    static Type I64(llvm::Optional<SourceRange> sourceRange = llvm::None) {
        return Integer(64, true, sourceRange);
    }
    static Type U8(llvm::Optional<SourceRange> sourceRange = llvm::None) {
        return Integer(8, false, sourceRange);
    }
    static Type U16(llvm::Optional<SourceRange> sourceRange = llvm::None) {
        return Integer(16, false, sourceRange);
    }
    static Type U32(llvm::Optional<SourceRange> sourceRange = llvm::None) {
        return Integer(32, false, sourceRange);
    }
    static Type U64(llvm::Optional<SourceRange> sourceRange = llvm::None) {
        return Integer(64, false, sourceRange);
    }
    static Type Error() { return Type(ErrorTy()); }

    static Type Pointer(Type pointedTy, llvm::Optional<SourceRange> sourceRange = llvm::None) {
        return Type(std::make_shared<Type>(pointedTy), sourceRange);
    }
    static Type TypeVariable(std::string name) {
        return Type(name);
    }
    static Type Void(llvm::Optional<SourceRange> sourceRange = llvm::None) {
        return Type(VoidTy(), sourceRange);
    }
    static Type Bool(llvm::Optional<SourceRange> sourceRange = llvm::None) {
        return Type(BoolTy(), sourceRange);
    }
    static Type Float(llvm::Optional<SourceRange> sourceRange = llvm::None) {
        return Type(FloatTy(), sourceRange);
    }
    static Type Double(llvm::Optional<SourceRange> sourceRange = llvm::None) {
        return Type(DoubleTy(), sourceRange);
    }

    ~Type() = default;
    Type(const Type& other) = default;
    Type& operator=(const Type& other) = default;

    DataType getData() const { return data; }

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
