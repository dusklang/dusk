//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include <memory>
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"

#include "AST/AST.hpp"
#include "AST/Expr.hpp"
#include "AST/Decl.hpp"

class CodeGenerator final {
private:
    llvm::LLVMContext context;
    llvm::IRBuilder<> builder;
    std::unique_ptr<llvm::Module> module;
public:
    CodeGenerator() : builder(context) {}
};
