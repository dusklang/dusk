//
//  AST.hpp
//  quillc
//
//  Created by Zach Wolfe on 2018-02-12.
//

#ifndef AST_hpp
#define AST_hpp

#include <string>
#include "llvm/ADT/SmallVector.h"

class Param {
private:
    std::string name;
    std::string typeName;
public:
    Param(const std::string& name, const std::string& typeName) : name(name), typeName(typeName) {}
    std::string prettyPrint() const {
        return name + ": " + typeName;
    }
};

class Decl {
private:
    std::string name;
    llvm::SmallVector<llvm::SmallVector<Param, 2>, 1> paramLists;
    std::string typeName;
public:
    Decl(const std::string& name,
         const llvm::SmallVector<llvm::SmallVector<Param, 2>, 1>& paramLists,
         const std::string& typeName)
        : name(name), paramLists(paramLists), typeName(typeName) {
            for(auto& params: paramLists) {
                assert(!params.empty() && "Encountered empty parameter list, which is not allowed.");
            }
        }
    std::string prettyPrint() const;
};

#endif /* AST_hpp */
