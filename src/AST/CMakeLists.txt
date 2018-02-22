set(SOURCES
    ASTPrinter.cpp
)
set(HEADERS
    AST.hpp
    Decl.hpp
    Expr.hpp
    Stmt.hpp
    Types.hpp
    ASTVisitor.hpp
    ASTNodes.def
    ASTPrinter.hpp
    BuiltinTypes.def
    Types.hpp
)

add_library(AST ${SOURCES} ${HEADERS})
target_link_libraries(AST ${llvm_libs})
