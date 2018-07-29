set(SOURCES
    AST.cpp
    ASTPrinter.cpp
    Types.cpp
    Decl.cpp
)
set(HEADERS
    AST.h
    Decl.h
    Expr.h
    Stmt.h
    ASTVisitor.h
    ASTNodes.def
    ASTPrinter.h
)

add_library(AST ${SOURCES} ${HEADERS})
target_link_libraries(AST ${llvm_libs})
