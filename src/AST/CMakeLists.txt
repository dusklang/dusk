set(SOURCES
    ASTPrinter.cpp
    Type.cpp
    Decl.cpp
    Expr.cpp
    Stmt.cpp
    Ident.cpp
)
set(HEADERS
    AST.h
    Decl.h
    Expr.h
    Stmt.h
    Type.h
    ASTVisitor.h
    ASTNodes.def
    ASTPrinter.h
    Ident.h
)

add_library(AST ${SOURCES} ${HEADERS})
target_link_libraries(AST ${llvm_libs})
