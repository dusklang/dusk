set(SOURCES
    ASTPrinter.cpp
    Types.cpp
)
set(HEADERS
    AST.h
    Decl.h
    Expr.h
    Stmt.h
    ASTVisitor.h
    ASTNodes.def
    ASTPrinter.h
    Types.h
)

add_library(AST ${SOURCES} ${HEADERS})
target_link_libraries(AST ${llvm_libs})
