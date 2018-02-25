set(SOURCES
    ASTPrinter.cpp
    Types.cpp
)
set(HEADERS
    AST.h
    Decl.h
    Expr.h
    Stmt.h
    Types.h
    ASTVisitor.h
    ASTNodes.def
    ASTPrinter.h
    BuiltinTypes.def
    Types.h
)

add_library(AST ${SOURCES} ${HEADERS})
target_link_libraries(AST ${llvm_libs})
