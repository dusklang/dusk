set(SOURCES
    AST.cpp
    Decl.cpp
    Expr.cpp
    Stmt.cpp
)
set(HEADERS
    AST.hpp
    Decl.hpp
    Expr.hpp
    Stmt.hpp
)

add_library(AST ${SOURCES} ${HEADERS})
target_link_libraries(AST ${llvm_libs})
