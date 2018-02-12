set(SOURCES
    AST.cpp
)
set(HEADERS
    AST.hpp
)

add_library(AST ${SOURCES} ${HEADERS})
target_link_libraries(AST ${llvm_libs})
