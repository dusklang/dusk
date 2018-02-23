set(SOURCES
    TypeChecker.cpp
)
set(HEADERS
    TypeChecker.h
)

add_library(Sema ${SOURCES} ${HEADERS})
target_link_libraries(Sema AST General ${llvm_libs} ${icu_libs})
