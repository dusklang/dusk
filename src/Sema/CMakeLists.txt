set(SOURCES
    TypeChecker.cpp
    ConstraintGenerator.cpp
)
set(HEADERS
    TypeChecker.h
    ConstraintGenerator.h
)

add_library(Sema ${SOURCES} ${HEADERS})
target_link_libraries(Sema AST General ${llvm_libs} ${icu_libs})
