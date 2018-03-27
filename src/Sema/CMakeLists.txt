set(SOURCES
    NameResolver.cpp
    ConstraintGenerator.cpp
    ConstraintSolver.cpp
    Constraint.cpp
)
set(HEADERS
    Solution.h
    NameResolver.h
    ConstraintGenerator.h
    Constraint.h
    ConstraintSolver.h
)

add_library(Sema ${SOURCES} ${HEADERS})
target_link_libraries(Sema AST General ${llvm_libs} ${icu_libs})
