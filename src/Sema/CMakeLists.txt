set(SOURCES
    TypeChecker.cpp
    ConstraintGenerator.cpp
    ConstraintSolver.cpp
    Constraint.cpp
)
set(HEADERS
    TypeChecker.h
    ConstraintGenerator.h
    Constraint.h
    ConstraintSolver.h
)

add_library(Sema ${SOURCES} ${HEADERS})
target_link_libraries(Sema AST General ${llvm_libs} ${icu_libs})
