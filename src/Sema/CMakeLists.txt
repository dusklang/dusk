set(SOURCES
    BuiltinTypes.cpp
)
set(HEADERS
    BuiltinTypes.hpp
    BuiltinTypes.def
)

add_library(Sema ${SOURCES} ${HEADERS})
target_link_libraries(Sema AST ${llvm_libs} ${icu_libs})
