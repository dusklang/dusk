set(SOURCES
    CodeGenerator.cpp
)
set(HEADERS
    CodeGenerator.hpp
)

add_library(IRGen ${SOURCES} ${HEADERS})
target_link_libraries(IRGen AST ${llvm_libs} ${icu_libs})
