set(SOURCES
    CodeGenerator.cpp
)
set(HEADERS
    CodeGenerator.h
)

add_library(IRGen ${SOURCES} ${HEADERS})
target_link_libraries(IRGen AST General ${llvm_libs})
