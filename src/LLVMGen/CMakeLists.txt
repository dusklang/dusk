set(SOURCES
    LLVMGenerator.cpp
)
set(HEADERS
    LLVMGenerator.h
)

add_library(LLVMGen ${SOURCES} ${HEADERS})
target_link_libraries(LLVMGen AST General ${llvm_libs})
