set(SOURCES
    SourceInfo.cpp
)
set(HEADERS
    SourceInfo.h
    String.h
)

add_library(General ${SOURCES} ${HEADERS})
target_link_libraries(General Parser AST ${llvm_libs})
