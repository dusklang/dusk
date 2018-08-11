set(SOURCES
    SourceInfo.cpp
    Diagnostics.cpp
)
set(HEADERS
    SourceInfo.h
    String.h
    Diagnostics.h
)

add_library(General ${SOURCES} ${HEADERS})
target_link_libraries(General Parser AST ${llvm_libs})
