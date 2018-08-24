set(SOURCES
    SourceInfo.cpp
    Diagnostics.cpp
    String.cpp
    Array.cpp
)
set(HEADERS
    SourceInfo.h
    String.h
    Diagnostics.h
    Array.h
	General.h
)

add_library(General ${SOURCES} ${HEADERS})
target_link_libraries(General Parser AST ${llvm_libs})
