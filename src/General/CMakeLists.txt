set(SOURCES
)
set(HEADERS
    SourceLoc.hpp
)

add_library(General ${SOURCES} ${HEADERS})
target_link_libraries(General Parser AST ${llvm_libs} ${icu_libs})
