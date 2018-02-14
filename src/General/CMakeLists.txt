set(SOURCES

)
set(HEADERS
    Diagnostic.hpp
)

add_library(General ${SOURCES} ${HEADERS})
target_link_libraries(General Parser AST ${llvm_libs} ${icu_libs})
