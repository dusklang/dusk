set(SOURCES
    LIR.cpp
)
set(HEADERS
    LIR.h
)

add_library(LIR ${SOURCES} ${HEADERS})
target_link_libraries(LIR AST General)
