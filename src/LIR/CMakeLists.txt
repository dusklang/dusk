set(SOURCES
    LIR.cpp
    LIRGenerator.cpp
)
set(HEADERS
    LIR.h
    LIRGenerator.h
)

add_library(LIR ${SOURCES} ${HEADERS})
target_link_libraries(LIR AST General)
