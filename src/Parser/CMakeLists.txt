set(SOURCES
    Parser.cpp
    Lexer.cpp
)
set(HEADERS
    Parser.h
    Lexer.h
    Token.h
    TokenKinds.def
)

add_library(Parser ${SOURCES} ${HEADERS})
target_link_libraries(Parser AST General ${llvm_libs} ${icu_libs})
