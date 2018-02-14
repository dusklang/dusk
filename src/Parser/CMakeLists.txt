set(SOURCES
    Parser.cpp
    Lexer.cpp
)
set(HEADERS
    Parser.hpp
    Lexer.hpp
    Token.hpp
    TokenKinds.def
    ParseResult.hpp
)

add_library(Parser ${SOURCES} ${HEADERS})
target_link_libraries(Parser AST ${llvm_libs} ${icu_libs})
