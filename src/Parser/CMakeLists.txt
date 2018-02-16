set(SOURCES
    Lexer.cpp
)
set(HEADERS
    Lexer.hpp
    Token.hpp
    ParseTree.hpp
    TokenKinds.def
    ParseResult.hpp
)

add_library(Parser ${SOURCES} ${HEADERS})
target_link_libraries(Parser AST ${llvm_libs} ${icu_libs})
