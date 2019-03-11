set(SOURCES
    main.cpp
    Diagnostics.cpp
    Lexer.cpp
    main.cpp
    SourceInfo.cpp
    HIR.cpp
    Parser.cpp
    TypeChecker.cpp
    Collections.cpp
)
set(HEADERS
    Collections.h
    Diagnostics.h
    Lexer.h
    Misc.h
    SourceInfo.h
    Token.h
    TokenKinds.h
    HIR.h
    Parser.h
    TypeChecker.h
)

add_executable(meda ${SOURCES} ${HEADERS})
target_link_libraries(meda ${llvm_libs})
