set(SOURCES
    main.cpp
    Diagnostics.cpp
    Lexer.cpp
    main.cpp
    SourceInfo.cpp
    String.cpp
)
set(HEADERS
    Array.h
    Diagnostics.h
    Lexer.h
    Misc.h
    SourceInfo.h
    String.h
    Token.h
    TokenKinds.h
)

add_executable(meda ${SOURCES} ${HEADERS})
target_link_libraries(meda ${llvm_libs})
