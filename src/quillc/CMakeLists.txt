set(SOURCES
    main.cpp
)
set(HEADERS
)

add_executable(quillc ${SOURCES} ${HEADERS})
target_link_libraries(quillc Parser AST IRGen ${llvm_libs})
