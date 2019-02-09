set(SOURCES
    main.cpp
)
set(HEADERS
)

add_executable(meda ${SOURCES} ${HEADERS})
target_link_libraries(meda Parser AST LLVMGen Sema LIR ${llvm_libs})
