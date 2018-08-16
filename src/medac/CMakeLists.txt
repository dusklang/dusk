set(SOURCES
    main.cpp
)
set(HEADERS
)

add_executable(medac ${SOURCES} ${HEADERS})
target_link_libraries(medac Parser AST LLVMGen Sema LIR ${llvm_libs})
