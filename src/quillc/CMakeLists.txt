add_executable(quillc main.cpp)
target_link_libraries(quillc Parser ${llvm_libs})
