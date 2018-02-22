cmake_minimum_required(VERSION 3.4.3)
project(quill-lang)

set(LLVM_DIR /Users/zachrwolfe/Downloads/llvm/build/lib/cmake/llvm)

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

# Set your project compile flags.
# E.g. if using the C++ header files
# you will need to enable C++11 support
# for your compiler.

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++1z")

include_directories(src ${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

# Find the libraries that correspond to the LLVM components
# that we wish to use
llvm_map_components_to_libnames(llvm_libs support core irreader)

set(ICU_PATH /usr/local/opt/icu4c)
link_directories(${ICU_PATH}/lib)
include_directories(${ICU_PATH}/include)

set(ICU_LIBS 
    libicuuc
)

add_subdirectory(src)
