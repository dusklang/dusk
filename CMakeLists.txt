cmake_minimum_required(VERSION 3.4.3)
project(meda-lang)

SET(CMAKE_C_COMPILER /usr/local/opt/llvm/bin/clang)
SET(CMAKE_CXX_COMPILER /usr/local/opt/llvm/bin/clang++)

find_package(LLVM REQUIRED CONFIG)
set(PATTERNS_INCLUDE_DIRS dependencies/patterns/include)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++17")
else()
    # MSVC
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /Zc:__cplusplus /std:c++17")
endif()

include_directories(src ${LLVM_INCLUDE_DIRS} ${PATTERNS_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

# Find the libraries that correspond to the LLVM components
# that we wish to use
llvm_map_components_to_libnames(llvm_libs support core irreader all)

add_subdirectory(src)

add_custom_target(run
    COMMAND meda
    DEPENDS meda
    WORKING_DIRECTORY ${CMAKE_PROJECT_DIR}
)
