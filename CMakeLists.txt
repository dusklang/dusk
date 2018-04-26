cmake_minimum_required(VERSION 3.4.3)
project(meda-lang)

find_package(LLVM REQUIRED CONFIG)
set(PATTERNS_INCLUDE_DIRS dependencies/patterns/include)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++17")

include_directories(src ${LLVM_INCLUDE_DIRS} ${PATTERNS_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

# Find the libraries that correspond to the LLVM components
# that we wish to use
llvm_map_components_to_libnames(llvm_libs support core irreader all)

set(ICU_PATH /usr/local/opt/icu4c)
link_directories(${ICU_PATH}/lib)
include_directories(${ICU_PATH}/include)

set(ICU_LIBS 
    libicuuc
)

add_subdirectory(src)

add_custom_target(run
    COMMAND medac
    DEPENDS medac
    WORKING_DIRECTORY ${CMAKE_PROJECT_DIR}
)
