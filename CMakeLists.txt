cmake_minimum_required(VERSION 3.4.3)
project(quill-lang)

set(LLVM_PATH /usr/local/opt/llvm)
link_directories(${LLVM_PATH}/lib)
include_directories(src ${LLVM_PATH}/include)

add_definitions(
-D__STDC_LIMIT_MACROS
-D__STDC_CONSTANT_MACROS
)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++1z")

#target_link_libraries(ifcount
#  clangFrontend
#  clangSerialization
#  clangDriver
#  clangParse
#  clangSema
#  clangAnalysis
#  clangAST
#  clangBasic
#  clangEdit
#  clangLex
#  clangTooling
#)

set(LLVM_LIBS
    LLVMX86AsmParser # MC, MCParser, Support, X86Desc, X86Info
    LLVMX86Desc # MC, Support, X86AsmPrinter, X86Info
    LLVMX86AsmPrinter # MC, Support, X86Utils
    LLVMX86Info # MC, Support, Target
    LLVMX86Utils # Core, Support
    LLVMipo
    LLVMScalarOpts
    LLVMInstCombine
    LLVMTransformUtils
    LLVMipa
    LLVMAnalysis
    LLVMTarget
    LLVMOption # Support
    LLVMMCParser # MC, Support
    LLVMMC # Object, Support
    LLVMObject # BitReader, Core, Support
    LLVMBitReader # Core, Support
    LLVMCore # Support
    LLVMSupport
)

include(FindCurses)
set(LLVM_LIBS ${LLVM_LIBS}
    pthread
    z
    dl
    ${CURSES_LIBRARIES}
)

set(ICU_PATH /usr/local/opt/icu4c)
link_directories(${ICU_PATH}/lib)
include_directories(${ICU_PATH}/include)

set(ICU_LIBS 
    libicuuc
)

add_subdirectory(src)
