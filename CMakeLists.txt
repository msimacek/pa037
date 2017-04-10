cmake_minimum_required (VERSION 3.5)

project (Project)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_SOURCE_DIR}/cmake)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-attributes")

find_package(LLVM REQUIRED CONFIG)
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})
llvm_map_components_to_libnames(llvm_libs support core)

find_package(Antlr)
include_directories(${ANTLR_INCLUDE_DIRS})
link_directories(${ANTLR_LIBRARY_DIRS})
add_definitions(${ANTLR_DEFINITIONS})

set(PARSER_DIR ${CMAKE_BINARY_DIR}/parser)
file(MAKE_DIRECTORY ${PARSER_DIR})
set(PARSER_FILES
    ${PARSER_DIR}/GrammarParser.cpp
    ${PARSER_DIR}/GrammarLexer.cpp
    ${PARSER_DIR}/GrammarBaseVisitor.cpp
)
add_custom_command(
    OUTPUT ${PARSER_FILES}
    COMMAND antlr4 -Dlanguage=Cpp -visitor -no-listener ${CMAKE_SOURCE_DIR}/src/Grammar.g4 -o ${PARSER_DIR}
    DEPENDS ${CMAKE_SOURCE_DIR}/src/Grammar.g4
)
include_directories(${PARSER_DIR})

add_executable(compiler src/compiler.cpp ${PARSER_FILES})
target_link_libraries(compiler antlr4-runtime ${llvm_libs})
