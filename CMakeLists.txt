cmake_minimum_required (VERSION 3.5)

project (Project)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_SOURCE_DIR}/cmake)

set (CMAKE_CXX_STANDARD 11)

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

add_executable(compiler src/compiler.cpp src/visitor.cpp ${PARSER_FILES})
target_link_libraries(compiler antlr4-runtime)
