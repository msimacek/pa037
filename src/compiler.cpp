#include <iostream>

#include "antlr4-runtime.h"
#include "GrammarLexer.h"
#include "GrammarParser.h"

#include "visitor.h"

int main(int argc, const char* argv[]) {
  std::ifstream stream;
  stream.open(argv[1]);
  antlr4::ANTLRInputStream input(stream);
  GrammarLexer lexer(&input);
  antlr4::CommonTokenStream tokens(&lexer);
  GrammarParser parser(&tokens);

  if (parser.getNumberOfSyntaxErrors())
    return 1;

  GrammarParser::ProgramFileContext* tree = parser.programFile();
  pa037::Visitor visitor;
  visitor.visitProgramFile(tree);

  return 0;
}
