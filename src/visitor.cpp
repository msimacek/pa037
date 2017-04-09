#include "visitor.h"
#include "GrammarParser.h"

#include <iostream>

using namespace std;
using namespace antlrcpp;
using namespace pa037;

Any Visitor::visitFunction(GrammarParser::FunctionContext* context) {
  cout << context->name->getText() << endl;
  return GrammarBaseVisitor::visitFunction(context);
}

Any Visitor::visitLiteral(GrammarParser::LiteralContext* context) {
  return Any(0);
}


