#ifndef LISTENER_H
#define LISTENER_H

#include "GrammarBaseVisitor.h"

namespace pa037 {

  class Visitor : public GrammarBaseVisitor {
  public:
    antlrcpp::Any visitFunction(GrammarParser::FunctionContext* context) override;
    antlrcpp::Any visitLiteral(GrammarParser::LiteralContext* context) override;
  };
}

#endif /* LISTENER_H */

