#ifndef LISTENER_H
#define LISTENER_H

#include "GrammarBaseVisitor.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"

#include <map>

namespace pa037 {

  enum Type {
    FUNCTION,
    INTEGER,
  };

  struct Expression {
    Type type;
    llvm::Value* value;

    Expression(Type type, llvm::Value* value) : type(type), value(value) {
    }
  };

  class SymbolTable {
  private:
    std::map<std::string, Expression> table;
  public:

    Expression get(std::string name) const {
      return table.at(name);
    }

    void set(std::string name, Expression symbol) {
      table.insert(std::make_pair(name, symbol));
    }
  };

  class Visitor : public GrammarBaseVisitor {
  private:
    llvm::LLVMContext llvmContext;
    llvm::IRBuilder<> builder;
    llvm::Module* module;
    SymbolTable symbolTable;
  public:

    Visitor() : builder(llvmContext) {
    };
    antlrcpp::Any visitProgramFile(GrammarParser::ProgramFileContext* context) override;
    antlrcpp::Any visitExtdecl(GrammarParser::ExtdeclContext* context) override;
    antlrcpp::Any visitFunction(GrammarParser::FunctionContext* context) override;
    antlrcpp::Any visitIntegerLiteral(GrammarParser::IntegerLiteralContext* ctx) override;
    antlrcpp::Any visitAddExpr(GrammarParser::AddExprContext* context) override;
  };
}

#endif /* LISTENER_H */

