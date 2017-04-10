#ifndef LISTENER_H
#define LISTENER_H

#include "GrammarBaseVisitor.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"

#include <map>

namespace pa037 {

  enum Type {
    UNDEFINED,
    INVALID,
    FUNCTION,
    INTEGER,
  };

  struct Expression {
    Type type = UNDEFINED;
    llvm::Value* value;

    Expression() {}
    Expression(Type) : type(type) {}
    Expression(Type type, llvm::Value* value) : type(type), value(value) {
    }
  };

  class SymbolTable {
  private:
    std::map<std::string, Expression> table;
  public:

    Expression& operator[](const std::string& name) {
      return table[name];
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
    antlrcpp::Any visitStatements(GrammarParser::StatementsContext* context) override;
    antlrcpp::Any visitIntegerLiteral(GrammarParser::IntegerLiteralContext* ctx) override;
    antlrcpp::Any visitAddExpr(GrammarParser::AddExprContext* context) override;
    antlrcpp::Any visitSubExpr(GrammarParser::SubExprContext* context) override;
    antlrcpp::Any visitMulExpr(GrammarParser::MulExprContext* context) override;
    antlrcpp::Any visitDivExpr(GrammarParser::DivExprContext* context) override;
    antlrcpp::Any visitDeclaration(GrammarParser::DeclarationContext* context) override;
    antlrcpp::Any visitIdentifierExpr(GrammarParser::IdentifierExprContext* context) override;


  private:
    llvm::Function* makeFunction(const std::string& name,
            GrammarParser::ArglistContext* arglist);
  };
}

#endif /* LISTENER_H */

