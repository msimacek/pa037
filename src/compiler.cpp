#include "llvm/Support/raw_ostream.h"

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Verifier.h"

#include "antlr4-runtime.h"
#include "GrammarLexer.h"
#include "GrammarParser.h"
#include "GrammarBaseVisitor.h"

#include <iostream>
#include <map>

using antlrcpp::Any;

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

    Expression() {
    }

    Expression(Type) : type(type) {
    }

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
    llvm::Function* function;
    SymbolTable symbolTable;
  public:

    Visitor() : builder(llvmContext) {
    };

    Any visitProgramFile(GrammarParser::ProgramFileContext* context) override {
      llvm::Module mod("main", llvmContext);
      module = &mod;
      GrammarBaseVisitor::visitProgramFile(context);
      module->print(llvm::errs(), nullptr);
      return Any();
    }

    Any visitExtdecl(GrammarParser::ExtdeclContext* context) override {
      auto name = context->name->getText();
      llvm::Function* function = makeFunction(name, context->arglist());
      symbolTable[name] = Expression(FUNCTION, function);
      return Any();
    }

    Any visitFunction(GrammarParser::FunctionContext* context) override {
      auto name = context->name->getText();
      function = makeFunction(name, context->arglist());
      symbolTable[name] = Expression(FUNCTION, function);
      llvm::BasicBlock *block = llvm::BasicBlock::Create(llvmContext, "entry", function);
      builder.SetInsertPoint(block);
      auto it = function->args().begin();
      for (auto arg : context->arglist()->ID()) {
        auto& fnArg = *it;
        fnArg.setName(arg->getText());
        symbolTable[arg->getText()] = Expression(INTEGER, &fnArg);
        it++;
      }
      Expression expr = visit(context->statements());
      if (expr.type != INVALID)
        builder.CreateRet(expr.value);
      llvm::verifyFunction(*function);
      function = nullptr;
      return Any();
    }

    Any visitStatements(GrammarParser::StatementsContext* context) override {
      Any value;
      for (auto statement : context->statement()) {
        value = visit(statement);
      }
      return value;
    }

    Any visitIntegerLiteral(GrammarParser::IntegerLiteralContext* ctx) override {
      long long value = std::stoi(ctx->value->getText());
      return Any(Expression(INTEGER,
              llvm::ConstantInt::get(llvmContext, llvm::APInt(32, value, true))));
    }

#define visitArithExpr(name, op) \
Any visit##name##Expr(GrammarParser::name##ExprContext* context) { \
  Expression left = visit(context->expression(0)); \
  Expression right = visit(context->expression(1)); \
  if (left.type == INTEGER && right.type == INTEGER) { \
    return Any(Expression(INTEGER, builder.Create##op(left.value, right.value))); \
  } else { \
    return Any(Expression(INVALID)); \
  } \
}

    //    Any visitAddExpr(GrammarParser::AddExprContext* context) override;
    //    Any visitSubExpr(GrammarParser::SubExprContext* context) override;
    //    Any visitMulExpr(GrammarParser::MulExprContext* context) override;
    //    Any visitDivExpr(GrammarParser::DivExprContext* context) override;
    visitArithExpr(Add, Add);
    visitArithExpr(Sub, Sub);
    visitArithExpr(Mul, Mul);
    visitArithExpr(Div, SDiv);
    //

    Any visitDeclaration(GrammarParser::DeclarationContext* context) override {
      const auto& name = context->name->getText();
      auto& var = symbolTable[name];
      var.type = INTEGER;
      var.value = builder.CreateAlloca(llvm::Type::getInt32Ty(llvmContext), nullptr, name);
    }

    Any visitIdentifierExpr(GrammarParser::IdentifierExprContext* context) override {
      const Expression& var = symbolTable[context->ID()->getText()];
      if (var.type == UNDEFINED)
        error(context, "Undefined variable");
      return Any(Expression(var));
    }


  private:

    llvm::Function* makeFunction(const std::string& name,
            GrammarParser::ArglistContext* arglist) {
      std::vector<llvm::Type*> argtypes(arglist->ID().size(),
              llvm::Type::getInt32Ty(llvmContext));
      llvm::FunctionType *functionType =
              llvm::FunctionType::get(llvm::Type::getInt32Ty(llvmContext), argtypes, false);

      llvm::Function *function =
              llvm::Function::Create(functionType, llvm::Function::ExternalLinkage,
              name, module);
      return function;
    }

    void error(antlr4::ParserRuleContext* context, const std::string& message) {
      std::cerr << message << std::endl;
      exit(2);
    }
  };
}

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
