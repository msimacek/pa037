#include "visitor.h"
#include "GrammarParser.h"

#include "llvm/IR/Constants.h"
#include "llvm/Support/raw_ostream.h"

#include <iostream>
#include <string>

using namespace std;
using namespace antlrcpp;
using namespace pa037;

antlrcpp::Any Visitor::visitProgramFile(GrammarParser::ProgramFileContext* context) {
  llvm::Module mod("main", llvmContext);
  module = &mod;
  GrammarBaseVisitor::visitProgramFile(context);
  module->print(llvm::errs(), nullptr);
  return Any();
}

llvm::Function* Visitor::makeFunction(const std::string& name,
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

antlrcpp::Any Visitor::visitExtdecl(GrammarParser::ExtdeclContext* context) {
  auto name = context->name->getText();
  llvm::Function* function = makeFunction(name, context->arglist());
  symbolTable.set(name, Expression(FUNCTION, function));
  return Any();
}

Any Visitor::visitFunction(GrammarParser::FunctionContext* context) {
  auto name = context->name->getText();
  llvm::Function* function = makeFunction(name, context->arglist());
  symbolTable.set(name, Expression(FUNCTION, function));
  llvm::BasicBlock *block = llvm::BasicBlock::Create(llvmContext, "entry", function);
  builder.SetInsertPoint(block);
//  auto it1 = function->args().begin();
//  auto it2 = context->arglist()->ID().begin();
//  for (; it2 != context->arglist()->ID().end(); ++it1, ++it2) {
//    symbolTable.set((*it2)->getText(), Expression(INTEGER, &*it1));
//  }
  Expression expr = visit(context->statements());
  builder.CreateRet(expr.value);
  return Any();
}

antlrcpp::Any Visitor::visitStatements(GrammarParser::StatementsContext* context) {
  Any value;
  for (auto statement : context->statement()) {
    value = visit(statement);
  }
  return value;
}


antlrcpp::Any Visitor::visitIntegerLiteral(GrammarParser::IntegerLiteralContext* ctx) {
  long long value = std::stoi(ctx->value->getText());
  return Any(Expression(INTEGER,
          llvm::ConstantInt::get(llvmContext, llvm::APInt(32, value, true))));
}

antlrcpp::Any Visitor::visitAddExpr(GrammarParser::AddExprContext* context) {
  Expression left = visit(context->expression(0));
  Expression right = visit(context->expression(1));
  return Any(Expression(INTEGER,
          builder.CreateAdd(left.value, right.value)));
}


