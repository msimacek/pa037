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
}

antlrcpp::Any Visitor::visitExtdecl(GrammarParser::ExtdeclContext* context) {
  const std::string& name = context->name->getText();
  std::vector<llvm::Type*> argtypes(context->arglist()->ID().size(),
                             llvm::Type::getInt32Ty(llvmContext));
  llvm::FunctionType *functionType =
    llvm::FunctionType::get(llvm::Type::getInt32Ty(llvmContext), argtypes, false);

  llvm::Function *function =
    llvm::Function::Create(functionType, llvm::Function::ExternalLinkage,
        name, module);
  symbolTable.set(name, Expression(FUNCTION, function));
  return Any();
}

Any Visitor::visitFunction(GrammarParser::FunctionContext* context) {
  return GrammarBaseVisitor::visitFunction(context);
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


