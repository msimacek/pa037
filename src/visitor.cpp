#include "visitor.h"
#include "GrammarParser.h"

#include "llvm/IR/Constants.h"
#include "llvm/Support/raw_ostream.h"

#include <iostream>
#include <string>

using namespace std;
using namespace antlrcpp;
using namespace pa037;

void error(antlr4::ParserRuleContext* context, const std::string& message) {
  cerr << message << endl;
  exit(2);
}

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
  symbolTable[name] = Expression(FUNCTION, function);
  return Any();
}

Any Visitor::visitFunction(GrammarParser::FunctionContext* context) {
  auto name = context->name->getText();
  llvm::Function* function = makeFunction(name, context->arglist());
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

#define visitArithExpr(name, op) \
antlrcpp::Any Visitor::visit##name##Expr(GrammarParser::name##ExprContext* context) { \
  Expression left = visit(context->expression(0)); \
  Expression right = visit(context->expression(1)); \
  if (left.type == INTEGER && right.type == INTEGER) { \
    return Any(Expression(INTEGER, builder.Create##op(left.value, right.value))); \
  } else { \
    return Any(Expression(INVALID)); \
  } \
}

visitArithExpr(Add, Add);
visitArithExpr(Sub, Sub);
visitArithExpr(Mul, Mul);
visitArithExpr(Div, SDiv);

antlrcpp::Any Visitor::visitDeclaration(GrammarParser::DeclarationContext* context) {
  //symbolTable[context->ID()]
}

antlrcpp::Any Visitor::visitIdentifierExpr(GrammarParser::IdentifierExprContext* context) {
  const Expression& var = symbolTable[context->ID()->getText()];
  if (var.type == UNDEFINED)
    error(context, "Undefined variable");
  return Any(Expression(var));
}

