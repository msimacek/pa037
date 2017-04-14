#include "llvm/Support/raw_ostream.h"

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Verifier.h"

#include "antlr4-runtime.h"
#include "GrammarLexer.h"
#include "GrammarParser.h"
#include "GrammarBaseVisitor.h"

#include <iostream>
#include <map>
#include <memory>

using antlrcpp::Any;
using namespace std;

namespace pa037 {

class SymbolTable {
private:
    std::vector<std::map<std::string, llvm::Value*>> tables;
public:
    SymbolTable() {
        tables.resize(1);
    }

    llvm::Value*& operator[](const std::string& name) {
        for (auto it = tables.rbegin(); it != tables.rend(); ++it) {
            auto match = it->find(name);
            if (match != it->end())
                return match->second;
        }
        return tables.back()[name];
    }

    void enterScope() {
        tables.resize(tables.size() + 1);
    }

    void exitScope() {
        tables.resize(tables.size() - 1);
    }
};

class Argument {
public:
    string name;
    llvm::Type* type;
    Argument(const string& name, llvm::Type* type) :
            name(name), type(type) {
    }
};

class Function {
public:
    llvm::Function* llfunction;
    llvm::Type* type;
    vector<shared_ptr<Argument>> args;
    Function(llvm::Function* llfunction, llvm::Type* type,
            vector<shared_ptr<Argument>> args) :
            llfunction(llfunction), type(type), args(args) {
    }
};

class Visitor: public GrammarBaseVisitor {
private:
    llvm::LLVMContext llvmContext;
    llvm::IRBuilder<> builder;
    llvm::Module* module;
    shared_ptr<Function> currentFunction;
    bool currentBlockTerminated = false;
    SymbolTable symbolTable;
    map<std::string, shared_ptr<Function>> functions;

    llvm::Type* intType = llvm::Type::getInt32Ty(llvmContext);
    llvm::Type* boolType = llvm::Type::getInt1Ty(llvmContext);
    llvm::Type* charType = llvm::Type::getInt8Ty(llvmContext);
    llvm::Type* voidType = llvm::Type::getVoidTy(llvmContext);
    llvm::Type* stringType = llvm::PointerType::getUnqual(charType);

    map<std::string, llvm::Type*> types { { "int", intType },
            { "bool", boolType }, { "char", charType } };

public:

    Visitor() :
            builder(llvmContext), module(nullptr) {
    }

#define error(context, message_expr) do { \
    cerr << "line " << context->start->getLine() << ": " << message_expr << endl; \
    exit(2); \
    throw 0; \
} while(0)

    Any visitProgramFile(GrammarParser::ProgramFileContext* context) override {
        llvm::Module mod("main", llvmContext);
        module = &mod;
        GrammarBaseVisitor::visitProgramFile(context);
        module->print(llvm::outs(), nullptr);
        return Any();
    }

    Any visitFndecl(GrammarParser::FndeclContext* context) override {
        const string& name = context->name->getText();
        vector<shared_ptr<Argument>> args;
        vector<llvm::Type*> lltypes;
        for (auto arg : context->arglist()->arg()) {
            auto argType = getType(context, arg->type());
            args.push_back(
                    make_shared<Argument>(arg->name->getText(), argType));
            lltypes.push_back(argType);
        }
        auto type =
                (context->type()) ?
                        getType(context, context->type()) : voidType;
        llvm::FunctionType *functionType = llvm::FunctionType::get(type,
                lltypes, context->variadic);

        llvm::Function *llfunction = llvm::Function::Create(functionType,
                llvm::Function::ExternalLinkage, name, module);
        shared_ptr<Function> function = make_shared<Function>(llfunction, type,
                args);
        functions[name] = function;
        return function;
    }

    Any visitFunction(GrammarParser::FunctionContext* context) override {
        currentFunction = visitFndecl(context->fndecl());
        currentBlockTerminated = false;
        llvm::BasicBlock *block = llvm::BasicBlock::Create(llvmContext, "entry",
                currentFunction->llfunction);
        builder.SetInsertPoint(block);
        auto it = currentFunction->llfunction->args().begin();
        for (auto arg : currentFunction->args) {
            auto& fnArg = *it;
            fnArg.setName(arg->name);
            auto alloc = builder.CreateAlloca(fnArg.getType(), nullptr,
                    arg->name + "_var");
            builder.CreateStore(&fnArg, alloc);
            symbolTable[arg->name] = alloc;
            it++;
        }
        visit(context->statements());
        if (!currentBlockTerminated) {
            if (currentFunction->type == voidType)
                builder.CreateRetVoid();
            else
                error(context, "Missing return");
        }
        if (llvm::verifyFunction(*currentFunction->llfunction, &llvm::errs()))
            error(context, "LLVM verification error");
        currentFunction = nullptr;
        return Any();
    }

    Any visitReturnStmt(GrammarParser::ReturnStmtContext* context) override {
        if (context->expression()) {
            llvm::Value* value = visit(context->expression());
            if (value->getType() != currentFunction->type)
                error(context,
                        "Expression in return doesn't match function type");
            builder.CreateRet(value);
        } else {
            if (currentFunction->type != voidType)
                error(context,
                        "Missing value for return in a non-void function");
            builder.CreateRetVoid();
        }
        currentBlockTerminated = true;
        return Any();
    }

    Any visitCall(GrammarParser::CallContext* context) override {
        const string& name = context->name->getText();
        shared_ptr<Function> function = functions[name];
        if (!function)
            error(context, "Function " << name << "not declared");
        vector<llvm::Value*> args;
        vector<llvm::Type*> expectedTypes;
        for (auto arg : function->args) {
            expectedTypes.push_back(arg->type);
        }
        vector<llvm::Type*> actualTypes;
        for (auto expr : context->expression()) {
            llvm::Value* value = visit(expr);
            args.push_back(value);
            actualTypes.push_back(value->getType());
        }
        if (function->llfunction->isVarArg()) {
            actualTypes.resize(expectedTypes.size());
        }
        if (actualTypes != expectedTypes) {
            error(context, "Incorrect function arguments");
        }
        llvm::Value* value = builder.CreateCall(function->llfunction, args);
        return value;
    }

    Any visitStatements(GrammarParser::StatementsContext* context) override {
        symbolTable.enterScope();
        for (auto statement : context->statement()) {
            if (currentBlockTerminated)
                error(statement, "Unreachable code");
            visit(statement);
        }
        symbolTable.exitScope();
        return Any();
    }

    Any visitIntegerLiteral(GrammarParser::IntegerLiteralContext* ctx)
            override {
        auto value = std::stoi(ctx->value->getText());
        return (llvm::Value*) llvm::ConstantInt::get(intType, value);
    }

    Any visitCharLiteral(GrammarParser::CharLiteralContext* context) override {
        string value = processStringLiteral(context->CHAR()->getText());
        return (llvm::Value*) llvm::ConstantInt::get(charType, value[0]);
    }

    Any visitStringLiteral(GrammarParser::StringLiteralContext* context)
            override {
        string value = processStringLiteral(context->STRING()->getText());
        auto constant = llvm::ConstantDataArray::getString(llvmContext, value,
                true);
        auto global = new llvm::GlobalVariable(*module, constant->getType(),
                true, llvm::GlobalValue::PrivateLinkage, constant);
        auto pointer = builder.CreateBitCast(global, stringType);
        return pointer;
    }

    Any visitBooleanLiteral(GrammarParser::BooleanLiteralContext* ctx)
            override {
        int value = (ctx->value->getText() == "true") ? 1 : 0;
        return (llvm::Value*)llvm::ConstantInt::get(llvmContext, llvm::APInt(1, value, true));
    }

    Any visitArithExpr(GrammarParser::ArithExprContext* context) override {
        llvm::Value* left = visit(context->expression(0));
        llvm::Value* right = visit(context->expression(1));
        char op1 = context->op->getText()[0];
        char op2 = 0;
        if (context->op->getText().length() == 2)
            op2 = context->op->getText()[1];
        if (left->getType() != right->getType())
            error(context, "Incompatible types");
        if (left->getType() != intType)
            error(context, "Unsupported type for " << op1);
        llvm::Value* value;
        switch (op1) {
        case '+':
            value = builder.CreateAdd(left, right);
            break;
        case '-':
            value = builder.CreateSub(left, right);
            break;
        case '*':
            value = builder.CreateMul(left, right);
            break;
        case '/':
            value = builder.CreateSDiv(left, right);
            break;
        case '!': // !=
            value = builder.CreateICmpNE(left, right);
            break;
        case '=': // ==
            value = builder.CreateICmpEQ(left, right);
            break;
        case '<':
            if (op2) // <=
                value = builder.CreateICmpSLE(left, right);
            else
                // <
                value = builder.CreateICmpSLT(left, right);
            break;
        case '>':
            if (op2) // >=
                value = builder.CreateICmpSGE(left, right);
            else
                // >
                value = builder.CreateICmpSGT(left, right);
            break;
        default:
            error(context, "BUG Unknown operator");
        }
        return value;
    }

    Any visitLogicExpr(GrammarParser::LogicExprContext* context) override {
        llvm::Value* left = visit(context->expression(0));
        llvm::Value* right = visit(context->expression(1));
        char op = context->op->getText()[0];
        if (left->getType() != boolType || right->getType() != boolType)
            error(context, op << " requires boolean operands");
        llvm::Value* value;
        switch (op) {
        case 'a':
            value = builder.CreateAnd(left, right);
            break;
        case 'o':
            value = builder.CreateOr(left, right);
            break;
        }
        return value;
    }

    Any visitNotExpr(GrammarParser::NotExprContext* context) override {
        llvm::Value* expr = visit(context->expression());
        if (expr->getType() != boolType)
            error(context, "Negation operand not boolean");
        llvm::Value* value = builder.CreateICmpEQ(expr,
                llvm::ConstantInt::getFalse(llvmContext));
        return value;
    }

    Any visitDeclaration(GrammarParser::DeclarationContext* context) override {
        const string& name = context->name->getText();
        llvm::Value* initializer = nullptr;
        if (context->expression())
            initializer = visit(context->expression());
        llvm::Type* type;
        if (context->type()) {
            type = getType(context, context->type());
        } else if (initializer) {
            type = initializer->getType();
        } else {
            error(context,
                    "Variable declaration needs to have a type or an initializer");
        }
        auto alloc = builder.CreateAlloca(type, nullptr, name);
        if (initializer) {
            if (initializer->getType() != type)
                error(context, "Incompatible type in initialization");
            builder.CreateStore(initializer, alloc);
        }
        symbolTable[name] = alloc;
        return Any();
    }

    Any visitIdentifier(GrammarParser::IdentifierContext* context) override {
        const string& name = context->ID()->getText();
        auto var = symbolTable[name];
        if (var == nullptr)
            error(context, "Undefined variable " << name);
        return (llvm::Value*)builder.CreateLoad(var);
    }

    Any visitAssignment(GrammarParser::AssignmentContext* context) override {
        const string& name = context->name->getText();
        auto var = symbolTable[name];
        if (var == nullptr)
            error(context, "Undefined variable " << name);
        llvm::Value* rhs = visit(context->expression());
        builder.CreateStore(rhs, var);
        return Any();
    }

    Any visitConditional(GrammarParser::ConditionalContext* context) override {
        llvm::Value* condition = visit(context->condition);
        if (condition->getType() != boolType)
            error(context, "Condition must be a boolean");
        llvm::Function* llfunction = currentFunction->llfunction;
        llvm::BasicBlock* trueBranch = llvm::BasicBlock::Create(llvmContext,
                "then", llfunction);
        llvm::BasicBlock* falseBranch = llvm::BasicBlock::Create(llvmContext,
                "else");
        llvm::BasicBlock* after = llvm::BasicBlock::Create(llvmContext,
                "endif");
        builder.CreateCondBr(condition, trueBranch, falseBranch);
        builder.SetInsertPoint(trueBranch);
        visit(context->trueBranch);
        bool trueBranchTerminated = currentBlockTerminated;
        if (!trueBranchTerminated)
            builder.CreateBr(after);
        llfunction->getBasicBlockList().push_back(falseBranch);
        builder.SetInsertPoint(falseBranch);
        currentBlockTerminated = false;
        if (context->falseBranch)
            visit(context->falseBranch);
        bool falseBranchTerminated = currentBlockTerminated;
        if (!falseBranchTerminated)
            builder.CreateBr(after);
        currentBlockTerminated = trueBranchTerminated && falseBranchTerminated;
        if (!currentBlockTerminated) {
            llfunction->getBasicBlockList().push_back(after);
            builder.SetInsertPoint(after);
        }
        return Any();
    }

    Any visitWhileLoop(GrammarParser::WhileLoopContext* context) override {
        llvm::Function* llfunction = currentFunction->llfunction;
        llvm::BasicBlock* loop = llvm::BasicBlock::Create(llvmContext, "while",
                llfunction);
        llvm::BasicBlock* inner = llvm::BasicBlock::Create(llvmContext, "do");
        llvm::BasicBlock* after = llvm::BasicBlock::Create(llvmContext,
                "endwhile");
        builder.CreateBr(loop);
        builder.SetInsertPoint(loop);
        llvm::Value* condition = visit(context->condition);
        if (condition->getType() != boolType)
            error(context, "Condition must be a boolean");
        builder.CreateCondBr(condition, inner, after);
        llfunction->getBasicBlockList().push_back(inner);
        builder.SetInsertPoint(inner);
        visit(context->statements());
        if (!currentBlockTerminated)
            builder.CreateBr(loop);
        llfunction->getBasicBlockList().push_back(after);
        builder.SetInsertPoint(after);
        currentBlockTerminated = false;
        return Any();
    }

    Any visitForLoop(GrammarParser::ForLoopContext* context) override {
        llvm::Function* llfunction = currentFunction->llfunction;
        llvm::Value* from = visit(context->from);
        llvm::Value* to = visit(context->to);
        if (from->getType() != intType || to->getType() != intType)
            error(context, "For loop bounds must be of integer type");
        const string& varName = context->var->getText();
        llvm::Value* iterVar = builder.CreateAlloca(intType, nullptr, varName);
        builder.CreateStore(from, iterVar);
        llvm::BasicBlock* loop = llvm::BasicBlock::Create(llvmContext, "for",
                llfunction);
        llvm::BasicBlock* inner = llvm::BasicBlock::Create(llvmContext, "do");
        llvm::BasicBlock* after = llvm::BasicBlock::Create(llvmContext,
                "endfor");
        builder.CreateBr(loop);
        builder.SetInsertPoint(loop);
        llvm::Value* varValue = builder.CreateLoad(iterVar, varName + "_var");
        llvm::Value* condition = builder.CreateICmpSLT(varValue, to);
        builder.CreateCondBr(condition, inner, after);
        llfunction->getBasicBlockList().push_back(inner);
        builder.SetInsertPoint(inner);
        symbolTable.enterScope();
        symbolTable[varName] = iterVar;
        visit(context->statements());
        symbolTable.exitScope();
        if (!currentBlockTerminated) {
            llvm::Value* incVal = builder.CreateAdd(varValue,
                    llvm::ConstantInt::get(intType, 1));
            builder.CreateStore(incVal, iterVar);
            builder.CreateBr(loop);
        }
        llfunction->getBasicBlockList().push_back(after);
        builder.SetInsertPoint(after);
        currentBlockTerminated = false;
        return Any();
    }

private:

    llvm::Type* getType(antlr4::ParserRuleContext* context,
            GrammarParser::TypeContext* typeContext) {
        const string& typeName = typeContext->name->getText();
        auto it = types.find(typeName);
        if (it == types.end())
            error(context, "Type not found: " << typeName);
        llvm::Type* type = it->second;
        if (!typeContext->ptrDims)
            return type;
        unsigned dims = typeContext->ptrDims->getText().length();
        for (unsigned i = 0; i < dims; i++)
            type = llvm::PointerType::getUnqual(type);
        return type;
    }

    string processStringLiteral(const string& input) {
        string literal = input.substr(1, input.length() - 2);
        size_t i = 0;
        while ((i = literal.find('\\', i)) != std::string::npos) {
            string replacement;
            switch (literal[i + 1]) {
            case 'n':
                replacement = "\n";
                break;
            case 't':
                replacement = "\t";
                break;
            default:
                i++;
                continue;
            }
            literal.replace(i, 2, replacement);
            i++;
        }
        return literal;
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

    GrammarParser::ProgramFileContext* tree = parser.programFile();

    if (parser.getNumberOfSyntaxErrors())
        return 1;

    pa037::Visitor visitor;
    visitor.visitProgramFile(tree);

    return 0;
}
