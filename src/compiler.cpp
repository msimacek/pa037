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
#include <memory>

using antlrcpp::Any;
using namespace std;

namespace pa037 {

class Type {
public:
    llvm::Type* lltype;
    Type(llvm::Type* lltype) :
            lltype(lltype) {
    }
};

enum ValueCategory {
    RVALUE, LVALUE
};

class Expression {
public:
    shared_ptr<Type> type;
    llvm::Value* value;
    ValueCategory category;

    Expression(shared_ptr<Type> type, llvm::Value* value = nullptr,
            ValueCategory category = RVALUE) :
            type(type), value(value), category(category) {
    }
};

class SymbolTable {
private:
    std::map<std::string, std::shared_ptr<Expression>> table;
public:

    std::shared_ptr<Expression>& operator[](const std::string& name) {
        return table[name];
    }
};

class Function {
public:
    llvm::Function* llfunction;
    Function(llvm::Function* llfunction) :
            llfunction(llfunction) {
    }
};

class Visitor: public GrammarBaseVisitor {
private:
    llvm::LLVMContext llvmContext;
    llvm::IRBuilder<> builder;
    llvm::Module* module;
    llvm::Function* currentFunction;
    SymbolTable symbolTable;
    map<std::string, shared_ptr<Function>> functions;

    shared_ptr<Type> intType = make_shared<Type>(
            llvm::Type::getInt32Ty(llvmContext));
    shared_ptr<Type> boolType = make_shared<Type>(
            llvm::Type::getInt1Ty(llvmContext));

    map<std::string, shared_ptr<Type>> types { { "int", intType }, { "bool",
            boolType } };

public:

    Visitor() :
            builder(llvmContext), module(nullptr), currentFunction(nullptr) {
    }

#define error(message_expr) do { \
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
        std::vector<llvm::Type*> argtypes;
        for (auto arg : context->arglist()->arg()) {
            auto type = getType(context, arg->type->getText());
            argtypes.push_back(type->lltype);
        }
        auto type = getType(context, context->type->getText());
        llvm::FunctionType *functionType = llvm::FunctionType::get(type->lltype,
                argtypes, false);

        llvm::Function *function = llvm::Function::Create(functionType,
                llvm::Function::ExternalLinkage, name, module);
        //symbolTable[name] = make_shared<Expression>(FUNCTION, function);
        return function;
    }

    Any visitFunction(GrammarParser::FunctionContext* context) override {
        llvm::Function* function = visitFndecl(context->fndecl());
        llvm::BasicBlock *block = llvm::BasicBlock::Create(llvmContext, "entry",
                function);
        builder.SetInsertPoint(block);
        auto it = function->args().begin();
        for (auto arg : context->fndecl()->arglist()->arg()) {
            const string& argname = arg->name->getText();
            auto type = getType(context, arg->type->getText());
            auto& fnArg = *it;
            fnArg.setName(argname);
            auto alloc = builder.CreateAlloca(fnArg.getType());
            builder.CreateStore(&fnArg, alloc);
            symbolTable[argname] = make_shared<Expression>(type, alloc);
            it++;
        }
        shared_ptr<Expression> expr = visit(context->statements());
        builder.CreateRet(expr->value);
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

    Any visitIntegerLiteral(GrammarParser::IntegerLiteralContext* ctx)
            override {
        long long value = std::stoi(ctx->value->getText());
        return make_shared<Expression>(intType,
                llvm::ConstantInt::get(llvmContext,
                        llvm::APInt(32, value, true)));
    }

    Any visitBooleanLiteral(GrammarParser::BooleanLiteralContext* ctx)
            override {
        int value = (ctx->value->getText() == "true") ? 1 : 0;
        return make_shared<Expression>(boolType,
                llvm::ConstantInt::get(llvmContext,
                        llvm::APInt(1, value, true)));
    }

    Any visitArithExpr(GrammarParser::ArithExprContext* context) override {
        shared_ptr<Expression> left = visit(context->expression(0));
        shared_ptr<Expression> right = visit(context->expression(1));
        char op = context->op->getText()[0];
        if (left->type != right->type)
            error("Incompatible types");
        auto type = left->type;
        if (type != intType)
            error("Unsupported type for " << op);
        llvm::Value* value;
        switch (op) {
        case '+':
            value = builder.CreateAdd(left->value, right->value);
            break;
        case '-':
            value = builder.CreateSub(left->value, right->value);
            break;
        case '*':
            value = builder.CreateMul(left->value, right->value);
            break;
        case '/':
            value = builder.CreateSDiv(left->value, right->value);
            break;
        default:
            error("BUG Unknown operator");
        }
        return make_shared<Expression>(type, value);
    }

    Any visitDeclaration(GrammarParser::DeclarationContext* context) override {
        shared_ptr<Expression> initializer = nullptr;
        if (context->expression())
            initializer = visit(context->expression());
        allocateVar(context, context->name->getText(), context->type->getText(),
                initializer);
        return Any();
    }

    Any visitIdentifier(GrammarParser::IdentifierContext* context) override {
        const string& name = context->ID()->getText();
        auto var = symbolTable[name];
        if (var == nullptr)
            error("Undefined variable " << name);
        return make_shared<Expression>(var->type,
                builder.CreateLoad(var->value), LVALUE);
    }

    Any visitAssignment(GrammarParser::AssignmentContext* context) override {
        const string& name = context->name->getText();
        auto var = symbolTable[name];
        if (var == nullptr)
            error("Undefined variable " << name);
        shared_ptr<Expression> rhs = visit(context->expression());
        builder.CreateStore(rhs->value, var->value);
        return Any();
    }

private:

    shared_ptr<Type> getType(antlr4::ParserRuleContext* context,
            const string& typeName) {
        auto it = types.find(typeName);
        if (it == types.end())
            error("Type not found: " << typeName);
        return it->second;
    }

    void allocateVar(antlr4::ParserRuleContext* context, const string& name,
            const string& typeName,
            shared_ptr<Expression> initializer = nullptr) {
        auto type = getType(context, typeName);
        auto alloc = builder.CreateAlloca(type->lltype, nullptr, name);
        if (initializer) {
            if (initializer->type != type)
                error("Incompatible type in initialization");
            builder.CreateStore(initializer->value, alloc);
        }
        symbolTable[name] = make_shared<Expression>(type, alloc);
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
