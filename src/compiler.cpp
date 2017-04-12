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

class Argument {
public:
    string name;
    shared_ptr<Type> type;
    Argument(const string& name, shared_ptr<Type> type) :
            name(name), type(type) {
    }
};

class Function {
public:
    llvm::Function* llfunction;
    shared_ptr<Type> type;
    vector<shared_ptr<Argument>> args;
    Function(llvm::Function* llfunction, shared_ptr<Type> type,
            vector<shared_ptr<Argument>> args) :
            llfunction(llfunction), type(type), args(args) {
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
        vector<shared_ptr<Argument>> args;
        vector<llvm::Type*> lltypes;
        for (auto arg : context->arglist()->arg()) {
            auto argType = getType(context, arg->type->getText());
            args.push_back(
                    make_shared<Argument>(arg->name->getText(), argType));
            lltypes.push_back(argType->lltype);
        }
        auto type = getType(context, context->type->getText());
        llvm::FunctionType *functionType = llvm::FunctionType::get(type->lltype,
                lltypes, false);

        llvm::Function *llfunction = llvm::Function::Create(functionType,
                llvm::Function::ExternalLinkage, name, module);
        shared_ptr<Function> function = make_shared<Function>(llfunction, type, args);
        functions[name] = function;
        return function;
    }

    Any visitFunction(GrammarParser::FunctionContext* context) override {
        shared_ptr<Function> function = visitFndecl(context->fndecl());
        llvm::BasicBlock *block = llvm::BasicBlock::Create(llvmContext, "entry",
                function->llfunction);
        builder.SetInsertPoint(block);
        auto it = function->llfunction->args().begin();
        for (auto arg : function->args) {
            auto& fnArg = *it;
            fnArg.setName(arg->name);
            auto alloc = builder.CreateAlloca(fnArg.getType(), nullptr,
                    arg->name + "_var");
            builder.CreateStore(&fnArg, alloc);
            symbolTable[arg->name] = make_shared<Expression>(arg->type, alloc);
            it++;
        }
        shared_ptr<Expression> expr = visit(context->statements());
        if (expr->type != function->type)
            error("Return value doesn't match type");
        builder.CreateRet(expr->value);
        llvm::verifyFunction(*function->llfunction);
        function = nullptr;
        return Any();
    }

    Any visitCall(GrammarParser::CallContext* context) override {
        const string& name = context->name->getText();
        shared_ptr<Function> function = functions[name];
        if (!function)
            error("Function " << name << "not declared");
        vector<llvm::Value*> args;
        vector<shared_ptr<Type>> expectedTypes;
        for (auto arg:function->args) {
            expectedTypes.push_back(arg->type);
        }
        vector<shared_ptr<Type>> actualTypes;
        for (auto expr:context->expression()) {
            shared_ptr<Expression> arg = visit(expr);
            args.push_back(arg->value);
            actualTypes.push_back(arg->type);
        }
        if (actualTypes != expectedTypes) {
            error("Incorect function arguments");
        }
        llvm::Value* value = builder.CreateCall(function->llfunction, args);
        return make_shared<Expression>(function->type, value);
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
        char op1 = context->op->getText()[0];
        char op2 = 0;
        if (context->op->getText().length() == 2)
            op2 = context->op->getText()[1];
        if (left->type != right->type)
            error("Incompatible types");
        if (left->type != intType)
            error("Unsupported type for " << op1);
        llvm::Value* value;
        shared_ptr<Type> type = left->type;
        switch (op1) {
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
        case '!': // !=
            type = boolType;
            value = builder.CreateICmpNE(left->value, right->value);
            break;
        case '=': // ==
            type = boolType;
            value = builder.CreateICmpEQ(left->value, right->value);
            break;
        case '<':
            type = boolType;
            if (op2) // <=
                value = builder.CreateICmpSLE(left->value, right->value);
            else
                // <
                value = builder.CreateICmpSLT(left->value, right->value);
            break;
        case '>':
            type = boolType;
            if (op2) // >=
                value = builder.CreateICmpSGE(left->value, right->value);
            else
                // >
                value = builder.CreateICmpSGT(left->value, right->value);
            break;
        default:
            error("BUG Unknown operator");
        }
        return make_shared<Expression>(type, value);
    }

    Any visitLogicExpr(GrammarParser::LogicExprContext* context) override {
        shared_ptr<Expression> left = visit(context->expression(0));
        shared_ptr<Expression> right = visit(context->expression(1));
        char op = context->op->getText()[0];
        if (left->type != boolType || right->type != boolType)
            error(op << " requires boolean operands");
        llvm::Value* value;
        switch (op) {
        case 'a':
            value = builder.CreateAnd(left->value, right->value);
            break;
        case 'o':
            value = builder.CreateOr(left->value, right->value);
            break;
        }
        return make_shared<Expression>(boolType, value);
    }

    Any visitNotExpr(GrammarParser::NotExprContext* context) override {
        shared_ptr<Expression> expr = visit(context->expression());
        if (expr->type != boolType)
            error("Negation operand not boolean");
        llvm::Value* value = builder.CreateICmpEQ(expr->value,
                llvm::ConstantInt::getFalse(llvmContext));
        return make_shared<Expression>(boolType, value);
    }

    Any visitDeclaration(GrammarParser::DeclarationContext* context) override {
        const string& name = context->name->getText();
        shared_ptr<Expression> initializer = nullptr;
        if (context->expression())
            initializer = visit(context->expression());
        shared_ptr<Type> type;
        if (context->type) {
            type = getType(context, context->type->getText());
        } else if (initializer) {
            type = initializer->type;
        } else {
            error(
                    "Variable declaration needs to have a type or an initializer");
        }
        auto alloc = builder.CreateAlloca(type->lltype, nullptr, name);
        if (initializer) {
            if (initializer->type != type)
                error("Incompatible type in initialization");
            builder.CreateStore(initializer->value, alloc);
        }
        symbolTable[name] = make_shared<Expression>(type, alloc);
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
