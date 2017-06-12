#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"

#include "antlr4-runtime.h"
#include "GrammarLexer.h"
#include "GrammarParser.h"
#include "GrammarBaseVisitor.h"

#include <iostream>
#include <map>
#include <memory>

using antlrcpp::Any;
using namespace std;

llvm::cl::opt<string> outputFilename("o",
        llvm::cl::desc("Specify output filename"),
        llvm::cl::value_desc("filename"), llvm::cl::init("a.out"));
llvm::cl::opt<string> inputFilename(llvm::cl::Positional,
        llvm::cl::desc("<input file>"), llvm::cl::Required);
llvm::cl::opt<bool> outputAssembly("S", llvm::cl::desc("Output LLVM assembly"));

template<class T>
class SymbolTable {
private:
    std::vector<std::map<std::string, T>> tables;
public:
    SymbolTable() {
        tables.resize(1);
    }

    T& operator[](const std::string& name) {
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

llvm::LLVMContext llvmContext;
llvm::IRBuilder<> builder(llvmContext);
llvm::Module* module;
shared_ptr<Function> currentFunction;
bool currentBlockTerminated = false;
SymbolTable<llvm::Value*> symbolTable;
SymbolTable<shared_ptr<Function>> functionTable;

llvm::Type* intType = llvm::Type::getInt32Ty(llvmContext);
llvm::Type* floatType = llvm::Type::getDoubleTy(llvmContext);
llvm::Type* boolType = llvm::Type::getInt1Ty(llvmContext);
llvm::Type* charType = llvm::Type::getInt8Ty(llvmContext);
llvm::Type* voidType = llvm::Type::getVoidTy(llvmContext);
llvm::Type* stringType = llvm::PointerType::getUnqual(charType);

map<std::string, llvm::Type*> types { { "int", intType }, { "bool", boolType },
        { "char", charType }, { "float", floatType } };

#define error(context, message_expr) do { \
    cerr << "line " << (context)->start->getLine() << ": " << message_expr << endl; \
    exit(2); \
    throw 0; \
} while(0)

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

class LValueVisitor: public GrammarBaseVisitor {
private:
    GrammarVisitor* rvalueVisitor;
public:
    LValueVisitor(GrammarVisitor* rvalueVisitor) :
            rvalueVisitor(rvalueVisitor) {
    }

    Any visitIdentifier(GrammarParser::IdentifierContext* context) override {
        const string& name = context->ID()->getText();
        auto var = symbolTable[name];
        if (var == nullptr)
            error(context, "Undefined variable " << name);
        return (llvm::Value*) var;
    }

    Any visitSubscriptExpr(GrammarParser::SubscriptExprContext* context)
            override {
        llvm::Value* pointer = rvalueVisitor->visit(context->expr);
        llvm::Value* offset = rvalueVisitor->visit(context->subscript);
        if (!pointer->getType()->isPointerTy())
            error(context, "Subscript access on non-pointer");
        pointer = builder.CreateGEP(pointer, offset);
        return pointer;
    }

    Any visit(antlr4::tree::ParseTree* context) override {
        Any ret = GrammarBaseVisitor::visit(context);
        if (ret.isNull())
            error((antlr4::ParserRuleContext* )context,
                    "Address operand not an rvalue");
        return ret;
    }
};

class Visitor: public GrammarBaseVisitor {
private:
    LValueVisitor lvalueVisitor;
public:
    Visitor() :
            lvalueVisitor(this) {
    }

    Any visitProgramFile(GrammarParser::ProgramFileContext* context) override {
        module = new llvm::Module("main", llvmContext); // context gets ownership
        GrammarBaseVisitor::visitProgramFile(context);
        if (llvm::verifyModule(*module, &llvm::errs()))
            error(context, "LLVM module verification error");
        if (outputAssembly) {
            module->print(llvm::outs(), nullptr);
        } else {
            string outf = outputFilename + ".tmp";
            error_code err;
            {
                llvm::raw_fd_ostream out(outf, err, llvm::sys::fs::F_None);
                if (err)
                    error(context, "Failed to open output file: " << err);
                module->print(out, nullptr);
            }
            string cmd = "sh -c 'llc " + outf + " -o - | gcc -x assembler - -o "
                    + outputFilename + "'";
            system(cmd.c_str());
        }
        return Any();
    }

    Any visitType(GrammarParser::TypeContext* context) override {
        const string& typeName = context->name->getText();
        auto it = types.find(typeName);
        if (it == types.end())
            error(context, "Type not found: " << typeName);
        llvm::Type* type = it->second;
        if (!context->ptrDims())
            return type;
        unsigned dims = context->ptrDims()->getText().length();
        for (unsigned i = 0; i < dims; i++)
            type = llvm::PointerType::getUnqual(type);
        return type;
    }

    Any visitFndecl(GrammarParser::FndeclContext* context) override {
        const string& name = context->name->getText();
        vector<shared_ptr<Argument>> args;
        vector<llvm::Type*> lltypes;
        for (auto arg : context->arglist()->arg()) {
            llvm::Type* argType = visit(arg->type());
            args.push_back(
                    make_shared<Argument>(arg->name->getText(), argType));
            lltypes.push_back(argType);
        }
        llvm::Type* type =
                (context->type()) ?
                        (llvm::Type*) visit(context->type()) : voidType;
        llvm::FunctionType *functionType = llvm::FunctionType::get(type,
                lltypes, context->variadic);

        llvm::Function *llfunction = llvm::Function::Create(functionType,
                llvm::Function::ExternalLinkage, name, module);
        shared_ptr<Function> function = make_shared<Function>(llfunction, type,
                args);
        functionTable[name] = function;
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
            error(context, "LLVM function verification error");
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
        shared_ptr<Function> function = functionTable[name];
        if (!function)
            error(context, "Function " << name << " not declared");
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

    Any visitIntegerLiteral(GrammarParser::IntegerLiteralContext* context)
            override {
        auto value = std::stoi(context->value->getText());
        return (llvm::Value*) llvm::ConstantInt::get(intType, value);
    }

    Any visitFloatLiteral(GrammarParser::FloatLiteralContext* context)
            override {
        double value = std::stod(context->value->getText());
        return (llvm::Value*) llvm::ConstantFP::get(floatType, value);
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

    Any visitBooleanLiteral(GrammarParser::BooleanLiteralContext* context)
            override {
        int value = (context->value->getText() == "true") ? 1 : 0;
        return (llvm::Value*) llvm::ConstantInt::get(llvmContext,
                llvm::APInt(1, value, true));
    }

    Any visitCastExpr(GrammarParser::CastExprContext* context) override {
        llvm::Value* value = visit(context->expr);
        llvm::Type* sourceType = value->getType();
        llvm::Type* targetType = visit(context->type());
        if (targetType == floatType) {
            if (sourceType == intType)
                return builder.CreateSIToFP(value, targetType);
        } else if (targetType == intType) {
            if (sourceType == floatType)
                return builder.CreateFPToSI(value, targetType);
        }
        error(context, "Cast not allowed for given types");
    }

    Any visitParenExpr(GrammarParser::ParenExprContext* context) override {
        return visit(context->expression());
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
        llvm::Type* type = left->getType();
        if (type != intType && type != floatType)
            error(context, "Unsupported type for " << op1);
        llvm::Value* value;
#define _insnp(name, iprefix) (type != floatType) ? builder.Create##iprefix##name(left, right) : builder.CreateF##name(left, right)
#define _insn(name) _insnp(name, )
#define _insncmpp(cmp, iprefix) (type != floatType) ? builder.CreateICmp##iprefix##cmp(left, right) : builder.CreateFCmpO##cmp(left, right)
#define _insncmp(cmp) _insncmpp(cmp, )
#define _insncmps(cmp) _insncmpp(cmp, S)
        switch (op1) {
        case '+':
            value = _insn(Add);
            break;
        case '-':
            value = _insn(Sub);
            break;
        case '*':
            value = _insn(Mul);
            break;
        case '/':
            value = _insnp(Div, S);
            break;
        case '!': // !=
            value = _insncmp(NE);
            break;
        case '=': // ==
            value = _insncmp(EQ);
            break;
        case '<':
            if (op2) // <=
                value = _insncmps(LE);
            else
                // <
                value = _insncmps(LT);
            break;
        case '>':
            if (op2) // >=
                value = _insncmps(GE);
            else
                // >
                value = _insncmps(GT);
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
        if (context->initializer)
            initializer = visit(context->initializer);
        llvm::Type* type;
        if (context->type()) {
            type = visit(context->type());
        } else if (initializer) {
            type = initializer->getType();
        } else {
            error(context,
                    "Variable declaration needs to have a type or an initializer");
        }
        if (context->arrayDim) {
            llvm::Value* arrayDim = visit(context->arrayDim);
            initializer = builder.CreateAlloca(type, arrayDim);
            type = llvm::PointerType::getUnqual(type);
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
        return (llvm::Value*) builder.CreateLoad(var);
    }

    Any visitAddrExpr(GrammarParser::AddrExprContext* context) override {
        llvm::Value* value = lvalueVisitor.visit(context->expression());
        return value;
    }

    Any visitSubscriptExpr(GrammarParser::SubscriptExprContext* context)
            override {
        llvm::Value* pointer = visit(context->expr);
        llvm::Value* offset = visit(context->subscript);
        if (!pointer->getType()->isPointerTy())
            error(context, "Subscript access on non-pointer");
        pointer = builder.CreateGEP(pointer, offset);
        llvm::Value* value = builder.CreateLoad(pointer);
        return value;
    }

    Any visitAssignment(GrammarParser::AssignmentContext* context) override {
        llvm::Value* rhs = visit(context->rhs);
        llvm::Value* addr = lvalueVisitor.visit(context->lhs);
        builder.CreateStore(rhs, addr);
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
        llvm::Value* condition;
        bool increasing = context->op->getText() == "to";
        if (increasing)
            condition = builder.CreateICmpSLT(varValue, to);
        else
            condition = builder.CreateICmpSGT(varValue, to);
        builder.CreateCondBr(condition, inner, after);
        llfunction->getBasicBlockList().push_back(inner);
        builder.SetInsertPoint(inner);
        symbolTable.enterScope();
        symbolTable[varName] = iterVar;
        visit(context->statements());
        symbolTable.exitScope();
        if (!currentBlockTerminated) {
            llvm::Value* incVal = builder.CreateAdd(varValue,
                    llvm::ConstantInt::get(intType, (increasing) ? 1 : -1));
            builder.CreateStore(incVal, iterVar);
            builder.CreateBr(loop);
        }
        llfunction->getBasicBlockList().push_back(after);
        builder.SetInsertPoint(after);
        currentBlockTerminated = false;
        return Any();
    }
};

int main(int argc, const char* argv[]) {
    llvm::cl::ParseCommandLineOptions(argc, argv);

    std::ifstream stream;
    stream.open(inputFilename);
    antlr4::ANTLRInputStream input(stream);
    GrammarLexer lexer(&input);
    antlr4::CommonTokenStream tokens(&lexer);
    GrammarParser parser(&tokens);

    GrammarParser::ProgramFileContext* tree = parser.programFile();

    if (parser.getNumberOfSyntaxErrors())
        return 1;

    Visitor visitor;
    visitor.visitProgramFile(tree);

    return 0;
}
