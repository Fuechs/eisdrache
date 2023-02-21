/**
 * @file eisdrache.hpp
 * @author fuechs
 * @brief Eisdrache class header
 * @version 0.1
 * @date 2023-01-30
 * 
 * @copyright Copyright (c) 2023, Fuechs.
 * 
 */

#pragma once

#include <iostream>
#include <string>
#include <vector>
#include <map>

#include <llvm/PassRegistry.h>
#include "llvm/InitializePasses.h"
#include <llvm/ADT/Triple.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include "llvm/Support/TargetSelect.h"
#include <llvm/MC/TargetRegistry.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>

namespace llvm {

struct WrappedVal {
    enum Kind {
        PARAMETER,  // function parameter
        COMPLEX,    // class / struct object
        LOADED,     // (already) loaded values
        GLOBAL,     // global variable
        LOCAL,      // local variable 
        LITERAL,    // (global) string literal
        NONE
    };

    typedef std::vector<WrappedVal> Vec;

    WrappedVal(Kind = NONE, Type *type = nullptr, Value *value = nullptr, BasicBlock *parent = nullptr);
    WrappedVal &operator=(const WrappedVal &copy);
    
    Kind kind;
    Type *type;
    Value *value;
    // TODO: add future function call (for struct types, etc.)
    Value *future; // value to assign when Value gets referenced (relevant for Kind::LOCAL)
    BasicBlock *parent;
};

struct WrappedType {
    typedef std::unordered_map<std::string, WrappedType> Map;

    WrappedType(StructType *type = nullptr, std::vector<Type *> elementTypes = {});
    WrappedType &operator=(const WrappedType &copy);
    // index < 0 returns StructType *
    Type *operator[](int64_t index);


    StructType *type;
    std::vector<Type *> elementTypes;
};

class Eisdrache {
public:
    enum BinaryOp {
        ADD,        // + addition
        SUB,        // - subtraction
        MUL,        // * multiplication
        DIV,        // / division
        XOR,        // ^ bit xor
        OR,         // | bit or
        AND,        // & bit and
        LSH,        // << left bit shift
        RSH,        // >> right bit shift
        EQU,        // == equals
        NEQ,        // != not equals
        GT,         // > greater than
        GTE,        // >= greater than or equals
        LT,         // < less than
        LTE         // <= less than or equals
    };

    ~Eisdrache();

    static void init();

    static Eisdrache *create(std::string moduleID, std::string targetTriple = "");
    static Eisdrache *create(LLVMContext *context, std::string moduleID, std::string targetTriple = "");
    static Eisdrache *create(Module *module, std::string targetTriple = "");
    static Eisdrache *create(Module *module, IRBuilder<> *builder, std::string targetTriple = "");

    // dumb the Module
    void dump(raw_fd_ostream &outs = errs());

    /// getter functions ///
    
    LLVMContext *getContext();
    Module *getModule();
    IRBuilder<> *getBuilder();
    
    Type *getVoidTy();
    Type *getBoolTy();
    IntegerType *getSizeTy();
    IntegerType *getIntTy(size_t bit);
    PointerType *getIntPtrTy(size_t bit);
    PointerType *getIntPtrPtrTy(size_t bit);
    Type *getFloatTy(size_t bit);
    PointerType *getFloatPtrTy(size_t bit);
    PointerType *getFloatPtrPtrTy(size_t bit);

    ConstantInt *getBool(bool value);
    ConstantInt *getInt(IntegerType *type, size_t value);
    ConstantInt *getInt(size_t bit, size_t value);
    ConstantFP *getFloat(double value);
    ConstantPointerNull *getNullPtr(PointerType *type);

    /// builder functions ///

    Value *allocate(Type *type, std::string name = "");
    Value *call(Function *callee, std::vector<Value *> args = {}, std::string name = "");
    // `entry`: set insert point at this function
    Function *declare(Type *type, std::vector<Type *> args = {}, std::string name = "", bool entry = false);
    // create void return and end function declaration
    // or start insertion at `next`
    ReturnInst *createRet(BasicBlock *next = nullptr);
    // create return instruction and end function declaration 
    // or start insertion at `next`
    ReturnInst *createRet(Value *value, BasicBlock *next = nullptr);
    // create a struct type
    StructType *createType(std::vector<Type *> elements, std::string name = "");
    // get pointer to element of a struct
    Value *getElementPtr(Value *ptr, size_t index, std::string name = "");
    // get value of element of a struct
    Value *getElementVal(Value *ptr, size_t index, std::string name = "");
    // store value at element of structPtr 
    void store(Value *value, Value *structPtr, size_t index);
    // store value at pointer
    void store(Value *value, Value *ptr);
    // jump to block
    BranchInst *jump(BasicBlock *block);
    // condition jump to block
    BranchInst *condJump(Value *condition, BasicBlock *then, BasicBlock *Else);
    // set current insert point 
    void setBlock(BasicBlock *block);
    // create new block with current parent 
    // `insert`: start insertion)
    BasicBlock *block(bool insert = false, std::string name = "");
    // create a binary op 
    Value *binaryOp(BinaryOp op, Value *LHS, Value *RHS, std::string name = "");
    // convert / cast a value to a different type
    Value *convert(Type *type, Value *value, std::string name = "");
    // create a global string literal
    Constant *literal(std::string value, std::string name = "");

    // get WrappedVal of pointer 
    WrappedVal &getWrap(Value *pointer);
    // get WrappedType of struct type
    WrappedType &getWrap(Type *type);
    // load value if pointer is WrappedVal::LOCAL
    Value *loadValue(Value *pointer, std::string name = "", bool force = false);
    // load value of a given type
    Value *loadValue(Type *type, Value *pointer, std::string name = "");
    // leave future value for local variable
    void setFuture(Value *local, Value *value);
    // check if a value is a constant value
    bool isConstant(Value *value);
    // check if a value is a function parameter
    bool isArgument(Value *value);
    // get element type if type is a pointer type
    // doesn't work on i8**, etc.
    Type *getElementType(Type *type);
 
    // call TYPE *malloc (SIZE_T size)
    Value *malloc(Type *type, Value *size, std::string name = "");
    // call void free (TYPE *value)
    void free(Type *type, Value *value);
    // call TYPE *memcpy (TYPE *dest, TYPE *source, SIZE_T size)
    Value *memcpy(Type *type, Value *dest, Value *source, Value *size, std::string name = "");

    void createMemoryFunctions(Type *type);

private:
    typedef std::map<Type *, std::map<std::string, Function *>> MemoryFuncMap;

    Eisdrache(LLVMContext *, Module *, IRBuilder<> *, std::string);

    LLVMContext *context;
    Module *module;
    IRBuilder<> *builder;

    MemoryFuncMap memoryFunctions = MemoryFuncMap();
    WrappedVal::Vec values;
    WrappedType::Map types;
};

}