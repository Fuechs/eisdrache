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

#include <string>
#include <vector>
#include <map>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Verifier.h>

namespace llvm {

struct WrappedVal {
    enum Kind {
        PARAMETER,  // function parameters
        COMPLEX,    // class / struct object
        GLOBAL,     // global variable
        LOCAL,      // local variable 
        LITERAL,    // (global) string literal
        NONE
    };

    typedef std::unordered_map<std::string, WrappedVal> Map;

    WrappedVal(Kind = NONE, Type *type = nullptr, Value *value = nullptr);
    WrappedVal &operator=(const WrappedVal &copy);
    
    Kind kind;
    Type *type;
    Value *value;
};

class Eisdrache {
public:
    ~Eisdrache();

    static Eisdrache *create(std::string moduleID);
    static Eisdrache *create(LLVMContext *context, std::string moduleID);
    static Eisdrache *create(Module *module);
    static Eisdrache *create(Module *module, IRBuilder<> *builder);

    // dumb the Module
    void dump(raw_fd_ostream &outs = errs());

    // getter functions
    LLVMContext *getContext();
    Module *getModule();
    IRBuilder<> *getBuilder();
    Type *getVoidTy();
    IntegerType *getSizeTy();
    IntegerType *getIntTy(size_t bit);
    PointerType *getIntPtrTy(size_t bit);
    PointerType *getIntPtrPtrTy(size_t bit);
    ConstantInt *getInt(IntegerType *type, size_t value);
    ConstantInt *getInt(size_t bit, size_t value);

    // builder functions
    Value *allocate(Type *type, std::string name = "");
    Value *call(Function *callee, std::vector<Value *> args = {}, std::string name = "");
    // `entry`: set insert point at this function
    Function *declare(Type *type, std::vector<Type *> args = {}, std::string = "", bool entry = false);
    // create return instruction and end function declaration 
    // or start insertion at `next`
    ReturnInst *createRet(Value *value, BasicBlock *next = nullptr);

    // get WrappedVal of pointer 
    WrappedVal &getWrap(Value *pointer);
    // load value if pointer is WrappedVal::LOCAL
    Value *loadValue(Value *pointer);

    // call TYPE *malloc (SIZE_T size)
    Value *malloc(Type *type, Value *size, std::string name = "");
    // call void free (TYPE *value)
    void free(Type *type, Value *value);
    // call TYPE *memcpy (TYPE *dest, TYPE *source, SIZE_T size)
    Value *memcpy(Type *type, Value *dest, Value *source, Value *size, std::string name = "");

    void createMemoryFunctions(Type *type);

private:
    typedef std::map<Type *, std::map<std::string, Function *>> MemoryFuncMap;

    Eisdrache(LLVMContext *, Module *, IRBuilder<> *);

    LLVMContext *context;
    Module *module;
    IRBuilder<> *builder;

    MemoryFuncMap memoryFunctions = MemoryFuncMap();
    WrappedVal::Map values;
};

}