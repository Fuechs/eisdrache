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

class Eisdrache {
public:
    ~Eisdrache();

    static Eisdrache *create(std::string moduleID);
    static Eisdrache *create(LLVMContext *context, std::string moduleID);
    static Eisdrache *create(Module *module);
    static Eisdrache *create(Module *module, IRBuilder<> *builder);

    // dumb the Module
    void dump(raw_fd_ostream & = errs());

    // getter functions
    LLVMContext *getContext();
    Module *getModule();
    IRBuilder<> *getBuilder();
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

    // call TYPE *malloc (SIZE_T size)
    Value *malloc(Type *type, Value *size, std::string name = "");
    // call void free (TYPE *value)
    void free(Type *type, Value *value);
    // call TYPE *memcpy (TYPE *dest, TYPE *source, SIZE_T size)
    Value *memcpy(Type *type, Value *dest, Value *source, Value *size, std::string name = "");

    void createMemoryFunctions(Type *type);

private:
    Eisdrache(LLVMContext *, Module *, IRBuilder<> *);

    LLVMContext *context;
    Module *module;
    IRBuilder<> *builder;

    std::map<Type *, std::map<std::string, Function *>> memoryFunctions = std::map<Type *, std::map<std::string, Function *>>();
};

}