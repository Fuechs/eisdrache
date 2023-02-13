/**
 * @file eisdrache.hpp
 * @author fuechs
 * @brief Eisdrache Wrapper class header
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

class Wrapper {
public:
    ~Wrapper();

    static Wrapper *create(std::string);
    static Wrapper *create(LLVMContext *, std::string);
    static Wrapper *create(Module *);
    static Wrapper *create(Module *, IRBuilder<> *);

    // create the main function and return pointer to it
    Function *createMain();

    // dumb the Module
    void dump(raw_fd_ostream & = errs());

    // getter functions
    LLVMContext *getContext();
    Module *getModule();
    IRBuilder<> *getBuilder();
    IntegerType *getSizeTy();
    IntegerType *getIntTy(size_t);
    PointerType *getIntPtrTy(size_t);
    PointerType *getIntPtrPtrTy(size_t);

    // builder functions
    Value *allocate(Type *, std::string = "");
    Value *call(Function *, std::vector<Value *> = {}, std::string = "");
    Function *declare(Type *, std::vector<Type *> = {}, std::string = "");

    // call TYPE *malloc (SIZE_T size)
    Value *malloc(Type *, Value *, std::string = "");
    // call void free (TYPE *value)
    void free(Type *, Value *);
    // call TYPE *memcpy (TYPE *dest, TYPE *source, SIZE_T size)
    Value *memcpy(Type *, Value *, Value *, Value *, std::string = "");

    void createMemoryFunctions(Type *);

private:
    Wrapper(LLVMContext *, Module *, IRBuilder<> *);

    LLVMContext *context;
    Module *module;
    IRBuilder<> *builder;

    Function *main = nullptr;
    std::map<Type *, std::map<std::string, Function *>> memoryFunctions = std::map<Type *, std::map<std::string, Function *>>();
};

}