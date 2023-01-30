/**
 * @file wrapper.hpp
 * @author fuechs
 * @brief LLVM Wrapper Wrapper class
 * @version 0.1
 * @date 2023-01-30
 * 
 * @copyright Copyright (c) 2023, Fuechs.
 * 
 */

#pragma once

#include <string>

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

    IntegerType *getIntTy(size_t);
    PointerType *getIntPtrTy(size_t);

private:
    Wrapper(LLVMContext *, Module *, IRBuilder<> *);

    LLVMContext *context;
    Module *module;
    IRBuilder<> *builder;

    Function *main = nullptr;
};

}