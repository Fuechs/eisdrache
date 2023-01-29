/**
 * @file llvm_array.hpp
 * @author fuechs
 * @brief llvm dynamic array type
 * @version 0.1
 * @date 2023-01-29
 * 
 * @copyright Copyright (c) 2023, Fuechs.
 * 
 */

#pragma once

#include <iostream>
#include <string>
#include <vector>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Verifier.h>

namespace llvmarr {

using std::cout, std::cerr, std::endl, 
    std::string, std::to_string, std::vector;

using llvm::LLVMContext, llvm::Module, llvm::IRBuilder, llvm::Type, llvm::StructType;

class Array {
public:
    Array(LLVMContext *context, Module *module, IRBuilder<> *builder, Type *type);
    ~Array();

    Type *getPointerTo();
    StructType *getTy();
    Type *getElementType();

private:
    Type *elementType = nullptr;    // type of elements
    StructType *self = nullptr;     // this array type
    Type *ptr = nullptr;            // pointer to this array type
};

#ifdef LLVM_ARRAY_IMPL 

Array::Array(LLVMContext *context, Module *module, IRBuilder<> *builder, Type *type) : elementType(type) {}
Array::~Array() {}

Type *Array::getPointerTo() { return ptr; }
StructType *Array::getTy() { return self; }
Type *Array::getElementType() { return elementType; }

#endif // LLVM_ARRAY_IMPL

}