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
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Value.h>

namespace llvmarr {

using std::cout, std::cerr, std::endl, 
    std::string, std::to_string, std::vector;

using llvm::LLVMContext, llvm::Module, llvm::IRBuilder, llvm::Type, llvm::StructType,
    llvm::Function, llvm::FunctionType, llvm::BasicBlock, llvm::Value, llvm::ConstantPointerNull;

class Array {
public:
    Array(LLVMContext *context, Module *module, IRBuilder<> *builder, Type *type, string name);
    ~Array();

    Value *allocate(string name = "");
    void initialize(Value *array);

    Type *getPointerTo();
    StructType *getTy();
    Type *getElementTy();

    Function *create_empty;
    Function *delete_array;

private:
    Type *elementType = nullptr;    // type of elements
    StructType *self = nullptr;     // this array type
    Type *ptr = nullptr;            // pointer to this array type
    string name = "";

    IRBuilder<> *builder;
};

#ifdef LLVM_ARRAY_IMPL 

Array::Array(LLVMContext *context, Module *module, IRBuilder<> *builder, Type *type, string name) 
: builder(builder), elementType(type) {
    self = StructType::create(*context, {
        elementType->getPointerTo(),    // type *buffer;
        builder->getInt64Ty(),          // i64 length;
        builder->getInt64Ty(),          // i64 maxlength;
        builder->getInt64Ty(),          // i64 factor; amount to preallocate when growing
    });
    if (!name.empty())
        self->setName(name);
    this->name = self->getName();
    ptr = self->getPointerTo();

    { // create_empty
    FunctionType *FT = FunctionType::get(builder->getVoidTy(), {ptr}, false);
    create_empty = Function::Create(FT, Function::ExternalLinkage, name+"_create_empty", *module);
    create_empty->setCallingConv(llvm::CallingConv::Fast);
    create_empty->setDoesNotThrow();
    BasicBlock *BB = BasicBlock::Create(*context, "entry", create_empty);

    builder->SetInsertPoint(BB);
    Value *buffer_ptr = builder->CreateGEP(self, create_empty->getArg(0), 
        {builder->getInt64(0), builder->getInt32(0)}, "buffer_ptr");
    Value *length_ptr = builder->CreateGEP(self, create_empty->getArg(0), 
        {builder->getInt64(0), builder->getInt32(1)}, "length_ptr");
    Value *maxlength_ptr = builder->CreateGEP(self, create_empty->getArg(0), 
        {builder->getInt64(0), builder->getInt32(2)}, "maxlength_ptr");
    Value *factor_ptr = builder->CreateGEP(self, create_empty->getArg(0), 
        {builder->getInt64(0), builder->getInt32(3)}, "factor_ptr");
    builder->CreateStore(ConstantPointerNull::get(builder->getInt8PtrTy()), buffer_ptr);
    builder->CreateStore(builder->getInt64(0), length_ptr);
    builder->CreateStore(builder->getInt64(0), maxlength_ptr);
    builder->CreateStore(builder->getInt64(16), maxlength_ptr);

    builder->CreateRetVoid();

    llvm::verifyFunction(*create_empty);
    }

    {

    }
}

Array::~Array() {}

Value *Array::allocate(string name) { return builder->CreateAlloca(self, nullptr, name); }
void Array::initialize(Value *array) { builder->CreateCall(this->create_empty, {array}); }

Type *Array::getPointerTo() { return ptr; }
StructType *Array::getTy() { return self; }
Type *Array::getElementTy() { return elementType; }

#endif // LLVM_ARRAY_IMPL

}