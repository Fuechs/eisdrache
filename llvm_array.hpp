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
    llvm::Function, llvm::FunctionType, llvm::BasicBlock, llvm::Value, llvm::ConstantPointerNull,
    llvm::PointerType;

class Memory {
public:
    Memory(LLVMContext *context, Module *module, IRBuilder<> *builder, Type *type);
    ~Memory();

    // call TYPE* malloc(i64)
    Value *malloc(Value *size, string name = "");
    // call void free(TYPE*)
    void free(Value *pointer);
    // call TYPE* memcpy(TYPE*, TYPE*, i64)
    Value *memcpy(Value *dest, Value *source, Value *size);

    LLVMContext *getContext();
    Module *getModule();
    IRBuilder<> *getBuilder();
    Type *getType();

private:
    Function *_malloc = nullptr;
    Function *_free = nullptr;
    Function *_memcpy = nullptr;

    Type *type = nullptr;
    Type *ptr = nullptr;

    LLVMContext *context;
    Module *module;
    IRBuilder<> *builder = nullptr;
};

class Array {
public:
    Array(Memory *mem, string name);
    ~Array();

    // allocate memory for this type
    // name: name for returned pointer
    Value *allocate(string name = "");
    // initialize object of this type
    // array: pointer to object
    void initialize(Value *array);
    // delete object of this type
    // array: pointer to object
    void del(Value *array);
    // get buffer of object of this type
    // array: pointer to object
    // name: name for returned pointer
    Value *getBuffer(Value *array, string name = "");
    // get buffer length/size of this object of this type
    // array: pointer to object
    // name: name for returned value
    Value *getLength(Value *array, string name = "");
    // copy buffer from `from` to `to`
    // from: pointer to object of this type
    // to: pointer to object of this type
    void copy(Value *from, Value *to);
    // create duplicate of object of this type
    // array: pointer to object
    // name: name for returned pointer
    Value *duplicate(Value *array, string name = "");

    // array* (pointer to this type)
    PointerType *getPointerTo();
    // array (this array type)
    StructType *getTy();
    // type of array elements
    Type *getElementTy();
    // get type of buffer
    PointerType *getElementPtrTy();


private:
    Type *elementType = nullptr;        // type of elements
    PointerType *elementPtr = nullptr;  // type of buffer (pointer to element type)
    StructType *self = nullptr;         // this array type
    PointerType *ptr = nullptr;         // pointer to this array type
    string name = "";                   // name of this array type

    Function *get_buffer = nullptr;    
    Function *get_length = nullptr;
    Function *resize = nullptr;    
    Function *push_back = nullptr; // TODO: push_back
    Function *pop_back = nullptr;  // TODO: pop_back
    Function *push_front = nullptr; // TODO: push_front
    Function *pop_front = nullptr; // TODO: pop_front
    
    Function *create_empty = nullptr;   // default constructor
    Function *create_copy = nullptr;    // TODO: copy constructor
    Function *delete_array = nullptr;   // destructor

    LLVMContext *context;
    Module *module;
    IRBuilder<> *builder;
};

#ifdef LLVM_ARRAY_IMPL 

Memory::Memory(LLVMContext *context, Module *module, IRBuilder<> *builder, Type *type) {
    this->context = context;
    this->module = module;
    this->builder = builder;
    this->type = type;
    this->ptr = type->getPointerTo();   

    { // malloc
    FunctionType *FT = FunctionType::get(ptr, {builder->getInt64Ty()}, false);
    _malloc = Function::Create(FT, Function::ExternalLinkage, "malloc", *module);
    llvm::verifyFunction(*_malloc);
    }
    { // free
    FunctionType *FT = FunctionType::get(builder->getVoidTy(), {ptr}, false);
    _free = Function::Create(FT, Function::ExternalLinkage, "free", *module);
    llvm::verifyFunction(*_free);
    }
    { // memcpy
    FunctionType *FT = FunctionType::get(ptr, {ptr, ptr, builder->getInt64Ty()}, false);
    _memcpy = Function::Create(FT, Function::ExternalLinkage, "memcpy", *module);
    llvm::verifyFunction(*_memcpy);
    }
}

Memory::~Memory() {}

Value *Memory::malloc(Value *size, string name) { return builder->CreateCall(_malloc, {size}, name); }

void Memory::free(Value *pointer) { builder->CreateCall(_free, {pointer}); }

Value *Memory::memcpy(Value *dest, Value *source, Value *size) { return builder->CreateCall(_memcpy, {dest, source, size}); }

LLVMContext *Memory::getContext() { return context; }

Module *Memory::getModule() { return module; }

IRBuilder<> *Memory::getBuilder() { return builder; }

Type *Memory::getType() { return type; }

Array::Array(Memory *mem, string name) {
    context = mem->getContext();
    module = mem->getModule();
    builder = mem->getBuilder();
    elementType = mem->getType();
    elementPtr = elementType->getPointerTo();
    this->name = name;

    self = StructType::create(*context, {
        elementPtr,             // type *buffer;
        builder->getInt64Ty(),  // i64 length;
        builder->getInt64Ty(),  // i64 maxlength;
        builder->getInt64Ty(),  // i64 factor; amount to preallocate when growing
    }, name);
    ptr = self->getPointerTo();

    { // get_buffer
    FunctionType *FT = FunctionType::get(elementPtr, {ptr}, false);
    get_buffer = Function::Create(FT, Function::ExternalLinkage, name+"_get_buffer", *module);
    BasicBlock *BB = BasicBlock::Create(*context, "entry", get_buffer);
    
    builder->SetInsertPoint(BB);
    Value *buffer_ptr = builder->CreateGEP(self, get_buffer->getArg(0), 
        {builder->getInt64(0), builder->getInt32(0)}, "buffer_ptr");
    Value *buffer = builder->CreateLoad(elementPtr, buffer_ptr, "buffer");
    builder->CreateRet(buffer);

    llvm::verifyFunction(*get_buffer);
    }
    
    { // get_length
    FunctionType *FT = FunctionType::get(builder->getInt64Ty(), {ptr}, false);
    get_length = Function::Create(FT, Function::ExternalLinkage, name+"_get_length", *module);
    BasicBlock *BB = BasicBlock::Create(*context, "entry", get_length);

    builder->SetInsertPoint(BB);
    Value *length_ptr = builder->CreateGEP(self, get_length->getArg(0),
        {builder->getInt64(0), builder->getInt32(1)}, "length_ptr");
    Value *length = builder->CreateLoad(builder->getInt64Ty(), length_ptr, "length");
    builder->CreateRet(length);

    llvm::verifyFunction(*get_length);
    }

    { // resize
    FunctionType *FT = FunctionType::get(builder->getVoidTy(), {ptr, builder->getInt64Ty()}, false);
    resize = Function::Create(FT, Function::ExternalLinkage, name+"_resize", *module);
    resize->setCallingConv(llvm::CallingConv::Fast);
    BasicBlock *BB = BasicBlock::Create(*context, "entry", resize);

    builder->SetInsertPoint(BB);
    Value *output = mem->malloc(resize->getArg(1), "output");
    Value *buffer_ptr = builder->CreateGEP(self, resize->getArg(0), 
        {builder->getInt64(0), builder->getInt32(0)}, "buffer_ptr");  // buffer_ptr is required later
    Value *buffer = builder->CreateLoad(elementPtr, buffer_ptr, "buffer");
    Value *length = this->getLength(resize->getArg(0));
    mem->memcpy(output, buffer, length);
    mem->free(buffer);
    builder->CreateStore(output, buffer_ptr);
    Value *maxlength_ptr = builder->CreateGEP(self, resize->getArg(0), 
        {builder->getInt64(0), builder->getInt32(2)}, "maxlength_ptr");
    builder->CreateStore(resize->getArg(1), maxlength_ptr);
    builder->CreateRetVoid();

    llvm::verifyFunction(*resize);
    }

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
    builder->CreateStore(ConstantPointerNull::get(elementPtr), buffer_ptr);
    builder->CreateStore(builder->getInt64(0), length_ptr);
    builder->CreateStore(builder->getInt64(0), maxlength_ptr);
    builder->CreateStore(builder->getInt64(16), factor_ptr);
    builder->CreateRetVoid();

    llvm::verifyFunction(*create_empty);
    }

    { // TODO: create_copy
    FunctionType *FT = FunctionType::get(builder->getVoidTy(), {ptr, ptr}, false);
    create_copy = Function::Create(FT, Function::ExternalLinkage, name+"_create_copy", *module);
    llvm::verifyFunction(*create_copy);
    }

    { // delete_array
    FunctionType *FT = FunctionType::get(builder->getVoidTy(), {ptr}, false);
    delete_array = Function::Create(FT, Function::ExternalLinkage, name+"_delete_array", *module);
    delete_array->setCallingConv(llvm::CallingConv::Fast);
    delete_array->setDoesNotThrow();
    BasicBlock *entry = BasicBlock::Create(*context, "entry", delete_array);
    BasicBlock *free_begin = BasicBlock::Create(*context, "free_begin", delete_array);
    BasicBlock *free_close = BasicBlock::Create(*context, "free_close", delete_array);

    builder->SetInsertPoint(entry);
    Value *buffer = this->getBuffer(delete_array->getArg(0), "buffer");
    Value *comp = builder->CreateICmpNE(buffer, ConstantPointerNull::get(elementPtr), "comp");
    builder->CreateCondBr(comp, free_begin, free_close);

    builder->SetInsertPoint(free_begin);
    mem->free(buffer);
    builder->CreateBr(free_close);

    builder->SetInsertPoint(free_close);
    builder->CreateRetVoid();

    llvm::verifyFunction(*delete_array);
    }
}

Array::~Array() { name.clear(); }

Value *Array::allocate(string name) { return builder->CreateAlloca(self, nullptr, name); }

void Array::initialize(Value *array) { builder->CreateCall(this->create_empty, {array}); }

void Array::del(Value *array) { builder->CreateCall(this->delete_array, {array}); }

Value *Array::getBuffer(Value *array, string name) { return builder->CreateCall(this->get_buffer, {array}, name); }

Value *Array::getLength(Value *array, string name) { return builder->CreateCall(this->get_length, {array}, name); }

void Array::copy(Value *from, Value *to) { builder->CreateCall(this->create_copy, {to, from}); }

Value *Array::duplicate(Value *array, string name) {
    Value *obj = this->allocate(name);
    this->copy(array, obj);
    return obj;
}

PointerType *Array::getPointerTo() { return ptr; }

StructType *Array::getTy() { return self; }

Type *Array::getElementTy() { return elementType; }

PointerType *Array::getElementPtrTy() { return elementPtr; }

#endif // LLVM_ARRAY_IMPL

}