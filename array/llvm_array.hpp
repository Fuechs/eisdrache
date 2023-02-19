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

#include "../src/eisdrache.hpp"

namespace llvm {

class EisdracheArray {
public:
    EisdracheArray(Eisdrache *eisdrache, Type *elementType, std::string typeName);
    ~EisdracheArray();

    // allocate memory for this type
    // name: name for returned pointer
    Value *allocate(std::string name = "");
    // initialize object of this type
    // array: pointer to object
    void initialize(Value *array);
    // delete object of this type
    // array: pointer to object
    void del(Value *array);
    // get buffer of object of this type
    // array: pointer to object
    // name: name for returned pointer
    Value *getBuffer(Value *array, std::string name = "");
    // set buffer of object of this type
    // array: pointer to object
    // value: value to assign
    // ! USER HAS TO ALLOCATE MEMORY
    void setBuffer(Value *array, Value *value);
    // get buffer length/size of object of this type
    // array: pointer to object
    // name: name for returned value
    Value *getLength(Value *array, std::string name = "");
    // set buffer length/size of object of this type
    // array: pointer to object
    // value: value to assign
    void setLength(Value *array, Value *value);
    // get max length of object of this types
    // array: pointer to object
    // name: name for returned value
    Value *getMaxLength(Value *array, std::string name = "");
    // set max length of object of this type
    // array: pointer to object
    // value: value to assign
    void setMaxLength(Value *array, Value *value);
    // get factor of object of this types
    // array: pointer to object
    // name: name for returned value
    Value *getFactor(Value *array, std::string name = "");
    // set factor of object of this types
    // array: pointer to object
    // value: value to assign
    void setFactor(Value *array, Value *value);
    // copy buffer from `from` to `to`
    // from: pointer to object of this type
    // to: pointer to object of this type
    void copy(Value *from, Value *to);
    // create duplicate of object of this type
    // array: pointer to object
    // name: name for returned pointer
    Value *duplicate(Value *array, std::string name = "");
    // resize of `array` with size `size`
    void resizeBuffer(Value *array, Value *size);

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
    std::string name = "";              // name of this array type

    Function *get_buffer = nullptr;    
    Function *set_buffer = nullptr;    
    Function *get_length = nullptr;
    Function *set_length = nullptr;
    Function *get_max_length = nullptr;
    Function *set_max_length = nullptr;
    Function *get_factor = nullptr;
    Function *set_factor = nullptr;

    Function *resize = nullptr;    
    Function *push_back = nullptr; // TODO: push_back
    Function *pop_back = nullptr;  // TODO: pop_back
    Function *push_front = nullptr; // TODO: push_front
    Function *pop_front = nullptr; // TODO: pop_front
    
    Function *create_empty = nullptr;   // default constructor
    Function *create_copy = nullptr;    // copy constructor
    Function *delete_array = nullptr;   // destructor

    Eisdrache *eisdrache = nullptr;
};

#ifdef LLVM_ARRAY_IMPL 

EisdracheArray::EisdracheArray(Eisdrache *eisdrache, Type *elementType, std::string typeName) {
    this->eisdrache = eisdrache;
    this->elementType = elementType;
    elementPtr = elementType->getPointerTo();
    this->name = name;

    self = eisdrache->createType({
        elementPtr,              // type *buffer;
        eisdrache->getSizeTy(),  // i64 length;
        eisdrache->getSizeTy(),  // i64 maxlength;
        eisdrache->getSizeTy(),  // i64 factor; amount to preallocate when growing
    }, name);
    ptr = self->getPointerTo();

    { // get_buffer
    get_buffer = eisdrache->declare(elementPtr, {ptr}, name+"_get_buffer", true);
    Value *buffer_ptr = eisdrache->getBuilder()->CreateGEP(self, get_buffer->getArg(0), 
        {eisdrache->getInt(64, 0), eisdrache->getInt(32, 0)}, "buffer_ptr");
    Value *buffer = eisdrache->getBuilder()->CreateLoad(elementPtr, buffer_ptr, "buffer");
    eisdrache->createRet(buffer);
    llvm::verifyFunction(*get_buffer);
    }

    { // set_buffer
    set_buffer = eisdrache->declare(eisdrache->getVoidTy(), {ptr, elementPtr}, name+"_set_buffer", true);
    Value *buffer_ptr = eisdrache->getBuilder()->CreateGEP(self, set_buffer->getArg(0),
        {eisdrache->getInt(64, 0), eisdrache->getInt(32, 0)}, "buffer_ptr");
    eisdrache->getBuilder()->CreateStore(set_buffer->getArg(1), buffer_ptr);
    eisdrache->createRet();
    llvm::verifyFunction(*set_buffer);
    }
    
    { // get_length
    get_length = eisdrache->declare(eisdrache->getSizeTy(), {ptr}, name+"_get_length", true);
    Value *length_ptr = eisdrache->getBuilder()->CreateGEP(self, get_length->getArg(0),
        {eisdrache->getInt(64, 0), eisdrache->getInt(32, 1)}, "length_ptr");
    Value *length = eisdrache->getBuilder()->CreateLoad(eisdrache->getSizeTy(), length_ptr, "length");
    eisdrache->createRet(length);
    llvm::verifyFunction(*get_length);
    }
    
    { // set_length
    FunctionType *FT = FunctionType::get(builder->getVoidTy(), {ptr, builder->getInt64Ty()}, false);
    set_length = Function::Create(FT, Function::ExternalLinkage, name+"_set_length", *module);
    BasicBlock *BB = BasicBlock::Create(*context, "entry", set_length);

    builder->SetInsertPoint(BB);
    Value *length_ptr = builder->CreateGEP(self, set_length->getArg(0), 
        {builder->getInt64(0), builder->getInt32(1)}, "length_ptr");
    builder->CreateStore(set_length->getArg(1), length_ptr);
    builder->CreateRetVoid();

    llvm::verifyFunction(*set_length);
    }

    { // get_max_length
    FunctionType *FT = FunctionType::get(builder->getInt64Ty(), {ptr}, false);
    get_max_length = Function::Create(FT, Function::ExternalLinkage, name+"_get_max_length", *module);
    BasicBlock *BB = BasicBlock::Create(*context, "entry", get_max_length);
    
    builder->SetInsertPoint(BB);
    Value *max_length_ptr = builder->CreateGEP(self, get_max_length->getArg(0), {builder->getInt64(0), builder->getInt32(2)}, "max_length_ptr");
    Value *max_length = builder->CreateLoad(builder->getInt64Ty(), max_length_ptr, "max_length");
    builder->CreateRet(max_length);

    llvm::verifyFunction(*get_max_length);
    }

    { // set_max_length
    FunctionType *FT = FunctionType::get(builder->getVoidTy(), {ptr, builder->getInt64Ty()}, false);
    set_max_length = Function::Create(FT, Function::ExternalLinkage, name+"_set_max_length", *module);
    BasicBlock *BB = BasicBlock::Create(*context, "entry", set_max_length);

    builder->SetInsertPoint(BB);
    Value *max_length_ptr = builder->CreateGEP(self, set_max_length->getArg(0), 
        {builder->getInt64(0), builder->getInt32(2)}, "max_length_ptr");
    builder->CreateStore(set_max_length->getArg(1), max_length_ptr);
    builder->CreateRetVoid();

    llvm::verifyFunction(*set_max_length);
    }

    { // get_factor
    FunctionType *FT = FunctionType::get(builder->getInt64Ty(), {ptr}, false);
    get_factor = Function::Create(FT, Function::ExternalLinkage, name+"_get_factor", *module);
    BasicBlock *BB = BasicBlock::Create(*context, "entry", get_factor);
    
    builder->SetInsertPoint(BB);
    Value *factor_ptr = builder->CreateGEP(self, get_factor->getArg(0), {builder->getInt64(0), builder->getInt32(3)}, "factor_ptr");
    Value *factor = builder->CreateLoad(builder->getInt64Ty(), factor_ptr, "factor");
    builder->CreateRet(factor);

    llvm::verifyFunction(*get_factor);
    }

    { // set_factor
    FunctionType *FT = FunctionType::get(builder->getVoidTy(), {ptr, builder->getInt64Ty()}, false);
    set_factor = Function::Create(FT, Function::ExternalLinkage, name+"_set_factor", *module);
    BasicBlock *BB = BasicBlock::Create(*context, "entry", set_factor);

    builder->SetInsertPoint(BB);
    Value *factor_ptr = builder->CreateGEP(self, set_factor->getArg(0), 
        {builder->getInt64(0), builder->getInt32(3)}, "factor_ptr");
    builder->CreateStore(set_factor->getArg(1), factor_ptr);
    builder->CreateRetVoid();

    llvm::verifyFunction(*set_factor);
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
    setMaxLength(resize->getArg(0), resize->getArg(1));
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
    Value *obj = create_empty->getArg(0);
    setBuffer(obj, ConstantPointerNull::get(elementPtr));
    setLength(obj, builder->getInt64(0));
    setMaxLength(obj, builder->getInt64(0));
    setFactor(obj, builder->getInt64(16));
    builder->CreateRetVoid();

    llvm::verifyFunction(*create_empty);
    }

    { // create_copy
    FunctionType *FT = FunctionType::get(builder->getVoidTy(), {ptr, ptr}, false);
    create_copy = Function::Create(FT, Function::ExternalLinkage, name+"_create_copy", *module);
    create_copy->setCallingConv(llvm::CallingConv::Fast);
    create_copy->setDoesNotThrow();
    BasicBlock *BB = BasicBlock::Create(*context, "entry", create_copy);

    builder->SetInsertPoint(BB);
    Value *dest = create_copy->getArg(0);
    Value *dest_buffer = getBuffer(dest, "dest_buffer");
    Value *source = create_copy->getArg(1);
    Value *source_buffer = getBuffer(source, "source_buffer");
    Value *source_length = getLength(source, "source_length");
    Value *source_max_length = getMaxLength(source, "source_max_length");
    Value *source_factor = getFactor(source, "source_factor");
    resizeBuffer(dest, source_max_length);
    mem->memcpy(dest_buffer, source_buffer, source_length);
    setLength(dest, source_length);
    setFactor(dest, source_factor);
    builder->CreateRetVoid();

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

void Array::setBuffer(Value *array, Value *value) { builder->CreateCall(this->set_buffer, {array, value}); }

Value *Array::getLength(Value *array, string name) { return builder->CreateCall(this->get_length, {array}, name); }

void Array::setLength(Value *array, Value *value) { builder->CreateCall(this->set_length, {array, value}); }

Value *Array::getMaxLength(Value *array, string name) { return builder->CreateCall(this->get_max_length, {array}, name); }

void Array::setMaxLength(Value *array, Value *value) { builder->CreateCall(this->set_max_length, {array, value}); }

Value *Array::getFactor(Value *array, string name) { return builder->CreateCall(this->get_factor, {array}, name); }

void Array::setFactor(Value *array, Value *value) { builder->CreateCall(this->set_factor, {array, value}); }

void Array::copy(Value *from, Value *to) { builder->CreateCall(this->create_copy, {to, from}); }

Value *Array::duplicate(Value *array, string name) {
    Value *obj = this->allocate(name);
    this->copy(array, obj);
    return obj;
}

void Array::resizeBuffer(Value *array, Value *size) { builder->CreateCall(this->resize, {array, size}); }

PointerType *Array::getPointerTo() { return ptr; }

StructType *Array::getTy() { return self; }

Type *Array::getElementTy() { return elementType; }

PointerType *Array::getElementPtrTy() { return elementPtr; }

#endif // LLVM_ARRAY_IMPL

}