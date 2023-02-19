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
    name = typeName;

    self = eisdrache->createType({
        elementPtr,              // type *buffer;
        eisdrache->getSizeTy(),  // i64 length;
        eisdrache->getSizeTy(),  // i64 maxlength;
        eisdrache->getSizeTy(),  // i64 factor; amount to preallocate when growing
    }, name);
    ptr = self->getPointerTo();

    eisdrache->createMemoryFunctions(eisdrache->getIntTy(8));

    { // get_buffer
    get_buffer = eisdrache->declare(elementPtr, {ptr}, name+"_get_buffer", true);
    Value *buffer = eisdrache->getElementVal(get_buffer->getArg(0), 0, "buffer");
    eisdrache->createRet(buffer);
    llvm::verifyFunction(*get_buffer);
    }

    // set_buffer
    set_buffer = eisdrache->declare(eisdrache->getVoidTy(), {ptr, elementPtr}, name+"_set_buffer", true);
    eisdrache->store(set_buffer->getArg(1), set_buffer->getArg(0), 0);
    eisdrache->createRet();
    llvm::verifyFunction(*set_buffer);
    
    { // get_length
    get_length = eisdrache->declare(eisdrache->getSizeTy(), {ptr}, name+"_get_length", true);
    Value *length = eisdrache->getElementVal(get_length->getArg(0), 1, "length");
    eisdrache->createRet(length);
    llvm::verifyFunction(*get_length);
    }
    
    // set_length
    set_length = eisdrache->declare(eisdrache->getVoidTy(), {ptr, eisdrache->getSizeTy()}, name+"_set_length", true);
    eisdrache->store(set_length->getArg(1), set_length->getArg(0), 1);
    eisdrache->createRet();
    llvm::verifyFunction(*set_length);

    { // get_max_length
    get_max_length = eisdrache->declare(eisdrache->getSizeTy(), {ptr}, name+"_get_max_length", true);
    Value *max_length = eisdrache->getElementVal(get_max_length->getArg(0), 2, "max_length");
    eisdrache->createRet(max_length);
    llvm::verifyFunction(*get_max_length);
    }

    // set_max_length
    set_max_length = eisdrache->declare(eisdrache->getVoidTy(), {ptr, eisdrache->getSizeTy()}, name+"_set_max_length", true);
    eisdrache->store(set_max_length->getArg(1), set_max_length->getArg(0), 2);
    eisdrache->createRet();
    llvm::verifyFunction(*set_max_length);

    { // get_factor
    get_factor = eisdrache->declare(eisdrache->getVoidTy(), {ptr}, name+"_get_factor", true);
    Value *factor = eisdrache->getElementVal(get_factor->getArg(0), 3, "factor");
    eisdrache->createRet(factor);
    llvm::verifyFunction(*get_factor);
    }

    // set_factor
    set_factor = eisdrache->declare(eisdrache->getVoidTy(), {ptr, eisdrache->getSizeTy()}, name+"_set_factor", true);
    eisdrache->store(set_factor->getArg(1), set_factor->getArg(0), 3);
    eisdrache->createRet();
    llvm::verifyFunction(*set_factor);

    { // resize
    resize = eisdrache->declare(eisdrache->getVoidTy(), {ptr, eisdrache->getSizeTy()}, name+"_resize", true);
    resize->setCallingConv(CallingConv::Fast);
    Value *output = eisdrache->malloc(eisdrache->getIntTy(8), resize->getArg(1), "output");
    Value *buffer_ptr = eisdrache->getElementPtr(resize->getArg(0), 0, "buffer_ptr");  // buffer_ptr is required later
    Value *buffer = eisdrache->loadValue(buffer_ptr, "buffer", true);
    Value *length = this->getLength(resize->getArg(0), "length");
    eisdrache->memcpy(eisdrache->getIntTy(8), output, buffer, length);
    eisdrache->free(eisdrache->getIntTy(8), buffer);
    eisdrache->store(output, buffer_ptr);
    setMaxLength(resize->getArg(0), resize->getArg(1));
    eisdrache->createRet();
    llvm::verifyFunction(*resize);
    }

    { // create_empty
    create_empty = eisdrache->declare(eisdrache->getVoidTy(), {ptr}, name+"_create_empty", true);
    create_empty->setCallingConv(CallingConv::Fast);
    create_empty->setDoesNotThrow();
    Value *obj = create_empty->getArg(0);
    setBuffer(obj, eisdrache->getNullPtr(elementPtr));
    setLength(obj, eisdrache->getInt(64, 0));
    setMaxLength(obj, eisdrache->getInt(64, 0));
    setFactor(obj, eisdrache->getInt(64, 16));
    eisdrache->createRet();
    llvm::verifyFunction(*create_empty);
    }

    { // create_copy
    create_copy = eisdrache->declare(eisdrache->getVoidTy(), {ptr, ptr}, name+"_create_copy", true);
    create_copy->setCallingConv(CallingConv::Fast);
    create_copy->setDoesNotThrow();
    Value *dest = create_copy->getArg(0);
    Value *dest_buffer = getBuffer(dest, "dest_buffer");
    Value *source = create_copy->getArg(1);
    Value *source_buffer = getBuffer(source, "source_buffer");
    Value *source_length = getLength(source, "source_length");
    Value *source_max_length = getMaxLength(source, "source_max_length");
    Value *source_factor = getFactor(source, "source_factor");
    resizeBuffer(dest, source_max_length);
    setBuffer(dest, source_buffer);
    setLength(dest, source_length);
    setFactor(dest, source_factor);
    eisdrache->createRet();
    llvm::verifyFunction(*create_copy);
    }

    { // delete_array
    delete_array = eisdrache->declare(eisdrache->getVoidTy(), {ptr}, name+"_delete_array", true);
    delete_array->setCallingConv(llvm::CallingConv::Fast);
    delete_array->setDoesNotThrow();
    BasicBlock *free_begin = eisdrache->block(false, "free_begin");
    BasicBlock *free_close = eisdrache->block(false, "free_close");
    Value *buffer = getBuffer(delete_array->getArg(0), "buffer");
    Value *comp = eisdrache->getBuilder()->CreateICmpNE(buffer, eisdrache->getNullPtr(elementPtr), "comp");
    eisdrache->condJump(comp, free_begin, free_close);
    
    eisdrache->setBlock(free_begin);
    eisdrache->free(eisdrache->getIntTy(8), buffer);
    eisdrache->jump(free_close);
    
    eisdrache->setBlock(free_close);
    eisdrache->createRet();

    llvm::verifyFunction(*delete_array);
    }
}

EisdracheArray::~EisdracheArray() { name.clear(); }

Value *EisdracheArray::allocate(std::string name) { return eisdrache->allocate(self, name); }

void EisdracheArray::initialize(Value *array) { eisdrache->call(this->create_empty, {array}); }

void EisdracheArray::del(Value *array) { eisdrache->call(this->delete_array, {array}); }

Value *EisdracheArray::getBuffer(Value *array, std::string name) { return eisdrache->call(this->get_buffer, {array}, name); }

void EisdracheArray::setBuffer(Value *array, Value *value) { eisdrache->call(this->set_buffer, {array, value}); }

Value *EisdracheArray::getLength(Value *array, std::string name) { return eisdrache->call(this->get_length, {array}, name); }

void EisdracheArray::setLength(Value *array, Value *value) { eisdrache->call(this->set_length, {array, value}); }

Value *EisdracheArray::getMaxLength(Value *array, std::string name) { return eisdrache->call(this->get_max_length, {array}, name); }

void EisdracheArray::setMaxLength(Value *array, Value *value) { eisdrache->call(this->set_max_length, {array, value}); }

Value *EisdracheArray::getFactor(Value *array, std::string name) { return eisdrache->call(this->get_factor, {array}, name); }

void EisdracheArray::setFactor(Value *array, Value *value) { eisdrache->call(this->set_factor, {array, value}); }

void EisdracheArray::copy(Value *from, Value *to) { eisdrache->call(this->create_copy, {to, from}); }

Value *EisdracheArray::duplicate(Value *array, std::string name) {
    Value *obj = this->allocate(name);
    this->copy(array, obj);
    return obj;
}

void EisdracheArray::resizeBuffer(Value *array, Value *size) { eisdrache->call(this->resize, {array, size}); }

PointerType *EisdracheArray::getPointerTo() { return ptr; }

StructType *EisdracheArray::getTy() { return self; }

Type *EisdracheArray::getElementTy() { return elementType; }

PointerType *EisdracheArray::getElementPtrTy() { return elementPtr; }

#endif // LLVM_ARRAY_IMPL

}