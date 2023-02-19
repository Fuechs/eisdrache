/**
 * @file eisdrache.cpp
 * @author fuechs
 * @brief Eisdrache class implementation
 * @version 0.1
 * @date 2023-01-30
 * 
 * @copyright Copyright (c) 2023, Fuechs.
 * 
 */

#include "eisdrache.hpp"

namespace llvm {

WrappedVal::WrappedVal(Kind kind, Type *type, Value *value) : kind(kind), type(type), value(value) {}

WrappedVal &WrappedVal::operator=(const WrappedVal &copy) {
    kind = copy.kind;
    type = copy.type;
    value = copy.value;
    return *this;
}

WrappedType::WrappedType(StructType *structType, std::vector<Type *> elementTypes) 
: type(structType), elementTypes(elementTypes) {}

WrappedType &WrappedType::operator=(const WrappedType &copy) {
    type = copy.type;
    elementTypes = copy.elementTypes;
    return *this;
}

Type *WrappedType::operator[](signed long long index) {
    if (index < 0) 
        return type;
    return elementTypes[index];
}

/// CREATE ///

Eisdrache::Eisdrache(LLVMContext *context, Module *module, IRBuilder<> *builder)
: context(context), module(module), builder(builder) {}

Eisdrache::~Eisdrache() {
    delete builder;
    memoryFunctions.clear();
}

Eisdrache *Eisdrache::create(std::string moduleID) {
    LLVMContext *context = new LLVMContext();
    return new Eisdrache(context, new Module(moduleID, *context), new IRBuilder<>(*context));
}

Eisdrache *Eisdrache::create(LLVMContext *context, std::string moduleID) {
    return new Eisdrache(context, new Module(moduleID, *context), new IRBuilder<>(*context));
}

Eisdrache *Eisdrache::create(Module *module) {
    return new Eisdrache(&module->getContext(), module, new IRBuilder<>(module->getContext()));
}

Eisdrache *Eisdrache::create(Module *module, IRBuilder<> *builder) {
    return new Eisdrache(&module->getContext(), module, builder);
};

/// DEBUG ///

void Eisdrache::dump(raw_fd_ostream &os) { module->print(os, nullptr); }

/// GETTER ///

LLVMContext *Eisdrache::getContext() { return context; }
Module *Eisdrache::getModule() { return module; }
IRBuilder<> *Eisdrache::getBuilder() { return builder; }

Type *Eisdrache::getVoidTy() { return builder->getVoidTy(); }
IntegerType *Eisdrache::getSizeTy() { return getIntTy(64); }
IntegerType *Eisdrache::getIntTy(size_t bit) { return Type::getIntNTy(*context, bit); }
PointerType *Eisdrache::getIntPtrTy(size_t bit) { return Type::getIntNPtrTy(*context, bit); }
PointerType *Eisdrache::getIntPtrPtrTy(size_t bit) { return getIntPtrTy(bit)->getPointerTo(); }

ConstantInt *Eisdrache::getInt(IntegerType *type, size_t value) { return ConstantInt::get(type, value); }
ConstantInt *Eisdrache::getInt(size_t bit, size_t value) { return ConstantInt::get(getIntTy(bit), value); }

/// BUILDER ///
Value *Eisdrache::allocate(Type *type, std::string name) { 
    Value *allocated = builder->CreateAlloca(type, nullptr, name);
    values[allocated->getName().str()] = WrappedVal(WrappedVal::LOCAL, type, allocated);
    return allocated;
}

Value *Eisdrache::call(Function *function, std::vector<Value *> args, std::string name) { return builder->CreateCall(function, args, name); };

Function *Eisdrache::declare(Type *type, std::vector<Type *> parameters, std::string name, bool entry) {
    FunctionType *FT = FunctionType::get(type, parameters, false);
    Function *F = Function::Create(FT, Function::ExternalLinkage, name, *module);
    if (entry) {
        BasicBlock *BB = BasicBlock::Create(*context, "entry", F);
        builder->SetInsertPoint(BB);
    } else
        llvm::verifyFunction(*F);
    return F;
}

ReturnInst *Eisdrache::createRet(BasicBlock *next) {
    ReturnInst *inst = builder->CreateRetVoid();
    if (next)
        builder->SetInsertPoint(next);
    return inst;
} 

ReturnInst *Eisdrache::createRet(Value *value, BasicBlock *next) {
    ReturnInst *inst = builder->CreateRet(value);
    if (next)
        builder->SetInsertPoint(next);
    return inst;
}

StructType *Eisdrache::createType(std::vector<Type *> elements, std::string name) { 
    StructType *that = StructType::create(*context, elements, name);
    types[that->getName().str()] = WrappedType(that, elements);
    return that;
}

Value *Eisdrache::getElementPtr(Value *ptr, size_t index, std::string name) {
    Type *structType = getWrap(ptr).type; 
    WrappedType &wrap = getWrap(structType);
    Value *elementPtr = builder->CreateGEP(structType, ptr, {getInt(64, 0), getInt(32, index)}, name);
    values[elementPtr->getName().str()] = WrappedVal(WrappedVal::LOADED, wrap[index], elementPtr);
    return elementPtr;
} 

Value *Eisdrache::getElementVal(Value *ptr, size_t index, std::string name) {
    Value *elementPtr = getElementPtr(ptr, index, name+"_ptr_");
    Value *elementVal = loadValue(elementPtr, name, true);
    values[elementVal->getName().str()] = WrappedVal(WrappedVal::LOADED, elementVal->getType(), elementVal);
    return elementVal;
}

void Eisdrache::store(Value *value, Value *structPtr, size_t index) {
    Value *elementPtr = getElementPtr(structPtr, index, "unnamed_gep_");
    store(value, elementPtr);
}

void Eisdrache::store(Value *value, Value *ptr) { builder->CreateStore(value, ptr); }

WrappedVal &Eisdrache::getWrap(Value *pointer) {
    for (WrappedVal::Map::value_type &x : values) 
        if (x.second.value == pointer)
            return x.second;
    assert(false && "value not found in WrappedVal::Map Eisdrache::values");
}

WrappedType &Eisdrache::getWrap(Type *type) {
    if (!type->isStructTy())
        assert (false && "type is not a StructType *");

    for (WrappedType::Map::value_type &x : types) 
        if ((Type *) x.second.type == type)
            return x.second;
    
    assert(false && "struct type not found in WrappedType::Map Eisdrache::types");
}

Value *Eisdrache::loadValue(Value *pointer, std::string name, bool force) {
    WrappedVal &that = getWrap(pointer);
    if (force || that.kind == WrappedVal::LOCAL)
        return builder->CreateLoad(that.type, pointer, name.empty() ? pointer->getName()+"_load_" : name);
    return pointer;
}

Value *Eisdrache::malloc(Type *type, Value *size, std::string name) { return call(memoryFunctions[type]["malloc"], {size}, name); }
void Eisdrache::free(Type *type, Value *value) { call(memoryFunctions[type]["free"], {value}); }
Value *Eisdrache::memcpy(Type *type, Value *dest, Value *source, Value *size, std::string name) { return call(memoryFunctions[type]["memcpy"], {dest, source, size}, name); }

void Eisdrache::createMemoryFunctions(Type *type) {
    PointerType *ptrType = type->getPointerTo();
    memoryFunctions[type]["malloc"] = declare(ptrType, {getSizeTy()}, "malloc");
    memoryFunctions[type]["free"] = declare(getVoidTy(), {ptrType}, "free");
    memoryFunctions[type]["memcpy"] = declare(ptrType, {ptrType, ptrType, getSizeTy()}, "memcpy");
}

}