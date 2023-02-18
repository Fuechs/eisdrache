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

ReturnInst *Eisdrache::createRet(Value *value, BasicBlock *next) {
    ReturnInst *inst = builder->CreateRet(value);
    if (next)
        builder->SetInsertPoint(next);
    return inst;
}

WrappedVal &Eisdrache::getWrap(Value *pointer) {
    for (WrappedVal::Map::value_type &x : values) 
        if (x.second.value == pointer)
            return x.second;
    assert(false && "value not found in WrappedVal::Map Eisdrache::values");
}

Value *Eisdrache::loadValue(Value *pointer) {
    WrappedVal &that = getWrap(pointer);
    if (that.kind == WrappedVal::LOCAL)
        return builder->CreateLoad(that.type, pointer, pointer->getName()+"_load_");
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