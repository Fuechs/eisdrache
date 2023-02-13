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

/// CREATE ///

Eisdrache::Eisdrache(LLVMContext *context, Module *module, IRBuilder<> *builder)
: context(context), module(module), builder(builder) {}

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

/// UTILITY ///

Function *Eisdrache::createMain() {
    FunctionType *FT = FunctionType::get(getIntTy(64), {getIntTy(64), getIntPtrTy(8)}, false);
    main = Function::Create(FT, Function::ExternalLinkage, "main", *module);
    verifyFunction(*main, &errs());
    return main;
}

/// DEBUG ///

void Eisdrache::dump(raw_fd_ostream &os) { module->print(os, nullptr); }

/// GETTER ///

LLVMContext *Eisdrache::getContext() { return context; }
Module *Eisdrache::getModule() { return module; }
IRBuilder<> *Eisdrache::getBuilder() { return builder; }

IntegerType *Eisdrache::getSizeTy() { return getIntTy(64); }
IntegerType *Eisdrache::getIntTy(size_t bit) { return Type::getIntNTy(*context, bit); }
PointerType *Eisdrache::getIntPtrTy(size_t bit) { return Type::getIntNPtrTy(*context, bit); }
PointerType *Eisdrache::getIntPtrPtrTy(size_t bit) { return getIntPtrTy(bit)->getPointerTo(); }

ConstantInt *Eisdrache::getInt(IntegerType *type, size_t value) { return ConstantInt::get(type, value); }
ConstantInt *Eisdrache::getInt(size_t bit, size_t value) { return ConstantInt::get(getIntTy(bit), value); }

/// BUILDER ///
Value *Eisdrache::allocate(Type *type, std::string name) { return builder->CreateAlloca(type, nullptr, name); }
Value *Eisdrache::call(Function *function, std::vector<Value *> args, std::string name) { return builder->CreateCall(function, args, name); };
Function *Eisdrache::declare(Type *type, std::vector<Type *> parameters, std::string name) {
    FunctionType *FT = FunctionType::get(type, parameters, false);
    Function *F = Function::Create(FT, Function::ExternalLinkage, name, *module);
    llvm::verifyFunction(*F);
    return F;
}

Value *Eisdrache::malloc(Type *type, Value *size, std::string name) { return call(memoryFunctions[type]["malloc"], {size}, name); }
void Eisdrache::free(Type *type, Value *value) { call(memoryFunctions[type]["free"], {value}); }
Value *Eisdrache::memcpy(Type *type, Value *dest, Value *source, Value *size, std::string name) { return call(memoryFunctions[type]["memcpy"], {dest, source, size}, name); }

void Eisdrache::createMemoryFunctions(Type *type) {
    PointerType *ptrType = type->getPointerTo();
    memoryFunctions[type]["malloc"] = Function::Create(FunctionType::get(ptrType, {getSizeTy()}, false), Function::ExternalLinkage, "malloc", *module);
    memoryFunctions[type]["free"] = Function::Create(FunctionType::get(builder->getVoidTy(), {ptrType}, false), Function::ExternalLinkage, "free", *module);
    memoryFunctions[type]["memcpy"] =  Function::Create(FunctionType::get(ptrType, {ptrType, ptrType, getSizeTy()}, false), Function::ExternalLinkage, "memcpy", *module);
    llvm::verifyFunction(*memoryFunctions[type]["malloc"]);
    llvm::verifyFunction(*memoryFunctions[type]["free"]);
    llvm::verifyFunction(*memoryFunctions[type]["memcpy"]);
}

}