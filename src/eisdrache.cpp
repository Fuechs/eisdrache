/**
 * @file eisdrache.cpp
 * @author fuechs
 * @brief Eisdrache Wrapper class implementation
 * @version 0.1
 * @date 2023-01-30
 * 
 * @copyright Copyright (c) 2023, Fuechs.
 * 
 */

#include "eisdrache.hpp"

namespace llvm {

/// CREATE ///

Wrapper::Wrapper(LLVMContext *context, Module *module, IRBuilder<> *builder)
: context(context), module(module), builder(builder) {}

Wrapper *Wrapper::create(std::string moduleID) {
    LLVMContext *context = new LLVMContext();
    return new Wrapper(context, new Module(moduleID, *context), new IRBuilder<>(*context));
}

Wrapper *Wrapper::create(LLVMContext *context, std::string moduleID) {
    return new Wrapper(context, new Module(moduleID, *context), new IRBuilder<>(*context));
}

Wrapper *Wrapper::create(Module *module) {
    return new Wrapper(&module->getContext(), module, new IRBuilder<>(module->getContext()));
}

Wrapper *Wrapper::create(Module *module, IRBuilder<> *builder) {
    return new Wrapper(&module->getContext(), module, builder);
};

/// UTILITY ///

Function *Wrapper::createMain() {
    FunctionType *FT = FunctionType::get(getIntTy(64), {getIntTy(64), getIntPtrTy(8)}, false);
    main = Function::Create(FT, Function::ExternalLinkage, "main", *module);
    verifyFunction(*main, &errs());
    return main;
}

/// DEBUG ///

void Wrapper::dump(raw_fd_ostream &os) { module->print(os, nullptr); }

/// GETTER ///

LLVMContext *Wrapper::getContext() { return context; }
Module *Wrapper::getModule() { return module; }
IRBuilder<> *Wrapper::getBuilder() { return builder; }

IntegerType *Wrapper::getSizeTy() { return getIntTy(64); }
IntegerType *Wrapper::getIntTy(size_t bit) { return Type::getIntNTy(*context, bit); }
PointerType *Wrapper::getIntPtrTy(size_t bit) { return Type::getIntNPtrTy(*context, bit); }
PointerType *Wrapper::getIntPtrPtrTy(size_t bit) { return getIntPtrTy(bit)->getPointerTo(); }


/// BUILDER ///
Value *Wrapper::allocate(Type *type, std::string name) { return builder->CreateAlloca(type, nullptr, name); }
Value *Wrapper::call(Function *function, std::vector<Value *> args, std::string name) { return builder->CreateCall(function, args, name); };
Function *Wrapper::declare(Type *type, std::vector<Type *> parameters, std::string name) {
    FunctionType *FT = FunctionType::get(type, parameters, false);
    Function *F = Function::Create(FT, Function::ExternalLinkage, name, *module);
    llvm::verifyFunction(*F);
    return F;
}

Value *Wrapper::malloc(Type *type, Value *size, std::string name) { return call(memoryFunctions[type]["malloc"], {size}, name); }
void Wrapper::free(Type *type, Value *value) { call(memoryFunctions[type]["free"], {value}); }
Value *Wrapper::memcpy(Type *type, Value *dest, Value *source, Value *size, std::string name) { return call(memoryFunctions[type]["memcpy"], {dest, source, size}, name); }

void Wrapper::createMemoryFunctions(Type *type) {
    PointerType *ptrType = type->getPointerTo();
    memoryFunctions[type]["malloc"] = Function::Create(FunctionType::get(ptrType, {getSizeTy()}, false), Function::ExternalLinkage, "malloc", *module);
    memoryFunctions[type]["free"] = Function::Create(FunctionType::get(builder->getVoidTy(), {ptrType}, false), Function::ExternalLinkage, "free", *module);
    memoryFunctions[type]["memcpy"] =  Function::Create(FunctionType::get(ptrType, {ptrType, ptrType, getSizeTy()}, false), Function::ExternalLinkage, "memcpy", *module);
    llvm::verifyFunction(*memoryFunctions[type]["malloc"]);
    llvm::verifyFunction(*memoryFunctions[type]["free"]);
    llvm::verifyFunction(*memoryFunctions[type]["memcpy"]);
}

}