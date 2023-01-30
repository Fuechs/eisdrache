/**
 * @file wrapper.cpp
 * @author fuechs
 * @brief LLVM Wrapper Wrapper class implementation
 * @version 0.1
 * @date 2023-01-30
 * 
 * @copyright Copyright (c) 2023, Fuechs.
 * 
 */

#include "wrapper.hpp"

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

IntegerType *Wrapper::getIntTy(size_t bit) { return Type::getIntNTy(*context, bit); }
PointerType *Wrapper::getIntPtrTy(size_t bit) { return Type::getIntNPtrTy(*context, bit); }

}