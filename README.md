# llvm-types

> __Warning__ \
> This project is unfinished. Use at your own risk.

Dynamic Array & String type in LLVM IR.

###### How to use

```cpp
// main.cpp
#define LLVM_ARRAY_IMPL
#include "llvm_array.hpp"

using namespace llvmarr;

int main(void) {
    LLVMContext *context = new LLVMContext();
    Module *module = new Module("test", *context);
    IRBuilder<> *builder = new IRBuilder<>(*context);
    Type *type = builder->getInt64Ty();

    Memory *mem = new Memory(context, module, builder, type);
    Array *array = new Array(context, module, mem, "test_array");

    FunctionType *FT = FunctionType::get(builder->getInt64Ty(), false);
    Function *main = Function::Create(FT, Function::ExternalLinkage, "main", *module);
    BasicBlock *BB = BasicBlock::Create(*context, "entry", main);
    builder->SetInsertPoint(BB);
    Value *vector = array->allocate("vector");
    array->initialize(vector);
    array->del(vector);
    builder->CreateRet(builder->getInt64(0));
    llvm::verifyFunction(*main);

    module->print(llvm::errs(), nullptr);

    return 0;
}
```

> You can see the current output for this [here](./llvm_array.ll).

```zsh
clang++ main.cpp `llvm-config --cxxflags --ldflags --system-libs --libs core` -std=c++20 -stdlib=libc++ 
```