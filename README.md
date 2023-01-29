# llvm-types

> __Warning__ \
> This project is unfinished. Use at your own risk.

Dynamic Array & String type in LLVM IR.

###### How to use

```cpp
// main.cpp
#define LLVM_ARRAY_IMPL
#include "llvm_array.hpp"

int main(void) {
    llvmarr::LLVMContext *context = new llvmarr::LLVMContext();
    llvmarr::Module *module = new llvmarr::Module("tutorial", *context);
    llvmarr::IRBuilder<> *builder = new llvmarr::IRBuilder<>(*context);
    llvmarr::Type *type = builder->getInt64Ty();
    llvmarr::Array *arrayType = new llvmarr::Array(context, module, builder, type);
    return 0;
}
```
```zsh
clang++ main.cpp `llvm-config --cxxflags --ldflags --system-libs --libs core` -std=c++20 -stdlib=libc++ 
```