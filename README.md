# Eisdrache

> __Warning__ \
> This project is unfinished. Use at your own risk.

### Custom wrapper for the LLVM API in C++

- Simplified `IRBuilder`
- Implementation for dynamic arrays

###### How to use

```cpp
// main.cpp
#include "eisdrache.hpp"

int main(void) {
    llvm::Eisdrache *eisdrache = new llvm::Eisdrache();
    // i64 @main (i64, i8*)
    llvm::Function *main = eisdrache->createMain();
    llvm::BasicBlock *entry = llvm::BasicBlock::Create(*eisdrache->getContext(), "entry", main);
    eisdrache->getBuilder()->SetInsertPoint(entry);
    eisdrache->getBuilder()->CreateRet(eisdrache->getInt(64, 0));
    llvm::verifyFunction(*main);
    eisdrache->dump();
    return 0;
}
```

```zsh
clang++ main.cpp eisdrache.cpp `llvm-config --cxxflags --ldflags --system-libs --libs core` -std=c++20 -stdlib=libc++ 
```