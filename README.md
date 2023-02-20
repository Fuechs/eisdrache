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
    llvm::Eisdrache::init();
    llvm::Eisdrache *eisdrache = llvm::Eisdrache::create("test compiler");
     // type { i64, half }
    StructType *testType = eisdrache->createType(
        {eisdrache->getSizeTy(), eisdrache->getFloatTy(16)}, "test_type");
    // i64 @main (i64, i8*)
    llvm::Function *main = eisdrache->declare(eisdrache->getIntTy(64), 
        {eisdrache->getIntTy(64), eisdrache->getIntPtrTy(8)}, "main", true);
    // %allocated = alloca %test_type
    Value *allocated = eisdrache->allocate(testType, "allocated");
    // store i64 0, ptr allocated.elements[0] (once it gets referenced)
    Value *elemPtr = eisdrache->getElementPtr(allocated, 0, "val_ptr");
    eisdrache->setFuture(elemPtr, eisdrache->getInt(64, 0));
    // %val = load i64, ptr %val_ptr
    Value *val = eisdrache->loadValue(elemPtr, "val", true);
    // ret i64 %val
    eisdrache->createRet(val);
    llvm::verifyFunction(*main);
    eisdrache->dump();
    return 0;
}
```

```zsh
clang++ main.cpp eisdrache.cpp `llvm-config --cxxflags --ldflags --system-libs --libs core` -std=c++20 -stdlib=libc++ 
```