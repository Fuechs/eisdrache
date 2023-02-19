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
    llvm::Eisdrache *eisdrache = llvm::Eisdrache::create("example");
    
    // type { i64 }
    StructType *testType = eisdrache->createType({eisdrache->getSizeTy()}, "test_type");
    
    // i64 @main (i64, i8*)
    llvm::Function *main = eisdrache->declare(eisdrache->getIntTy(64), 
        {eisdrache->getIntTy(64), eisdrache->getIntPtrTy(8)}, "main", true);
    
    // %allocated = alloca %test_type
    Value *allocated = eisdrache->allocate(testType, "allocated");
    
    // store i64 0, ptr allocated.elements[0]
    eisdrache->store(eisdrache->getInt(64, 0), allocated, 0);
    
    // %val_ptr_ = getelementptr %test_type, ptr %allocated, i64 0, i32 0
    // %val = load i64, ptr %val_ptr_
    Value *val = eisdrache->getElementVal(allocated, 0, "val");
    
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