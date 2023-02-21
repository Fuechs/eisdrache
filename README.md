# Eisdrache

> __Warning__ \
> This project is unfinished. Use at your own risk.

### Custom wrapper for the LLVM API in C++

- Simplified `IRBuilder`
- Implementation for dynamic arrays

###### How to use

###### Struct Types

```cpp
// main.cpp
#include "eisdrache.hpp"

int main(void) {
    llvm::Eisdrache::init();
    llvm::Eisdrache *eisdrache = llvm::Eisdrache::create("test compiler");
     // type { i64, half }
    llvm::StructType *testType = eisdrache->createType(
        {eisdrache->getSizeTy(), eisdrache->getFloatTy(16)}, "test_type");
    // i64 @main (i64, i8**)
    llvm::Function *main = eisdrache->declare(eisdrache->getIntTy(64), 
        {eisdrache->getIntTy(64), eisdrache->getIntPtrPtrTy(8)}, "main", true);
    // %allocated = alloca %test_type
    llvm::Value *allocated = eisdrache->allocate(testType, "allocated");
    // store i64 0, ptr allocated.elements[0] (once it gets referenced)
    llvm::Value *elemPtr = eisdrache->getElementPtr(allocated, 0, "val_ptr");
    eisdrache->setFuture(elemPtr, eisdrache->getInt(64, 0));
    // %val = load i64, ptr %val_ptr
    llvm::Value *val = eisdrache->loadValue(elemPtr, "val", true);
    // ret i64 %val
    eisdrache->createRet(val);
    llvm::verifyFunction(*main);
    eisdrache->dump();
    return 0;
}
```

###### Hello World

```cpp
#include "eisdrache.hpp"

int main(void) {
    llvm::Eisdrache::init();
    llvm::Eisdrache *eisdrache = llvm::Eisdrache::create("test compiler");
    // i64 @puts (i8* nocapture)
    llvm::Function *puts = eisdrache->declare(eisdrache->getIntTy(64), {eisdrache->getIntPtrTy(8)}, "puts");
    puts->getArg(0)->addAttr(llvm::Attribute::NoCapture);
    // i64 @main (i64, i8**)
    llvm::Function *main = eisdrache->declare(eisdrache->getIntTy(64), 
        {eisdrache->getIntTy(64), eisdrache->getIntPtrPtrTy(8)}, "main", true);
    // @message = private unnamed_addr constant [14 x i8] c"Hello World!\0A\00"
    llvm::Value *message = eisdrache->literal("Hello World!\n", "message");
    // call i64 @puts(ptr @message)
    eisdrache->call(puts, {message});
    // ret i64 0
    eisdrache->createRet(eisdrache->getInt(64, 0));
    llvm::verifyFunction(*main);
    eisdrache->dump();
    return 0;
}
```

###### Compile & Run

```zsh
clang++ main.cpp eisdrache.cpp `llvm-config --cxxflags --ldflags --system-libs --libs core` -std=c++20 -stdlib=libc++ 
./a.out
```