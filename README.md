# Eisdrache

> __Warning__ \
> This project is unfinished. Use at your own risk.

### Custom wrapper for the LLVM API in C++

- Simplified `IRBuilder`
- Implementation for dynamic arrays

#### How to Use

###### Hello World

```cpp
#include "eisdrache.hpp"

int main(void) {
    Eisdrache::initialize();
    Eisdrache *eisdrache = Eisdrache::create("test compiler");
    
    // i64 @puts(i8* nocapture %buffer)
    Eisdrache::Func &puts = eisdrache->declareFunction(eisdrache->getIntTy(), "puts", {eisdrache->getIntPtrTy(8)});
    puts.arg(0)->addAttr(Attribute::NoCapture);
    
    // i64 @main(i64 %argc, i8** %argv)
    Eisdrache::Func &main = eisdrache->declareFunction(eisdrache->getIntTy(), "main", 
        {{"argc", eisdrache->getIntTy()}, {"argv", eisdrache->getIntPtrPtrTy(8)}}, true);
    // @literal = private unnamed_addr constant [14 x i8] c"Hello World!\0A\00"
    Constant *literal = eisdrache->getLiteral("Hello World!\n", "literal");
    // %0 = call i64 @puts(ptr @literal)
    eisdrache->callFunction(puts, {literal});
    // ret %argc
    eisdrache->createRet(main.arg(0));
    eisdrache->verifyFunc(main);

    eisdrache->dump();
    return 0;
}
```

###### Compile & Run

```zsh
clang++ main.cpp eisdrache.cpp `llvm-config --cxxflags --ldflags --system-libs --libs core` -std=c++20 -stdlib=libc++ 
./a.out
```