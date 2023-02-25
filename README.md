# Eisdrache

> __Warning__ \
> This project is unfinished. Use at your own risk.

### Custom wrapper for the LLVM API in C++

- Simplified `IRBuilder`
- Implementation for dynamic arrays

#### How to Use

###### Hello World

```cpp
// main.cpp
#include "eisdrache.hpp"

using namespace llvm;

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
    puts.call({literal});
    // ret i64 0
    eisdrache->createRet(eisdrache->getInt(64, 0));
    eisdrache->verifyFunc(main);
    
    eisdrache->dump();
    return 0;
}
```

###### Local Variables 

```cpp
// main.cpp
#include "eisdrache.hpp"

using namespace llvm;

int main(void) {
    Eisdrache::initialize();
    Eisdrache *eisdrache = Eisdrache::create("test compiler");    
        
    // i64 @main(i64 %argc, i8** %argv)
    Eisdrache::Func &main = eisdrache->declareFunction(eisdrache->getIntTy(), "main", 
        {{"argc", eisdrache->getIntTy()}, {"argv", eisdrache->getIntPtrPtrTy(8)}}, true);
    // %var = alloca i64
    Value *var = eisdrache->declareLocal(eisdrache->getIntTy(), "var", eisdrache->getInt(64, 3));
    // store i64 3, ptr %var ; (future value assigned from declaration)
    // %var_load = load i64, ptr %var
    Value *load = eisdrache->loadLocal(var, "var_load");
    // ret i64 %var_load
    eisdrache->createRet(load);
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