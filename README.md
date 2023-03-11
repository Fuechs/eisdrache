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

    // i64 @puts(i8* nocapture)
    Eisdrache::Func &puts = eisdrache->declareFunction(eisdrache->getSignedTy(64), 
        "puts", {eisdrache->getUnsignedPtrTy(8)});
    dyn_cast<Argument>(puts.arg(0).getValuePtr())->addAttr(Attribute::NoCapture);
        
    // i64 @main(i64 %argc, i8** %argv)
    Eisdrache::Func &main = eisdrache->declareFunction(eisdrache->getSignedTy(64), "main", 
        {{"argc", eisdrache->getUnsignedTy(64)}, {"argv", eisdrache->getUnsignedPtrPtrTy(8)}}, true);
    // @literal = private unnamed_addr constant [14 x i8] c"Hello World!\0A\00"
    Constant *literal = eisdrache->getLiteral("Hello World!\n", "literal");
    // %status = call i64 @puts(ptr @literal)
    Eisdrache::Local &status = puts.call({literal}, "status");
    // ret i64 %status
    eisdrache->createRet(status);
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
    Eisdrache::Func &main = eisdrache->declareFunction(eisdrache->getSignedTy(64), "main", 
        {{"argc", eisdrache->getUnsignedTy(64)}, {"argv", eisdrache->getUnsignedPtrPtrTy(8)}}, true);
    // %var = alloca i64
    Eisdrache::Local &var = eisdrache->declareLocal(eisdrache->getUnsignedTy(64), "var", eisdrache->getInt(64, 3));
    // store i64 3, ptr %var ; future value assigned from declaration
    // %var_load = load i64, ptr %var
    Eisdrache::Local &load = eisdrache->loadLocal(var);
    // ret i64 %var_load
    eisdrache->createRet(load);
    eisdrache->verifyFunc(main);

    eisdrache->dump();
    return 0;
}
```

###### Dynamic Arrays

```cpp
// main.cpp
#include "eisdrache.hpp"

using namespace llvm;

int main(void) {
    Eisdrache::initialize();
    Eisdrache *eisdrache = Eisdrache::create("test compiler");

    // %vector = type { ptr, i64, i64, i64 }
    // ...functions...
    Eisdrache::Array array = Eisdrache::Array(eisdrache, eisdrache->getUnsignedTy(64), "vector");  

    // i64 @main(i64 %argc, i8** %argv)
    Eisdrache::Func &main = eisdrache->declareFunction(eisdrache->getSignedTy(64), "main", 
        {{"argc", eisdrache->getUnsignedTy(64)}, {"argv", eisdrache->getUnsignedPtrPtrTy(8)}}, true);
    // %list = alloca %vector
    Eisdrache::Local &list = array.allocate("list");
    // %buffer = call ptr @vector_get_buffer(ptr %list)
    array.call(array.GET_BUFFER, {list.getValuePtr()}, "buffer");
    // %size = call i64 @vector_get_size(ptr %list);
    array.call(array.GET_SIZE, {list.getValuePtr()}, "size");
    // %max = call i64 @vector_get_max(ptr %list);
    array.call(array.GET_MAX, {list.getValuePtr()}, "max");
    // %factor = call i64 @vector_get_factor(ptr %list)
    array.call(array.GET_FACTOR, {list.getValuePtr()}, "factor");
    // ret i64 0
    eisdrache->createRet(eisdrache->getInt(64, 0));
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