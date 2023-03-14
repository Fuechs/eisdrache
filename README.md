# Eisdrache

> __Warning__ \
> This project is unfinished. Use at your own risk.

### Custom wrapper for the LLVM API in C++

- Wrappers for Types `Ty`, Locals `Local`, Functions `Func` and Structures `Struct`
- Simplified Load, GEP, Binary OP, Type Cast, Bit Cast and Branching (WIP)
- Support for future value assignment or calls for locals
- Implementation for dynamic arrays `Array` (WIP)

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
using Array = Eisdrache::Array;

int main(void) {
    Eisdrache::initialize();
    Eisdrache *eisdrache = Eisdrache::create("test compiler");

    // %vector = type { ptr, i64, i64, i64 }
    // ...functions...
    Array *array = new Array(eisdrache, eisdrache->getUnsignedTy(64), "vector");  

    // i64 @main(i64 %argc, i8** %argv)
    Eisdrache::Func &main = eisdrache->declareFunction(eisdrache->getSignedTy(64), "main", 
        {{"argc", eisdrache->getUnsignedTy(64)}, {"argv", eisdrache->getUnsignedPtrPtrTy(8)}}, true);
    // %list = alloca %vector
    Eisdrache::Local &list = array->allocate("list");
    // call void @vector_constructor(ptr %list)
    array->call(Array::CONSTRUCTOR, {list.getValuePtr()});
    // call void @vector_resize(ptr %list, i64 2)
    array->call(Array::RESIZE, {list.getValuePtr(), eisdrache->getInt(64, 2)});
    // %buffer = call ptr @vector_get_buffer(ptr %list)
    Eisdrache::Local &buffer = array->call(Array::GET_BUFFER, {list.getValuePtr()}, "buffer");
    // %fst_ptr = getelementptr ptr, ptr %buffer, i32 0
    Eisdrache::Local &fst_ptr = eisdrache->getArrayElement(buffer, 0, "fst_ptr");
    // store i64 69, ptr %fst_ptr
    eisdrache->storeValue(fst_ptr, eisdrache->getInt(64, 69));
    // %fst = load i64, ptr %fst_ptr
    Eisdrache::Local &fst = fst_ptr.loadValue(true, "fst");
    // call void @vector_destructor(ptr %list)
    array->call(Array::DESTRUCTOR, {list.getValuePtr()});
    // ret i64 %fst
    eisdrache->createRet(fst);
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