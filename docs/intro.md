# Introduction to Eisdrache

This introduction is heavily inspired by the [original LLVM tutorial](https://llvm.org/docs/tutorial/). 
It starts at Chapter 3, so follow the original tutorial first and then continue here.
The full code can be viewed [here](intro.cpp).

## Kaleidoscope: Implementing a Language with Eisdrache

* [Chapter I — Kaleidoscope Introduction and Lexer](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl01.html)
* [Chapter II — Implementing a Parser and AST](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl02.html)
* [Chapter III — Code Generation to LLVM IR](#chapter-iii--code-generation-to-llvm-ir)
* Chapter IV — SOON

## Chapter III — Code Generation to LLVM IR

At this point, you should've gone through the first two chapters of the LLVM tutorial. 
This tutorial will continue with the same code. 

> [!IMPORTANT]
> Remember to add `#include "path/to/eisdrache.hpp"` to the top of your file! \
> This tutorial also omits `Eisdrache::` in front of Eisdrache classes for simplicity. 
> Either add a `using` declaration to the top of your file or remember to use e.g. `Eisdrache::Entity` instead of just `Entity`. 

* [3.1 Code Generation Setup](#31-code-generation-setup)
* [3.2 Expression Code Generation](#32-expression-code-generation)
* [3.2 Function Code Generation](#33-function-code-generation)
* [Chapter III Code](#chapter-iii-code)

### 3.1 Code Generation Setup

To generate the code, we want to be able to simply call `codegen()` on the generated ASTs.
This function will return a pointer to an `Entity`.

> [!NOTE]
> `Entity::Ptr` is equal to `std::shared_ptr<Entity>` (smart pointer).

```c++
/// ExprAST - Base class for all expression nodes.
class ExprAST {
public:
  virtual ~ExprAST() = default;
  virtual Entity::Ptr codegen() = 0;
};

/// NumberExprAST - Expression class for numeric literals like "1.0".
class NumberExprAST : public ExprAST {
  double Val;

public:
  NumberExprAST(double Val) : Val(Val) {}
  Entity::Ptr codegen() override;
};

...
```

Next we declare a variable for the Eisdrache wrapper. This class contains the context, builder and module.

```c++
static Eisdrache::Ptr eisdrache;
```

Pretty much everything happens through this class, like requesting 
types, values, declaring variables and managing function bodies.

### 3.2 Expression Code Generation

Generating the LLVM code is simplified as far as possible by the Eisdrache wrapper.

```c++
Eisdrache::Entity::Ptr NumberExprAST::codegen() {
  return eisdrache->createLocal(eisdrache->getFloatTy(64), eisdrache->getFloat(Val));
}
```

This code creates a `Local` with a 64-bit floating point type 
and assigns it the `Val` stored in the AST. To get the LLVM
representation of the constant, we have to call `getFloat()` 
with the value. Note that the `Local` we created here doesn't 
serve as a normal variable the user (of Kaleidoscope) interacts 
with but is just a temporary internal variable used in further 
operations. LLVM will automatically omit this in the final code.

> [!IMPORTANT]
> The `declareLocal()` function is similar but not correct for this case. 
> This function allocates memory for a more permanent variable and initializes 
> it. 

```c++
Eisdrache::Entity::Ptr VariableExprAST::codegen() {
  return eisdrache->getCurrentParent()->getLocal(Name);
}
```

This code requests the current function we're writing into.
With this class, we can get all variables the function has 
access to, by their names.

### 3.3 Function Code Generation

SOON

### Chapter III Code

SOON

### 