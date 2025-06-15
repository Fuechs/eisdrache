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

SOON

### 3.2 Expression Code Generation

SOON


### 3.3 Function Code Generation

SOON

### Chapter III Code

SOON

### 