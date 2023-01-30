# LLVM C++ Wrapper Wrapper

> __Warning__ \
> This project is unfinished. Use at your own risk.

Custom wrapper for the LLVM C++ wrapper. Including dynamic arrays & strings.

###### How to use

```cpp
// main.cpp
int main(void) { /* SOON. */ }
```

```zsh
clang++ main.cpp `llvm-config --cxxflags --ldflags --system-libs --libs core` -std=c++20 -stdlib=libc++ 
```