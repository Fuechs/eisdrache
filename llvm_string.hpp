/**
 * @file llvm_string.hpp
 * @author fuechs
 * @brief llvm string type
 * @version 0.1
 * @date 2023-01-29
 * 
 * @copyright Copyright (c) 2023, Fuechs.
 * 
 */

#pragma once

#ifdef LLVM_STRING_IMPL
#define LLVM_ARRAY_IMPL
#endif
#include "llvm_array.hpp"

namespace llvmstr {

using namespace llvmarr;

class String {
public:
    String(LLVMContext *context, Module *module, IRBuilder<> *builder);
    ~String();
};

#ifdef LLVM_STRING_IMPL

String::String(LLVMContext *context, Module *module, IRBuilder<> *builder) {

}
String::~String() {}

#endif // LLVM_STRING_IMPL

}