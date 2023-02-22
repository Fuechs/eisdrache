/**
 * @file eisdrache.hpp
 * @author fuechs
 * @brief Eisdrache class header
 * @version 0.1
 * @date 2023-01-30
 * 
 * @copyright Copyright (c) 2023, Fuechs.
 * 
 */

#pragma once

#include <iostream>
#include <string>
#include <vector>
#include <map>

#include <llvm/PassRegistry.h>
#include "llvm/InitializePasses.h"
#include <llvm/ADT/Triple.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include "llvm/Support/TargetSelect.h"
#include <llvm/MC/TargetRegistry.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>


namespace llvm {

/**
 * @brief Eisdrache Wrapper for the LLVM API.
 * 
 * This wrapper offers a simplified version of the llvm::IRBuilder to avoid
 * unnecessary code.
 */
class Eisdrache {
public:
    typedef std::vector<Value *> ValueVec;
    typedef std::vector<Type *> TypeVec;

    /**
     * @brief Wrapper for llvm::Function.
     * 
     * This struct contains all locals, parameters and blocks 
     * from the wrapped llvm::Function. Can be initialized by the user, 
     * but should be initialized by the Eisdrache Wrapper.
     * 
     * @example
     * Eisdrache::Func &main = eisdrache->declareFunction(eisdrache->getIntTy(), "main",
     *      {{"argc", eisdrache->getSizeTy()}, {"argv", eisdrache->getIntPtrPtrTy(8)}});
     */
    struct Func {
        typedef std::vector<Func> Vec;
        typedef std::unordered_map<std::string, Func> Map; 
        typedef std::map<std::string, Type *> ParamMap;
        typedef std::vector<BasicBlock *> BlockVec;
        typedef std::vector<AllocaInst *> LocalVec;

        Func();
        Func(Module *module, IRBuilder<> *builder, Type *type, std::string name, ParamMap parameters, bool entry = false);
        ~Func();

        Func &operator=(const Func &copy);
        bool operator==(const Func &comp) const;
        bool operator==(const Function *comp) const;
        // get type of a local / parameter
        Type *operator[](Value *local);

        // get argument at index
        Argument *arg(size_t index);

        Function *func;
        Type *type;
        LocalVec locals;
    };

    /**
     * @brief Wrapper for llvm::StructType
     * 
     * This struct contains the struct type itself and all types of its elements. 
     * Can be initialized by the user, but should be initialized by the Eisdrache Wrapper.
     * 
     */
    struct Struct {
        typedef std::vector<Struct> Vec;

        Struct();
        Struct(Module *module, IRBuilder<> *builder, std::string name, TypeVec elements);
        ~Struct();

        Struct &operator=(const Struct &copy);
        bool operator==(const Struct &comp) const;
        bool operator==(const Type *comp) const;
        // get type of an element at an index
        Type *operator[](size_t index);

        StructType *type;
        PointerType *ptr;
    };

    ~Eisdrache();

    // Initialize the LLVM API
    static void initialize();

    static Eisdrache *create(std::string moduleID, std::string targetTriple = "");
    static Eisdrache *create(LLVMContext *context, std::string moduleID, std::string targetTriple = "");
    static Eisdrache *create(Module *module, std::string targetTriple = "");
    static Eisdrache *create(Module *module, IRBuilder<> *builder, std::string targetTriple = "");

    // dump the generated LLVM IR
    void dump(raw_fd_ostream &os = errs());

    /// TYPES //

    // Type: void
    Type *getVoidTy();
    // Type: i1
    IntegerType *getBoolTy();
    // Type: i64 (size_t)
    IntegerType *getSizeTy();
    // Type: bit
    IntegerType *getIntTy(size_t bit = 64);
    // Type: bit*
    PointerType *getIntPtrTy(size_t bit = 64);
    // Type: bit**
    PointerType *getIntPtrPtrTy(size_t bit = 64);
    // Type: 16 = half, 32 = float, 64 = double
    Type *getFloatTy(size_t bit = 64);
    // Type: 16 = half*, 32 = float*, 64 = double*
    Type *getFloatPtrTy(size_t bit  = 64);   
    // Type: 16 = half**, 32 = float**, 64 = double**
    Type *getFloatPtrPtrTy(size_t bit  = 64);

    // get the element type of a Value *
    // if instruction is not supported function will assert
    Type *getElementTy(Value *value);

    /// VALUES ///

    ConstantInt *getBool(bool value);
    ConstantInt *getInt(size_t bit, size_t value);
    ConstantFP *getFloat(double value);
    Constant *getLiteral(std::string value, std::string name = "");

    /// FUNCTIONS ///
    
    /**
     * @brief Declare a llvm::Function without parameters names.
     * 
     * @param type return type of the function
     * @param name name of the function
     * @param parameters parameters of the function 
     * @return Func & - Eisdrache::Func (wrapped llvm::Function)
     */
    Func &declareFunction(Type *type, std::string name, TypeVec parameters);
    /**
     * @brief Declare a llvm::Function.
     * 
     * @param type return type of the function
     * @param name name of the function
     * @param parameters (optional) parameters of the function 
     * @param entry (optional) creates entry llvm::BasicBlock in function body if true
     * @return Func & - Eisdrache::Func (wrapped llvm::Function)
     */
    Func &declareFunction(Type *type, std::string name, Func::ParamMap parameters = Func::ParamMap(), bool entry = false);
    /**
     * @brief Get the Eisdrache::Func wrapper object
     * 
     * @param function Pointer to llvm::Function
     * @return Func & - Eisdrache::Func (wrapped llvm::Function)
     */
    Func &getWrap(Function *function);
    /**
     * @brief Verify that a Eisdrache::Func is error-free.
     *          TODO: Implement this function.
     * @param wrap Eisdrache::Func (wrapped llvm::Function)
     * @return true - Eisdrache::Func is error-free.
     * @return false - Eisdrache::Func contains errors.
     */
    bool verifyFunc(Func &wrap);
    /**
     * @brief Call a llvm::Function by its address.
     * 
     * @param func Pointer to llvm::Function
     * @param args (optional) Function call arguments
     * @param name (optional) Name of the returned value
     * @return Value * - Value returned from the call.
     */
    Value *callFunction(Function *func, ValueVec args = {}, std::string name = "");
    /**
     * @brief Call a llvm::Function by its wrap.
     * 
     * @param wrap Eisdrache::Func (wrapped llvm::Function) of the callee function
     * @param args (optional) Function call arguments
     * @param name (optional) Name of the returned value
     * @return Value * - Value returned from the call.
     */
    Value *callFunction(Func &wrap, ValueVec args = {}, std::string name = "");
    /**
     * @brief Call a llvm::Function by its name.
     * 
     * @param callee Name of the callee function
     * @param args (optional) Function call arguments
     * @param name (optional) Name of the returned value
     * @return Value * - Value returned from the call.
     */
    Value *callFunction(std::string callee, ValueVec args = {}, std::string name = "");

    /// LOCALS ///

    /**
     * @brief Declare (Allocate) a local variable. Automatically adds llvm::AllocaInst to the parent (Eisdrache::Func)
     * 
     * @param type Type to allocate
     * @param name (optional) Name of the AllocaInst *
     * @param value (optional) Future value to be assigned to local variable
     * @return AllocaInst * - Allocate Instruction returned from llvm::IRBuilder
     */
    AllocaInst *declareLocal(Type *type, std::string name = "", Value *value = nullptr);

    /// MEMORY ///

    /**
     * @brief Call TYPE* \@malloc(SIZE); 
     *      This allocates memory of the given type.
     * @param type Type to allocate
     * @param size Amount to allocate
     * @param name (optional) Name of the returned pointer
     * @return Value * - Pointer returned from \@malloc().
     * 
     */
    Value *callMalloc(Type *type, Value *size, std::string name = "");
    Value *callFree(Type *type, Value *value);
    Value *callMemcpy(Type *type, Value *dest, Value *source, Value *size, std::string name = "");

    /// STRUCT TYPES ///

    /// BUILDER ///

    /**
     * @brief Create a void return instruction.
     * 
     * @param next (optional) Next insertion point
     * @return ReturnInst * - Return Instruction returned from llvm::IRBuilder
     */
    ReturnInst *createRet(BasicBlock *next = nullptr);
    /**
     * @brief Create a return instruction with a value.
     * 
     * @param value Value to return
     * @param next (optional) Next insertion point
     * @return ReturnInst * - Return Instruction returned from llvm::IRBuilder
     */
    ReturnInst *createRet(Value *value, BasicBlock *next = nullptr);

    /// GETTER ///

    LLVMContext *getContext();
    Module *getModule();
    IRBuilder<> *getBuilder();


private:
    typedef std::unordered_map<AllocaInst *, Value *> FutureMap;
    typedef std::unordered_map<Type *, std::map<std::string, Function *>> MemoryFuncMap;

    Eisdrache(LLVMContext *, Module *, IRBuilder<> *, std::string);

    static std::nullptr_t complain(std::string);

    LLVMContext *context;
    Module *module;
    IRBuilder<> *builder;

    Func::Map functions;
    FutureMap futures; // future values to be assigned to locals when they are referenced
    MemoryFuncMap memoryFunctions;
};

} // namespace llvm