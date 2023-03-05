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
    using ValueVec = std::vector<Value *>;
    using TypeVec = std::vector<Type *>;
    using InstVec = std::vector<Instruction *>;

    /**
     * @brief Custom Type Class;
     * 
     * This class contains
     * * the amount of bits (0 = void), 
     * * wether the type is a floating point type,
     * * and the pointer depth.
     */
    class Ty {
    public:
        using Vec = std::vector<Ty>;
        using Map = std::map<std::string, Ty>;

        Ty(Eisdrache *eisdrache = nullptr, size_t bit = 0, size_t ptrDepth = 0, bool isFloat = false, bool isSigned = false);

        Ty &operator=(const Ty &copy);
        bool operator==(const Ty &comp);
        // NOTE: can not check wether types are completely equal
        bool operator==(const Type *comp);
        // return this Ty with pointer depth - 1 ("dereference")
        Ty operator*();

        // get the equivalent llvm::Type 
        Type *getTy();
        // get this type with pointer depth + 1
        Ty getPtrTo();
        bool isFloatTy();
        bool isSignedTy();

    private:
        size_t bit;
        size_t ptrDepth;
        bool isFloat;
        bool isSigned;

        Eisdrache *eisdrache;
    };

    /**
     * @brief Wrapper for llvm::Value | llvm::AllocaInst;
     * 
     * This class contains 
     * * the value itself,
     * * wether the (integer) value is signed,
     * * wether the value is a llvm::AllocaInst,
     * * and the value to be assigned once the value is referenced. 
     * (Relevant for llvm::AllocaInst)
     */
    class Local {
    public:
        using Vec = std::vector<Local>;

        Local(Eisdrache *eisdrache = nullptr, Value *ptr = nullptr, Value *future = nullptr);
        
        Local &operator=(const Local &copy);
        bool operator==(const Local &comp) const;
        bool operator==(const Value *comp) const;
        AllocaInst *operator*();

        AllocaInst *getAllocaPtr();
        Value *getValuePtr();
        Ty &getTy();

        bool isAlloca();
        bool isSigned();

        Value *loadValue();
        void invokeFuture();

    private:
        union {
            Value *v_ptr;
            AllocaInst *a_ptr;
        };
        Ty type;
        Value *future;

        Eisdrache *eisdrache;
    };

    /**
     * @brief Wrapper for llvm::Function.
     * 
     * This class contains all locals, parameters and blocks 
     * from the wrapped llvm::Function. Can be initialized by the user, 
     * but should be initialized by the Eisdrache Wrapper.
     * 
     * @example
     * Eisdrache::Func &main = eisdrache->declareFunction(eisdrache->getIntTy(), "main",
     *      {{"argc", eisdrache->getSizeTy()}, {"argv", eisdrache->getIntPtrPtrTy(8)}});
     */
    class Func {
    public:
        using Vec = std::vector<Func>;
        using Map = std::unordered_map<std::string, Func>;
        using ParamMap = std::map<std::string, Type *>;
        using BlockVec = std::vector<BasicBlock *>;

        Func();
        Func(Eisdrache *eisdrache, Type *type, std::string name, ParamMap parameters, bool entry = false);
        ~Func();

        Func &operator=(const Func &copy);
        bool operator==(const Func &comp) const;
        bool operator==(const Function *comp) const;
        // get type of a local / parameter
        Type *operator[](Value *local);
        // get the wrapped llvm::Function
        Function *operator*();

        // get argument at index
        Argument *arg(size_t index);
        // call this function
        Value *call(ValueVec args = {}, std::string name = "");
        // add a local variable to this function
        // and return reference to copy of local
        Local &addLocal(Local local);

    private:
        Function *func;
        Type *type;
        Local::Vec locals;

        Eisdrache *eisdrache;
    };

    /**
     * @brief Wrapper for llvm::StructType
     * 
     * This class contains the struct type itself and all types of its elements. 
     * Can be initialized by the user, but should be initialized by the Eisdrache Wrapper.
     * 
     * @example
     * Eisdrache::Struct &array = eisdrache->declareStruct("array", {eisdrache->getIntPtrTy(), eisdrache->getSizeTy()});
     */
    class Struct {
    public:
        using Vec = std::vector<Struct>;
        using Map = std::unordered_map<std::string, Struct>;

        Struct();
        Struct(Eisdrache *eisdrache, std::string name, TypeVec elements);
        ~Struct();

        Struct &operator=(const Struct &copy);
        bool operator==(const Struct &comp) const;
        bool operator==(const Type *comp) const;
        // get type of an element at an index
        Type *operator[](size_t index);
        // get the wrapped llvm::StructType
        StructType *operator*();
        
        // allocate object of this type
        Local &allocate(std::string name = "");
        // get the pointer to this type
        PointerType *getPtr();

    private:
        StructType *type;
        PointerType *ptr;

        Eisdrache *eisdrache;
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
    Ty getVoidTy();
    // Type: i1
    Ty getBoolTy();
    // Type: i64 (size_t)
    Ty getSizeTy();
    // Type: bit
    Ty getSignedTy(size_t bit);
    // Type: bit*
    Ty getSignedPtrTy(size_t bit);
    // Type: bit**
    Ty getSignedPtrPtrTy(size_t bit);
    // Type: bit
    Ty getUnsignedTy(size_t bit);
    // Type: bit*
    Ty getUnsignedPtrTy(size_t bit);
    // Type: bit**
    Ty getUnsignedPtrPtrTy(size_t bit);
    // Type: 16 = half, 32 = float, 64 = double
    Ty getFloatTy(size_t bit);
    // Type: 16 = half*, 32 = float*, 64 = double*
    Ty getFloatPtrTy(size_t bit);
    // Type: 16 = half**, 32 = float**, 64 = double**
    Ty getFloatPtrPtrTy(size_t bit);

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
     * @return Local & - Wrapped alloca instruction
     */
    Local &declareLocal(Type *type, std::string name = "", Value *value = nullptr);

    /**
     * @brief Load the value of a local variable.
     * 
     * @param local Pointer to the local.
     * @param name (optional) Name of the loaded value.
     * @return Value * - The loaded value. 
     */
    Value *loadLocal(Value *local, std::string name = "");

    /// STRUCT TYPES ///

    /**
     * @brief Declare a struct type.
     * 
     * @param name Name of the struct type
     * @param elements Types of the elements of the struct type
     * @return Struct & - Eisdrache::Struct (wrapped llvm::StructType) 
     */
    Struct &declareStruct(std::string name, TypeVec elements);

    /**
     * @brief Allocate object of struct type.
     *      Automatically appends to Eisdrache::Func::locals.
     * 
     * @param wrap Wrapped llvm::StructType
     * @param name Name of the returned pointer
     * @return Local & - Wrapped alloca instruction
     */
    Local &allocateStruct(Struct &wrap, std::string name = "");
    /**
     * @brief Allocate object of struct type.
     *      Automatically appends to Eisdrache::Func::locals.
     * 
     * @param typeName Name of the struct type.
     * @param name Name of the returned pointer
     * @return Local & - Wrapped alloca instruction
     */
    Local &allocateStruct(std::string typeName, std::string name = "");

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
    /**
     * @brief Set the current insertion block.
     * 
     * @param block The insertion block
     */
    void setBlock(BasicBlock *block);

    /// GETTER ///

    /**
     * @brief Get the llvm::LLVMContext
     * 
     * @return LLVMContext * 
     */
    LLVMContext *getContext();
    /**
     * @brief Get the llvm::Module
     * 
     * @return Module * 
     */
    Module *getModule();
    /**
     * @brief Get the llvm::IRBuilder
     * 
     * @return IRBuilder<> * 
     */
    IRBuilder<> *getBuilder();
    /**
     * @brief Get the current wrapped parent llvm::Function
     * 
     * @return Func & 
     */
    Func &getCurrentParent();


private:
    Eisdrache(LLVMContext *, Module *, IRBuilder<> *, std::string);

    static std::nullptr_t complain(std::string);

    LLVMContext *context;
    Module *module;
    IRBuilder<> *builder;

    Func *parent; // current parent function

    Func::Map functions;
    Struct::Map structs;
};

} // namespace llvm