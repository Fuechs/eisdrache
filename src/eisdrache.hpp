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

    class Struct;

    /**
     * @brief Custom Type Class;
     * 
     * This class contains
     * * the amount of bits (0 = void), 
     * * wether the type is a floating point type,
     * * wether the type is signed,
     * * a possible struct type,
     * * and the pointer depth.
     */
    class Ty {
    public:
        using Vec = std::vector<Ty>;
        using Map = std::map<std::string, Ty>;

        Ty(Eisdrache *eisdrache = nullptr, size_t bit = 0, size_t ptrDepth = 0, bool isFloat = false, bool isSigned = false);
        Ty(Eisdrache *eisdrache, Struct &structTy, size_t ptrDepth = 0);

        Ty &operator=(const Ty &copy);
        bool operator==(const Ty &comp) const;
        // NOTE: can not check wether types are completely equal
        bool operator==(const Type *comp) const;
        // return this Ty with pointer depth - 1 ("dereference")
        Ty operator*() const;

        // get the equivalent llvm::Type 
        Type *getTy() const;
        // get this type with pointer depth + 1
        Ty getPtrTo() const;
        Struct &getStructTy() const;
        bool isFloatTy() const;
        bool isSignedTy() const;
        bool isPtrTy() const;

    private:
        size_t bit;
        size_t ptrDepth;

        bool isFloat;
        bool isSigned;

        Struct *structTy;

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

        Local(Eisdrache *eisdrache = nullptr, Ty type = Ty(), Value *ptr = nullptr, Value *future = nullptr);
        
        Local &operator=(const Local &copy);
        bool operator==(const Local &comp) const;
        bool operator==(const Value *comp) const;
        AllocaInst *operator*();

        void setPtr(Value *ptr);
        void setFuture(Value *future);

        AllocaInst *getAllocaPtr();
        Value *getValuePtr();
        const Ty &getTy();

        bool isAlloca();

        Local &loadValue(bool force = false, std::string name = "");
        void invokeFuture();

    private:
        union {
            Value *v_ptr;
            AllocaInst *a_ptr;
        };
        const Ty type;
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
        using BlockVec = std::vector<BasicBlock *>;

        Func();
        Func(Eisdrache *eisdrache, Ty type, std::string name, Ty::Map parameters, bool entry = false);
        ~Func();

        Func &operator=(const Func &copy);
        bool operator==(const Func &comp) const;
        bool operator==(const Function *comp) const;
        // get type of a local / parameter
        const Ty &operator[](Value *local);
        // get the wrapped llvm::Function
        Function *operator*();

        // get argument at index
        Local &arg(size_t index);
        // call this function
        Local &call(ValueVec args = {}, std::string name = "");
        // add a local variable to this function
        // and return reference to copy of local
        Local &addLocal(Local local);

        const Ty &getTy() const;

    private:
        Function *func;
        Ty type;
        Local::Vec parameters;
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
        Struct(Eisdrache *eisdrache, std::string name, Ty::Vec elements);
        ~Struct();

        Struct &operator=(const Struct &copy);
        bool operator==(const Struct &comp) const;
        bool operator==(const Type *comp) const;
        // get type of an element at an index
        const Ty &operator[](size_t index) const;
        // get the wrapped llvm::StructType
        StructType *operator*();
        
        // allocate object of this type
        Local &allocate(std::string name = "");
        // get the pointer to this type
        const Ty &getPtrTy() const;

    private:
        StructType *type;
        Ty ptr;
        Ty::Vec elements;

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
    ConstantInt *getInt(size_t bit, uint64_t value);
    Value *getNegative(ConstantInt *value);
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
    Func &declareFunction(Ty type, std::string name, Ty::Vec parameters);
    /**
     * @brief Declare a llvm::Function.
     * 
     * @param type return type of the function
     * @param name name of the function
     * @param parameters (optional) parameters of the function 
     * @param entry (optional) creates entry llvm::BasicBlock in function body if true
     * @return Func & - Eisdrache::Func (wrapped llvm::Function)
     */
    Func &declareFunction(Ty type, std::string name, Ty::Map parameters = Ty::Map(), bool entry = false);
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
     * @brief Call a llvm::Function by its wrap.
     * 
     * @param wrap Eisdrache::Func (wrapped llvm::Function) of the callee function
     * @param args (optional) Function call arguments
     * @param name (optional) Name of the returned value
     * @return Value * - Wrapped llvm::Value returned from call
     */
    Local &callFunction(Func &wrap, ValueVec args = {}, std::string name = "");
    /**
     * @brief Call a llvm::Function by its name.
     * 
     * @param callee Name of the callee function
     * @param args (optional) Function call arguments
     * @param name (optional) Name of the returned value
     * @return Value * - Wrapped llvm::Value returned from call
     */
    Local &callFunction(std::string callee, ValueVec args = {}, std::string name = "");

    /// LOCALS ///

    /**
     * @brief Declare (Allocate) a local variable. Automatically adds llvm::AllocaInst to the parent (Eisdrache::Func)
     * 
     * @param type Type to allocate
     * @param name (optional) Name of the AllocaInst *
     * @param value (optional) Future value to be assigned to local variable
     * @return Local & - Wrapped alloca instruction
     */
    Local &declareLocal(Ty type, std::string name = "", Value *value = nullptr);

    /**
     * @brief Load the value of a local variable.
     * 
     * @param local Wrapped llvm::Value
     * @param name (optional) Name of the loaded value.
     * @return Local & - Wrapped llvm::Value
     */
    Local &loadLocal(Local &local, std::string name = "");

    /// STRUCT TYPES ///

    /**
     * @brief Declare a struct type.
     * 
     * @param name Name of the struct type
     * @param elements Types of the elements of the struct type
     * @return Struct & - Wrapped llvm::StructType
     */
    Struct &declareStruct(std::string name, Ty::Vec elements);

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

    /**
     * @brief Get the pointer to element at an index
     * 
     * @param parent Parent of the element
     * @param index Index of the element
     * @param name Name of the returned value
     * @return Local & - Wrapped llvm::Value
     */
    Local &getElementPtr(Local &parent, size_t index, std::string name = "");

    /**
     * @brief Get the value of an element at an index
     * 
     * @param parent Parent of the element
     * @param index Index of the element
     * @param name Name of the returned value
     * @return Local & - Wrapped llvm::Value 
     */
    Local &getElementVal(Local &parent, size_t index, std::string name = "");

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
    ReturnInst *createRet(Local &value, BasicBlock *next = nullptr);
    /**
     * @brief Create a return instruction with a constant.
     * 
     * @param value Value to return
     * @param next (optional) Next insertion point
     * @return ReturnInst * - Return Instruction returned from llvm::IRBuilder
     */
    ReturnInst *createRet(Constant *value, BasicBlock *next = nullptr);

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