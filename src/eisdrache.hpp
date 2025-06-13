/**
 * @file eisdrache.hpp
 * @brief Eisdrache class header
 * @version 0.3.2
 * 
 * @copyright Copyright (c) 2023-2025, Ari.
 * 
 */

#pragma once

#include <iostream>
#include <string>
#include <vector>
#include <map>

#include <llvm/PassRegistry.h>
#include <llvm/InitializePasses.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/Instructions.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>


namespace llvm {

/**
 * @brief Eisdrache Wrapper for the LLVM API.
 * 
 * This wrapper offers a simplified version of the llvm::IRBuilder to avoid
 * unnecessary code.
 */
class Eisdrache : public std::enable_shared_from_this<Eisdrache> {
public:
    using Ptr = std::shared_ptr<Eisdrache>;
    using ValueVec = std::vector<Value *>;
    using TypeVec = std::vector<Type *>;

    // Binary and Unary Operations
    enum Op {
        ADD,    // addition             +
        SUB,    // subtraction          -
        MUL,    // multiplication       *
        DIV,    // division             /
        MOD,    // modulus / remainder  %
        OR,     // bit or               |
        XOR,    // bit xor              ^
        AND,    // bit and              &
        LSH,    // left bit shift       <<
        RSH,    // right bit shift      >>

        EQU,    // equals               ==
        NEQ,    // not equals           !=
        LES,    // less than            <
        LTE,    // less than equals     <=
        GRE,    // greater than         >
        GTE,    // greater than equals  >=

        NEG,    // negate               -
        NOT,    // bit not              ~
    };

    /**
     * @brief Parent class for references, locals, functions, ...
     * 
     */
    class Entity {
    public:
        using Ptr = std::shared_ptr<Entity>;
        using Vec = std::vector<Ptr>;

        explicit Entity(Eisdrache::Ptr eisdrache = nullptr);
        virtual ~Entity();

        enum Kind {
            CONDITION,
            REFERENCE,
            LOCAL,
            FUNC,
            ALIAS,
            VOID,
            PTR,
            INT,
            FLOAT,
            STRUCT,
            NONE,
        };

        [[nodiscard]] virtual Kind kind() const = 0;

    protected:
        Eisdrache::Ptr eisdrache;
    };

    /**
     * @brief Custom Type Parent Class;
     */
    class Ty : public Entity, public std::enable_shared_from_this<Ty> {
    public:
        using Ptr = std::shared_ptr<Ty>;
        using Vec = std::vector<Ptr>;
        using OldMap = std::map<std::string, Ptr>;
        using Map = std::vector<std::pair<std::string, Ptr>>;

        explicit Ty(Eisdrache::Ptr eisdrache = nullptr);

        static Ptr create(const Eisdrache::Ptr &eisdrache, const Type *llvmTy);

        Ptr getPtrTo();
        virtual size_t getBit() const;

        // get the equivalent llvm::Type 
        virtual Type *getTy() const = 0;

        virtual bool isValidRHS(Ptr comp) const = 0;
        virtual bool isEqual(Ptr comp) const = 0;

        constexpr bool isPtrTy() const;
        constexpr bool isIntTy() const;
        constexpr bool isFloatTy() const;
        constexpr bool isSignedTy();

        Kind kind() const override = 0;
    };

    /**
     * @brief An alias for a type.
     * 
     */
    class AliasTy : public Ty {
    public:
        using Ptr = std::shared_ptr<AliasTy>;
        using Vec = std::vector<AliasTy>;

        AliasTy(Eisdrache::Ptr eisdrache, std::string alias, Ty::Ptr type);
        ~AliasTy() override;

        size_t getBit() const override;

        Type *getTy() const override;

        bool isValidRHS(Ty::Ptr comp) const override;
        bool isEqual(Ty::Ptr comp) const override;

        Kind kind() const override;

    private:
        std::string alias;
        Ty::Ptr type;
    };

    /**
     * @brief Type representing void.
     * 
     */
    class VoidTy : public Ty {
    public:
        using Ptr = std::shared_ptr<VoidTy>;
        using Vec = std::vector<Ptr>;

        explicit VoidTy(Eisdrache::Ptr eisdrache = nullptr);

        Type *getTy() const override;

        bool isValidRHS(Ty::Ptr comp) const override;
        bool isEqual(Ty::Ptr comp) const override;

        Kind kind() const override;
    };

    /**
     * @brief Type representing a pointer to an address.
     * 
     */
    class PtrTy : public Ty {
    public:
        using Ptr = std::shared_ptr<PtrTy>;
        using Vec = std::vector<Ptr>;

        PtrTy(Eisdrache::Ptr eisdrache, Ty::Ptr pointee);

        Ty::Ptr &getPointeeTy();
        
        size_t getBit() const override;

        Type *getTy() const override;

        bool isValidRHS(Ty::Ptr comp) const override;
        bool isEqual(Ty::Ptr comp) const override;

        Kind kind() const override;

    private:
        Ty::Ptr pointee;
    };

    class IntTy : public Ty {
    public:
        using Ptr = std::shared_ptr<IntTy>;
        using Vec = std::vector<Ptr>;

        IntTy(Eisdrache::Ptr eisdrache, size_t bit, bool _signed = false);
        
        size_t getBit() const override;

        const bool &getSigned() const;
        Ty::Ptr getSignedTy() const;

        Type *getTy() const override;

        bool isValidRHS(Ty::Ptr comp) const override;
        bool isEqual(Ty::Ptr comp) const override;

        Kind kind() const override;

    private:
        size_t bit;
        bool _signed;
    };

    class FloatTy : public Ty {
    public:
        using Ptr = std::shared_ptr<FloatTy>;
        using Vec = std::vector<Ptr>;

        FloatTy(Eisdrache::Ptr eisdrache, size_t bit);

        size_t getBit() const override;

        Type *getTy() const override;

        bool isValidRHS(Ty::Ptr comp) const override;
        bool isEqual(Ty::Ptr comp) const override;

        Kind kind() const override;
    
    private:
        size_t bit;
    };

    /**
     * @brief A reference to a symbol (local, function, ...).
     * 
     */
    class Reference : public Entity {
    public:
        using Vec = std::vector<Reference>;

        explicit Reference(Eisdrache::Ptr eisdrache = nullptr, std::string symbol = "");
        ~Reference() override;

        Reference &operator=(const Reference &copy);

        [[nodiscard]] const std::string &getSymbol() const;

        // get the entity referred to
        [[nodiscard]] Entity &getEntity() const;

        [[nodiscard]] Kind kind() const override;

    private:
        std::string symbol;
    };

    /**
     * @brief Wrapper for llvm::Value | llvm::AllocaInst;
     * 
     * This class contains 
     * * the value itself,
     * * whether the value is a llvm::AllocaInst,
     * * and the value to be assigned once the value is referenced. 
     * (Relevant for llvm::AllocaInst)
     */
    class Local : public Entity, public std::enable_shared_from_this<Local> {
    public:
        using Ptr = std::shared_ptr<Local>;
        using Vec = std::vector<Ptr>;
        using Map = std::map<std::string, Ptr>;

        Local(Eisdrache::Ptr eisdrache, Constant *constant);
        explicit Local(Eisdrache::Ptr eisdrache = nullptr, Ty::Ptr type = nullptr, Value *ptr = nullptr, Value *future = nullptr, ValueVec future_args = ValueVec());
        
        Local &operator=(const Local &copy);
        bool operator==(const Local &comp) const;
        bool operator==(const Value *comp) const;
        AllocaInst *operator*();

        void setPtr(Value *ptr);
        void setFuture(Value *future);
        void setFutureArgs(ValueVec args);
        void setTy(Ty::Ptr ty);

        AllocaInst *getAllocaPtr();
        Value *getValuePtr();
        Ty::Ptr getTy();
        [[nodiscard]] std::string getName() const;

        [[nodiscard]] bool isAlloca() const;
        // Using this function should be avoided if the loaded value is required in the same block, as it creates unnecessary instructions
        bool isValidRHS(Local &rhs);

        /**
         * @brief Load the value stored at the address of the local.
         *
         * Do <i>NOT</i> use this if you intend to implement dereferencing in a compiler.\n
         * Use Local::dereference() instead.
         * 
         * @param force Load value even if Local is not an alloca instruction
         * @param name Name of the loaded value
         * @return Local::Ptr
         */
         Ptr loadValue(bool force = false, const std::string &name = "");

        /**
         * @brief Dereference the stored value of the local.
         *
         * This function forcibly loads the value of the local and marks it as manually dereferenced.
         * If `dereferenced` is true, the loaded value will still be technically treated as an allocation instruction,
         * and thus doesn't have to be forcibly loaded. Usually, other functions in the Eisdrache wrapper
         * (e.g., binaryOp()) check if locals were created as an allocation instruction
         * and automatically dereference them for convenience to use the stored value in operations.
         *
         * This function is useful when generating code involving pointers;
         * the compiler otherwise would have to differentiate dereferences and e.g., only dereference
         * once for assignments, as the store instruction still requires a pointer, while other instructions
         * require the immediate value stored at the pointer and thus would have to be dereferenced twice.
         * <code>
         * int *x;
         * *x = *x + 1;
         * </code>
         * <br>
         *
         * The C++ code above looks roughly like this in LLVM IR:
         * (without intrinsic types and initialization for readability)
         * <code>
         * %x = alloca i32**                        ; declaration; note that it declares i32**, while the original C++ code declared i32* (int*)
         * %x_load = load i32*, i32** %x            ; first load
         * %x_load_load = load i32, i32* %x_load    ; second load; the actual value stored at the address x
         * %add = add i32 1, i32 %x_load_load       ; addition
         * store i32 %add, i32* %x_load             ; store; the result is assigned to x
         *                 ^^^^^^^^^^^^ the store instruction requires the pointer to the value stored at x,
         *                              while the addition required the actual value
         * </code>
         *
         * @param name Name of the dereferenced value
         * @return Local::Ptr
         */
        Ptr dereference(const std::string &name = "");

        /**
         * @brief This function should be called automatically when trying to access `v_ptr` or `a_ptr`.
         *      However, the user can call this function themselves if required.
         *      This function also checks whether `future` is a nullptr
         *          and sets `future` to nullptr once it was invoked. 
         *      (`future_args` too)
         */
        void invokeFuture();

        [[nodiscard]] Kind kind() const override;

    private:
        bool dereferenced; // whether the user manually dereferenced the local
        union {
            Value *v_ptr;
            AllocaInst *a_ptr;
        };
        Ty::Ptr type;
        Value *future;
        ValueVec future_args;
    };

    /**
     * @brief Condition for branching.
     * 
     */
    class Condition : public Entity {
    public:
        using Vec = std::vector<Condition>;
    
        Condition(Eisdrache::Ptr eisdrache, Op operation, Local::Ptr lhs, Local::Ptr rhs);
        ~Condition() override;

        Local::Ptr create();
    
        [[nodiscard]] Kind kind() const override;

        CmpInst::Predicate getPredicate() const;
    private:
        Op operation;  
        Local::Ptr lhs, rhs;
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
    class Func : public Entity {
    public:
        using Ptr = std::shared_ptr<Func>;
        using Vec = std::vector<Ptr>;
        using Map = std::map<std::string, Ptr>;

        Func();
        Func(Eisdrache::Ptr eisdrache, Ty::Ptr type, const std::string &name, const Ty::Map &parameters, bool entry = false);
        ~Func() override;

        Func &operator=(const Func &copy);
        bool operator==(const Func &comp) const;
        bool operator==(const Function *comp) const;
        // update reference local by symbol
        Local::Ptr operator[](const std::string &symbol);
        // get the wrapped llvm::Function
        Function *operator*() const;

        // get argument at index
        Local::Ptr arg(size_t index);
        // call this function
        Local::Ptr call(const ValueVec &args = {}, const std::string &name = "") const;
        Local::Ptr call(const Local::Vec &args = {}, const std::string &name = "") const;
        // add a local variable to this function
        // and return reference to copy of local
        Local::Ptr addLocal(const Local::Ptr &local);

        // add an attribute to the function or a parameter
        void addAttr(Attribute attr, int64_t index = -1) const;
        void addAttr(Attribute::AttrKind attr, int64_t index = -1) const;

        // set the calling convention of the function
        void setCallingConv(CallingConv::ID conv) const;

        // toggle no exception 
        void setDoesNotThrow() const;

        Ty::Ptr getTy();
        std::string getName() const;

        [[nodiscard]] Kind kind() const override;

    private:
        Function *func;
        Ty::Ptr type;
        Local::Vec parameters;
        Local::Map locals;
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
    class Struct : public Ty {
    public:
        using Ptr = std::shared_ptr<Struct>;
        using Vec = std::vector<Ptr>;
        using Map = std::map<std::string, Ptr>;

        Struct();
        Struct(Eisdrache::Ptr eisdrache, const std::string &name, const Ty::Vec &elements);
        ~Struct() override;

        Struct &operator=(const Struct &copy);
        bool operator==(const Struct &comp) const;
        bool operator==(const Type *comp) const;
        // get the type of element at an index
        Ty::Ptr operator[](size_t index);
        // get the wrapped llvm::StructType
        StructType *operator*() const;
        
        // allocate object of this type
        Local::Ptr allocate(const std::string &name = "");

        /**
         * @brief Create a member function of this struct.
         *      Automatically adds `ptr this` as the first parameter.
         * 
         * @param type Type of returned value
         * @param name Function name
         * @param args Additional parameters
         * @return Func::Ptr
         */
        Func::Ptr createMemberFunc(const Ty::Ptr &type, const std::string &name, const Ty::Map &args = Ty::Map());

        Type *getTy() const override;

        bool isValidRHS(Ty::Ptr comp) const override;
        bool isEqual(Ty::Ptr comp) const override;

        Kind kind() const override;

    private:
        std::string name;
        StructType *type;
        Ty::Vec elements;
    };

    class Array {
    public:
        enum Member {
            GET_BUFFER,
            SET_BUFFER,
            GET_SIZE,
            SET_SIZE,
            GET_MAX,
            SET_MAX,
            GET_FACTOR,
            SET_FACTOR,
            CONSTRUCTOR,
            CONSTRUCTOR_SIZE,
            CONSTRUCTOR_COPY,
            DESTRUCTOR,
            RESIZE,
            IS_VALID_INDEX,
            GET_AT_INDEX,
            SET_AT_INDEX,
        };

        explicit Array(Ptr eisdrache = nullptr, Ty::Ptr elementTy = nullptr, const std::string &name = "");
        ~Array();

        [[nodiscard]] Local::Ptr allocate(const std::string &name = "") const;
        [[nodiscard]] Local::Ptr call(Member callee, const ValueVec &args = {}, const std::string &name = "") const;
        [[nodiscard]] Local::Ptr call(Member callee, const Local::Vec &args = {}, const std::string &name = "") const;

    private:
        std::string name;
        Struct::Ptr self;
        Ty::Ptr elementTy;
        Ty::Ptr bufferTy;
        
        Func::Ptr get_buffer = nullptr;
        Func::Ptr set_buffer = nullptr;
        Func::Ptr get_size = nullptr;
        Func::Ptr set_size = nullptr;
        Func::Ptr get_max = nullptr;
        Func::Ptr set_max = nullptr;
        Func::Ptr get_factor = nullptr;
        Func::Ptr set_factor = nullptr;
        Func::Ptr constructor = nullptr;
        Func::Ptr constructor_size = nullptr;
        Func::Ptr constructor_copy = nullptr;
        Func::Ptr destructor = nullptr;
        Func::Ptr resize = nullptr;
        Func::Ptr is_valid_index = nullptr;
        Func::Ptr get_at_index = nullptr;
        Func::Ptr set_at_index = nullptr;

        Ptr eisdrache;
    };

    ~Eisdrache();

    // Initialize the LLVM API
    static void initialize();

    static Ptr create(const std::string &moduleID, const std::string &targetTriple = "");

    // dump the generated LLVM IR
    void dump(raw_fd_ostream &os = errs()) const;
    // automatically create output stream
    void dump(const std::string &filePath) const;

    /// TYPES //

    // Type: void
    Ty::Ptr getVoidTy();
    // Type: i1
    Ty::Ptr getBoolTy();
    // Type: i64 (size_t)
    Ty::Ptr getSizeTy();
    // Type: bit
    Ty::Ptr getSignedTy(size_t bit);
    // Type: bit*
    Ty::Ptr getSignedPtrTy(size_t bit);
    // Type: bit**
    Ty::Ptr getSignedPtrPtrTy(size_t bit);
    // Type: bit
    Ty::Ptr getUnsignedTy(size_t bit);
    // Type: bit*
    Ty::Ptr getUnsignedPtrTy(size_t bit);
    // Type: bit**
    Ty::Ptr getUnsignedPtrPtrTy(size_t bit);
    // Type: 16 = half, 32 = float, 64 = double
    Ty::Ptr getFloatTy(size_t bit);
    // Type: 16 = half*, 32 = float*, 64 = double*
    Ty::Ptr getFloatPtrTy(size_t bit);
    // Type: 16 = half**, 32 = float**, 64 = double**
    Ty::Ptr getFloatPtrPtrTy(size_t bit);

    /// VALUES ///

    ConstantInt *getBool(bool value) const;

    ConstantInt *getInt(size_t bit, uint64_t value) const;
    
    Value *getNegative(ConstantInt *value) const;
    
    ConstantFP *getFloat(double value) const;
    
    Constant *getLiteral(const std::string &value, const std::string &name = "") const;

    // returns `nullLocal`, an empty local representing null
    Local::Ptr getNull();
    
    static ConstantPointerNull *getNullPtr(const Ty::Ptr &ptrTy) ;

    /// FUNCTIONS ///
    
    /**
     * @brief Declare a llvm::Function without parameters' names.
     * 
     * @param type return-type of the function
     * @param name name of the function
     * @param parameters parameters of the function 
     * @return Func & - Eisdrache::Func (wrapped llvm::Function)
     */
    Func::Ptr declareFunction(const Ty::Ptr &type, const std::string &name, const Ty::Vec &parameters);
    /**
     * @brief Declare a llvm::Function.
     * 
     * @param type return-type of the function
     * @param name name of the function
     * @param parameters (optional) parameters of the function 
     * @param entry (optional) creates entry llvm::BasicBlock in the function body if true
     * @return Func & - Eisdrache::Func (wrapped llvm::Function)
     */
    Func::Ptr declareFunction(const Ty::Ptr &type, const std::string &name, const Ty::Map &parameters = Ty::Map(), bool entry = false);
    
    /**
     * @brief Get the Eisdrache::Func wrapper object
     * 
     * @param function Pointer to llvm::Function
     * @return Func & - Eisdrache::Func (wrapped llvm::Function)
     */
    Func::Ptr getWrap(const Function *function);

    /**
     * @brief Verify that a Eisdrache::Func is free of errors.
     *          TODO: Implement this function.
     * @param wrap Eisdrache::Func (wrapped llvm::Function)
     * @return true - Eisdrache::Func is error-free.
     * @return false - Eisdrache::Func contains errors.
     */
    static bool verifyFunc(const Func::Ptr &wrap);

    /**
     * @brief Call a llvm::Function by its wrap.
     * 
     * @param wrap Eisdrache::Func (wrapped llvm::Function) of the callee function
     * @param args (optional) Function call arguments
     * @param name (optional) Name of the returned value
     * @return Local::Ptr - Wrapped llvm::Value returned from call
     */
    static Local::Ptr callFunction(const Func::Ptr &wrap, const ValueVec &args = {}, const std::string &name = "");

    /**
     * @brief Call a llvm::Function by its wrap.
     * 
     * @param wrap Eisdrache::Func (wrapped llvm::Function) of the callee function
     * @param args (optional) Function call arguments
     * @param name (optional) Name of the returned value
     * @return Local::Ptr - Wrapped llvm::Value returned from call
     */
    static Local::Ptr callFunction(const Func::Ptr &wrap, const Local::Vec &args = {}, const std::string &name = "");

    /**
     * @brief Call a llvm::Function by its name.
     * 
     * @param callee Name of the callee function
     * @param args (optional) Function call arguments
     * @param name (optional) Name of the returned value
     * @return Local::Ptr - Wrapped llvm::Value returned from call
     */
    Local::Ptr callFunction(const std::string &callee, const ValueVec &args = {}, const std::string &name = "") const;

    /**
     * @brief Call a llvm::Function by its name.
     * 
     * @param callee Name of the callee function
     * @param args (optional) Function call arguments
     * @param name (optional) Name of the returned value
     * @return Local::Ptr - Wrapped llvm::Value returned from call
     */
    Local::Ptr callFunction(const std::string &callee, const Local::Vec &args = {}, const std::string &name = "") const;

    /// LOCALS ///

    /**
     * @brief Declare (Allocate) a local variable. Automatically adds llvm::AllocaInst to the parent (Eisdrache::Func)
     * 
     * @param type Type to allocate
     * @param name (optional) Name of the AllocaInst *
     * @param future (optional) Future value to be assigned to the local variable
     * @param future_args (optional) Arguments if future value is a function
     * @return Local::Ptr - Wrapped alloca instruction
     */
    Local::Ptr declareLocal(const Ty::Ptr &type, const std::string &name = "", Value *future = nullptr, const ValueVec &future_args = ValueVec());

    /**
     * @brief Load the value of a local variable.
     * 
     * @param local Wrapped llvm::Value
     * @param name (optional) Name of the loaded value.
     * @return Local::Ptr - Wrapped llvm::Value
     */
    static Local::Ptr loadLocal(const Local::Ptr &local, const std::string &name = "");

    /**
     * @brief Store a value in a local variable.
     * 
     * @param local Local to be stored at (must be a pointer)
     * @param value Value to store in local
     * @return StoreInst * - Store instruction returned by llvm::IRBuilder
     */
    StoreInst *storeValue(const Local::Ptr &local, const Local::Ptr &value) const;
    /**
     * @brief Store a value in a local variable.
     * 
     * @param local Local to be stored at (must be a pointer)
     * @param value Value to store in local
     * @return StoreInst * - Store instruction returned by llvm::IRBuilder
     */
    StoreInst *storeValue(const Local::Ptr &local, Constant *value) const;

    /**
     * @brief Create an instruction for the future assignment of a local
     * 
     * @param local The local
     * @param value Value to be assigned
     */
    static void createFuture(Local &local, Value *value);
    /**
     * @brief Create an instruction for the future assignment of a local
     * 
     * @param local The local
     * @param func Function to be called on local
     * @param args Arguments for the function call
     */
    static void createFuture(Local &local, const Func &func, const ValueVec &args);


    /// STRUCT TYPES ///

    /**
     * @brief Declare a struct type.
     * 
     * @param name Name of the struct type
     * @param elements Types of the elements of the struct type
     * @return Struct::Ptr & - Wrapped llvm::StructType
     */
    Struct::Ptr &declareStruct(const std::string &name, const Ty::Vec &elements);

    /**
     * @brief Allocate the object of the struct type.
     *      Automatically appends to Eisdrache::Func::locals.
     * 
     * @param wrap Wrapped llvm::StructType
     * @param name Name of the returned pointer
     * @return Local::Ptr - Wrapped alloca instruction
     */
    Local::Ptr allocateStruct(const Struct::Ptr &wrap, const std::string &name = "");
    /**
     * @brief Allocate the object of the struct type.
     *      Automatically appends to Eisdrache::Func::locals.
     * 
     * @param typeName Name of the struct type.
     * @param name Name of the returned pointer
     * @return Local::Ptr - Wrapped alloca instruction
     */
    Local::Ptr allocateStruct(const std::string &typeName, const std::string &name = "");

    /**
     * @brief Get the pointer to the element at an index
     * 
     * @param parent Parent of the element
     * @param index Index of the element
     * @param name Name of the returned value
     * @return Local::Ptr - Wrapped llvm::Value
     */
    Local::Ptr getElementPtr(const Local::Ptr &parent, size_t index, const std::string &name = "");

    /**
     * @brief Get the value of an element at an index
     * 
     * @param parent Parent of the element
     * @param index Index of the element
     * @param name Name of the returned value
     * @return Local::Ptr - Wrapped llvm::Value
     */
    Local::Ptr getElementVal(const Local::Ptr &parent, size_t index, const std::string &name = "");

    /// BUILDER ///

    /**
     * @brief Create a void return instruction.
     * 
     * @param next (optional) Next insertion point
     * @return ReturnInst * - Return Instruction returned from llvm::IRBuilder
     */
    ReturnInst *createRet(BasicBlock *next = nullptr) const;
    /**
     * @brief Create a return instruction with a value.
     * 
     * @param value Value to return
     * @param next (optional) Next insertion point
     * @return ReturnInst * - Return Instruction returned from llvm::IRBuilder
     */
    ReturnInst *createRet(const Local::Ptr &value, BasicBlock *next = nullptr) const;
    /**
     * @brief Create a return instruction with a constant.
     * 
     * @param value Value to return
     * @param next (optional) Next insertion point
     * @return ReturnInst * - Return Instruction returned from llvm::IRBuilder
     */
    ReturnInst *createRet(Constant *value, BasicBlock *next = nullptr) const;

    /**
     * @brief Create a block and set the insert point to it.
     * 
     * @param name (optional) Name of the block
     * @param insert (optional) Start insertion at this block
     * @return BasicBlock * 
     */
    BasicBlock *createBlock(const std::string &name = "", bool insert = false) const;

    /**
     * @brief Set the current insertion block.
     * 
     * @param block The insertion block
     */
    void setBlock(BasicBlock *block) const;

    /**
     * @brief Create a binary operation.
     * 
     * @param op Operation
     * @param LHS Left-Hand-Side
     * @param RHS Right-Hand-Side
     * @param name (optional) Name of the result
     * @return Local::Ptr - Result
     */
    Local::Ptr binaryOp(Op op, const Local::Ptr &LHS, const Local::Ptr &RHS, std::string name = "");

    /**
     * @brief Bitcast a pointer to a type.
     * 
     * @param ptr The original pointer 
     * @param to The destination type
     * @param name (optional) Name of the returned pointer
     * @return Local::Ptr - The returned pointer from the bitcast
     */
    Local::Ptr bitCast(const Local::Ptr &ptr, const Ty::Ptr &to, const std::string &name = "");

    /**
     * @brief Jump to block.
     * 
     * @param block 
     * @return BranchInst *
     */
    BranchInst *jump(BasicBlock *block) const;
    /**
     * @brief Jump to `then` if condition is true, else jump to `else´.
     * 
     * @param condition The condition
     * @param then The `then` block
     * @param else_ (optional) The `else` block
     * @return BranchInst *
     */
    BranchInst *jump(const Local::Ptr &condition, BasicBlock *then, BasicBlock *else_ = nullptr) const;

    /**
     * @brief Type cast a value.
     * 
     * @param local The value
     * @param to The destination type
     * @param name (optional) The name of the cast value
     * @return Local::Ptr
     */
    Local::Ptr typeCast(const Local::Ptr &local, const Ty::Ptr &to, const std::string &name = "typecast");

    /**
     * @brief Get the pointer to an element of an array.
     * 
     * @param array The array
     * @param index The index of the element
     * @param name (optional) The name of the returned pointer
     * @return Local::Ptr
     */
    Local::Ptr getArrayElement(const Local::Ptr &array, size_t index, const std::string &name = "");

    /**
     * @brief Get the pointer to an element of an array.
     * 
     * @param array The array
     * @param index The index of the element
     * @param name (optional) The name of the returned pointer
     * @return Local::Ptr
     */
    Local::Ptr getArrayElement(const Local::Ptr &array, const Local::Ptr &index, const std::string &name = "");

    /**
     * @brief Check whether the given pointer is a nullptr and return the result.
     * 
     * @param pointer The pointer 
     * @param name Name of the result
     * @return Local::Ptr,
     *          true: pointer == nullptr,
     *          false: pointer != nullptr
     */
    Local::Ptr compareToNull(const Local::Ptr &pointer, const std::string &name = "");

    /**
     * @brief Create a unary operation.
     * 
     * @param op The unary operation
     * @param expr The expression
     * @param name Name of the result
     * @return Local::Ptr
     */
    Local::Ptr unaryOp(Op op, const Local::Ptr &expr, const std::string &name = "");

    /**
     * @brief Create jump instructions and if/else blocks. Sets IRBuilder to block 'then'.
     * 
     * @param condition Condition that determines which block is jumped to
     * @param then_name Name of Block that is jumped to if 'condition' is true
     * @param else_name Name of Block that is jumped to if 'condition' is false
     * @return BasicBlock * - Returns else_ block
     */
    BasicBlock *ifStatement(Condition condition, const std::string &then_name = "then_block", const std::string &else_name = "else_block") const;

    /// GETTER ///

    /**
     * @brief Get the llvm::LLVMContext
     * 
     * @return LLVMContext * 
     */
    LLVMContext *getContext() const;
    
    /**
     * @brief Get the llvm::Module
     * 
     * @return Module * 
     */
    Module *getModule() const;
    
    /**
     * @brief Get the llvm::IRBuilder
     * 
     * @return IRBuilder<> * 
     */
    IRBuilder<> *getBuilder() const;
    
    /**
     * @brief Get the current wrapped parent llvm::Function
     * 
     * @return Func::Ptr
     */
    Func::Ptr getCurrentParent() const;
    
    /**
     * @brief Get the vector of Eisdrache::Tys in this context
     * 
     * @return Ty::Vec & 
     */
    Ty::Vec &getTypes();
    
    /**
     * @brief Add a new Eisdrache::Ty to this context.
     *      If the type exists, it will return the pointer to it.
     *      If not, it will add the given type to the context.
     * 
     * @param ty Pointer to the Ty
     * @return Ty::Ptr - Pointer to the found type.
     */
    Ty::Ptr addTy(const Ty::Ptr &ty);

    /**
     * @brief Get the pointer to a function by its name.
     * 
     * @param name Name of the function
     * @return Func::Ptr - Pointer to the found function.
     */
    Func::Ptr getFunc(const std::string &name);

    /**
     * @brief Set the current parent
     * 
     * @param func Pointer to the new parent
     */
    void setParent(Func::Ptr func);

private:
    Eisdrache(LLVMContext *context, Module *module, IRBuilder<> *builder, const std::string &targetTriple);

    static std::nullptr_t complain(const std::string&);

    LLVMContext *context;
    Module *module;
    IRBuilder<> *builder;

    Local::Ptr nullLocal; // empty local representing null
    Func::Ptr parent; // current parent function

    Func::Map functions;
    Struct::Map structs;
    Ty::Vec types;
};

} // namespace llvm