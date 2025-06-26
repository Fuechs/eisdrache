/**
 * @file wyvern.hpp
 * @brief Wyvern LLVM API Wrapper header
 * @version 0.4
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


// TODO: remove useless functions (all should just accept Entity instead of Local, Val, etc)

namespace wyvern {

class Wrapper;

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

    explicit Entity(std::shared_ptr<Wrapper> wrapper = nullptr);
    virtual ~Entity();

    enum Kind {
        CONDITION,
        REFERENCE,
        VALUE,
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

    virtual constexpr Kind kind() const = 0;

protected:
    std::shared_ptr<Wrapper> wrapper;
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

    explicit Ty(std::shared_ptr<Wrapper> wrapper = nullptr);

    static Ptr create(const std::shared_ptr<Wrapper>  &wrapper, const llvm::Type *llvmTy);

    Ptr getPtrTo();
    virtual size_t getBit() const;

    // get the equivalent llvm::Type
    virtual llvm::Type *getTy() const = 0;

    virtual bool isValidRHS(Ptr comp) const = 0;
    virtual bool isEqual(Ptr comp) const = 0;

    virtual constexpr bool isPtrTy()    const { return false; }
    virtual constexpr bool isIntTy()    const { return false; }
    virtual constexpr bool isFloatTy()  const { return false; }
    virtual constexpr bool isSignedTy() const { return false; }

    constexpr Kind kind() const override = 0;
};

/**
     * @brief An alias for a type.
     *
     */
class AliasTy : public Ty {
public:
    using Ptr = std::shared_ptr<AliasTy>;
    using Vec = std::vector<AliasTy>;

    AliasTy(std::shared_ptr<Wrapper> wrapper, std::string alias, Ty::Ptr type);
    ~AliasTy() override;

    size_t getBit() const override;

    llvm::Type *getTy() const override;

    bool isValidRHS(Ty::Ptr comp) const override;
    bool isEqual(Ty::Ptr comp) const override;

    constexpr bool isPtrTy()    const override { return type->isPtrTy(); }
    constexpr bool isIntTy()    const override { return type->isIntTy(); }
    constexpr bool isFloatTy()  const override { return type->isFloatTy(); }
    constexpr bool isSignedTy() const override { return type->isSignedTy(); }

    constexpr Kind kind() const override { return ALIAS; }

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

    explicit VoidTy(std::shared_ptr<Wrapper> wrapper = nullptr);

    llvm::Type *getTy() const override;

    bool isValidRHS(Ty::Ptr comp) const override;
    bool isEqual(Ty::Ptr comp) const override;

    constexpr Kind kind() const override { return VOID; }
};

/**
 * @brief Type representing a pointer to an address.
 *
 */
class PtrTy : public Ty {
public:
    using Ptr = std::shared_ptr<PtrTy>;
    using Vec = std::vector<Ptr>;

    PtrTy(std::shared_ptr<Wrapper> wrapper, Ty::Ptr pointee);

    Ty::Ptr &getPointeeTy();

    size_t getBit() const override;

    llvm::Type *getTy() const override;

    bool isValidRHS(Ty::Ptr comp) const override;
    bool isEqual(Ty::Ptr comp) const override;

    constexpr bool isPtrTy() const override { return true; }

    constexpr Kind kind() const override { return PTR; }

private:
    Ty::Ptr pointee;
};

class IntTy : public Ty {
public:
    using Ptr = std::shared_ptr<IntTy>;
    using Vec = std::vector<Ptr>;

    IntTy(std::shared_ptr<Wrapper> wrapper, size_t bit, bool _signed = false);

    size_t getBit() const override;

    const bool &getSigned() const;
    Ty::Ptr getSignedTy() const;

    llvm::Type *getTy() const override;

    bool isValidRHS(Ty::Ptr comp) const override;
    bool isEqual(Ty::Ptr comp) const override;

    constexpr bool isIntTy()    const override { return true; }
    constexpr bool isSignedTy() const override { return _signed; }

    constexpr Kind kind() const override { return INT; }

private:
    size_t bit;
    bool _signed;
};

class FloatTy : public Ty {
public:
    using Ptr = std::shared_ptr<FloatTy>;
    using Vec = std::vector<Ptr>;

    FloatTy(std::shared_ptr<Wrapper> wrapper, size_t bit);

    size_t getBit() const override;

    llvm::Type *getTy() const override;

    bool isValidRHS(Ty::Ptr comp) const override;
    bool isEqual(Ty::Ptr comp) const override;

    constexpr bool isFloatTy()  const override { return true; }
    constexpr bool isSignedTy() const override { return true; }

    constexpr Kind kind() const override { return FLOAT; }

private:
    size_t bit;
};

/**
 * @brief An immediate value.
 */
class Val : public Entity {
public:
    using Ptr = std::shared_ptr<Val>;
    using Vec = std::vector<Ptr>;

    explicit Val(std::shared_ptr<Wrapper> wrapper = nullptr, Ty::Ptr type = nullptr, llvm::Value *value = nullptr);
    ~Val() override;

    static Ptr create(std::shared_ptr<Wrapper> wrapper = nullptr, llvm::Value *value = nullptr);
    static Ptr create(std::shared_ptr<Wrapper> wrapper, const Ty::Ptr &type, llvm::Value *value = nullptr);

    constexpr llvm::Value *operator*() const { return value; }

    constexpr llvm::Value *getValuePtr() const { return value; }
    constexpr const Ty::Ptr &getTy() const { return type; }

    constexpr Kind kind() const override { return VALUE; }

private:
    Ty::Ptr type;
    llvm::Value *value;
};

/**
     * @brief A reference to a symbol (local, function, ...).
     *
     */
class Reference : public Entity {
public:
    using Ptr = std::shared_ptr<Reference>;
    using Vec = std::vector<Ptr>;

    explicit Reference(std::shared_ptr<Wrapper> wrapper = nullptr, std::string symbol = "");
    ~Reference() override;

    Reference &operator=(const Reference &copy);

    constexpr const std::string &getSymbol() const { return symbol; }

    // get the entity referred to
    [[nodiscard]] Entity::Ptr getEntity() const;

    constexpr Kind kind() const override { return REFERENCE; }

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

    // TODO: instead of llvm::Value it should take wyvern::Val for future values
    //      maybe needs to take wyvern::Entity to support wyvern::Func as well

    [[deprecated("Use Wrapper::Val instead")]]
    Local(std::shared_ptr<Wrapper> wrapper, llvm::Constant *constant);
    explicit Local(std::shared_ptr<Wrapper> wrapper, const Val::Ptr &value, llvm::Value *future = nullptr, std::vector<llvm::Value *> future_args = {});

    explicit Local(std::shared_ptr<Wrapper> wrapper = nullptr, Ty::Ptr type = nullptr, llvm::Value *ptr = nullptr, llvm::Value *future = nullptr, std::vector<llvm::Value *> future_args = {});

    [[deprecated("Use Wrapper::Val instead")]]
    static Ptr create(std::shared_ptr<Wrapper> wrapper = nullptr, llvm::Constant *constant = nullptr);
    // FIXME: this may habe to be rewritten somehow to house Vals
    static Ptr create(std::shared_ptr<Wrapper> wrapper = nullptr, const Val::Ptr &value = nullptr);
    static Ptr create(std::shared_ptr<Wrapper> wrapper = nullptr, Ty::Ptr type = nullptr, llvm::Value *ptr = nullptr, llvm::Value *future = nullptr, std::vector<llvm::Value *> future_args = {});

    Local &operator=(const Local &copy);
    bool operator==(const Local &comp) const;
    bool operator==(const llvm::Value *comp) const;
    llvm::AllocaInst *operator*();

    void setPtr(llvm::Value *ptr);
    void setFuture(llvm::Value *future);
    void setFutureArgs(std::vector<llvm::Value *> args);
    void setTy(Ty::Ptr ty);

    llvm::AllocaInst *getAllocaPtr();
    llvm::Value *getValuePtr();
    Ty::Ptr getTy();
    [[nodiscard]] std::string getName() const;

    [[nodiscard]] bool isAlloca() const;
    // Using this function should be avoided if the loaded value is required in the same block, as it creates unnecessary instructions
    bool isValidRHS(Local &rhs);

    /**
     * @brief Load the value stored at the address of the local.
     *
     * Do <i>NOT</i> use this if you intend to implement dereferencing in a compiler.\n
     * Use <code>Local::dereference()</code> instead.
     *
     * @param force Load value even if Local is not an alloca instruction
     * @param name Name of the loaded value
     * @return The loaded value
     */
    Ptr loadValue(bool force = false, const std::string &name = "");

    /**
     * @brief Dereference the stored value of the local.
     *
     * This function forcibly loads the value of the local and marks it as manually dereferenced.
     * If `dereferenced` is true, the loaded value will still be technically treated as an allocation instruction,
     * and thus doesn't have to be forcibly loaded. Usually, other functions in the wrapper
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
     * @return The dereferenced local
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

    constexpr Kind kind() const override { return LOCAL; }

private:
    bool dereferenced; // whether the user manually dereferenced the local
    union {
        llvm::Value *v_ptr;
        llvm::AllocaInst *a_ptr;
    };

    Ty::Ptr type;
    llvm::Value *future;
    std::vector<llvm::Value *> future_args;
};


/**
 * @brief Condition for branching.
 *
 */
class Condition : public Entity {
public:
    using Vec = std::vector<Condition>;

    Condition(std::shared_ptr<Wrapper> wrapper, Op operation, Entity::Ptr lhs, Entity::Ptr rhs);
    ~Condition() override;

    Val::Ptr create() const;

    constexpr Kind kind() const override { return CONDITION; }

    llvm::CmpInst::Predicate getPredicate() const;
private:
    Op operation;
    Entity::Ptr lhs, rhs;
};

/**
 * @brief Wrapper for llvm::Function.
 *
 * This class contains all locals, parameters and blocks
 * from the wrapped llvm::Function. Can be initialized by the user,
 * but should be initialized by the Wyvern Wrapper.
 */
class Func : public Entity {
public:
    using Ptr = std::shared_ptr<Func>;
    using Vec = std::vector<Ptr>;
    using Map = std::map<std::string, Ptr>;

    Func();
    Func(std::shared_ptr<Wrapper> wrapper, Ty::Ptr type, const std::string &name, const Ty::Map &parameters, bool entry = false);
    ~Func() override;

    Func &operator=(const Func &copy);
    bool operator==(const Func &comp) const;
    bool operator==(const llvm::Function *comp) const;
    // update reference local by symbol
    Local::Ptr operator[](const std::string &symbol);
    // get the wrapped llvm::Function
    llvm::Function *operator*() const;

    // get argument at index
    Local::Ptr arg(size_t index);
    // call this function
    Val::Ptr call(const Entity::Vec &args = {}, const std::string &name = "") const;
    Val::Ptr call(const std::vector<llvm::Value *> &args = {}, const std::string &name = "") const;
    // add a local variable to this function
    // and return reference to copy of local
    Local::Ptr addLocal(const Local::Ptr &local);

    // add an attribute to the function or a parameter
    void addAttr(llvm::Attribute attr, int64_t index = -1) const;
    void addAttr(llvm::Attribute::AttrKind attr, int64_t index = -1) const;

    // set the calling convention of the function
    void setCallingConv(llvm::CallingConv::ID conv) const;

    // toggle no exception
    void setDoesNotThrow() const;

    // returns whether the function body is empty
    constexpr bool empty() const { return func->empty(); }

    Ty::Ptr getTy();
    std::string getName() const;
    Local::Ptr getLocal(const std::string &symbol);
    constexpr const Local::Vec &getArgs() const { return parameters; }

    // print LLVM IR of function
    constexpr void print() const { func->print(llvm::errs()); }

    constexpr Kind kind() const override { return FUNC; }

private:
    llvm::Function *func;
    Ty::Ptr type;
    Local::Vec parameters;
    Local::Map locals;
};

/**
 * @brief Wrapper for llvm::StructType
 *
 * This class contains the struct type itself and all types of its elements.
 * Can be initialized by the user, but should be initialized by the Wyvern Wrapper.
 */
class Struct : public Ty {
public:
    using Ptr = std::shared_ptr<Struct>;
    using Vec = std::vector<Ptr>;
    using Map = std::map<std::string, Ptr>;

    Struct();
    Struct(std::shared_ptr<Wrapper> wrapper, const std::string &name, const Ty::Vec &elements);
    ~Struct() override;

    Struct &operator=(const Struct &copy);
    bool operator==(const Struct &comp) const;
    bool operator==(const llvm::Type *comp) const;
    // get the type of element at an index
    Ty::Ptr operator[](size_t index);
    // get the wrapped llvm::StructType
    llvm::StructType *operator*() const;

    // allocate object of this type
    Local::Ptr allocate(const std::string &name = "");

    /**
     * @brief Create a member function of this struct.
     *      Automatically adds `ptr this` as the first parameter.
     *
     * @param type Type of returned value
     * @param name Function name
     * @param args Additional parameters
     * @return Member function
     */
    Func::Ptr createMemberFunc(const Ty::Ptr &type, const std::string &name, const Ty::Map &args = Ty::Map());

    llvm::Type *getTy() const override;

    bool isValidRHS(Ty::Ptr comp) const override;
    bool isEqual(Ty::Ptr comp) const override;

    constexpr Kind kind() const override { return STRUCT; }

private:
    std::string name;
    llvm::StructType *type;
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

    explicit Array(std::shared_ptr<Wrapper> wrapper = nullptr, Ty::Ptr elementTy = nullptr, const std::string &name = "");
    ~Array();

    [[nodiscard]] Local::Ptr allocate(const std::string &name = "") const;
    [[nodiscard]] Val::Ptr call(Member callee, const std::vector<llvm::Value *> &args = {}, const std::string &name = "") const;
    [[nodiscard]] Val::Ptr call(Member callee, const Local::Vec &args = {}, const std::string &name = "") const;

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

    std::shared_ptr<Wrapper> wrapper;
};

/**
 * @brief Wyvern Wrapper for the LLVM API.
 * 
 * This wrapper offers a simplified version of the llvm::IRBuilder to avoid
 * unnecessary code.
 */
class Wrapper : public std::enable_shared_from_this<Wrapper> {
public:
    using Ptr = std::shared_ptr<Wrapper>;
    using ValueVec = std::vector<llvm::Value *>;
    using TypeVec = std::vector<llvm::Type *>;

    ~Wrapper();

    // Initialize the LLVM API
    static void initialize();

    static Ptr create(const std::string &moduleID, const std::string &targetTriple = "");

    // dump the generated LLVM IR
    void dump(llvm::raw_fd_ostream &os = llvm::errs()) const;
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

    Val::Ptr getBool(bool value);

    Val::Ptr getInt(size_t bit, uint64_t value);

    Val::Ptr getFloat(double value);
    
    Val::Ptr getLiteral(const std::string &value, const std::string &name = "");

    Val::Ptr getNull();
    
    Val::Ptr getNullPtr(const Ty::Ptr &ptrTy);

    /// FUNCTIONS ///
    
    /**
     * @brief Declare a llvm::Function without parameters' names.
     * 
     * @param type return-type of the function
     * @param name name of the function
     * @param parameters parameters of the function 
     * @return wrapped llvm::Function
     */
    Func::Ptr declareFunction(const Ty::Ptr &type, const std::string &name, const Ty::Vec &parameters);
    /**
     * @brief Declare a llvm::Function.
     * 
     * @param type return-type of the function
     * @param name name of the function
     * @param parameters (optional) parameters of the function 
     * @param entry (optional) creates entry llvm::BasicBlock in the function body if true
     * @return wrapped llvm::Function
     */
    Func::Ptr declareFunction(const Ty::Ptr &type, const std::string &name, const Ty::Map &parameters = Ty::Map(), bool entry = false);
    
    /**
     * @brief Get the Func wrapper object
     * 
     * @param function Pointer to llvm::Function
     * @return wrapped llvm::Function
     */
    Func::Ptr getWrap(const llvm::Function *function);

    /**
     * @brief Verify that a Func is free of errors.
     *          TODO: Implement this function.
     * @param wrap Func (wrapped llvm::Function)
     * @return true - Func is error-free.
     * @return false - Func contains errors.
     */
    static bool verifyFunc(const Func::Ptr &wrap);

    /**
     * @brief Call a llvm::Function by its name.
     * 
     * @param callee Name of the callee function
     * @param args (optional) Function call arguments
     * @param name (optional) Name of the returned value
     * @return Wrapped llvm::Value returned from call
     */
    Val::Ptr callFunction(const std::string &callee, const ValueVec &args = {}, const std::string &name = "") const;

    /**
     * @brief Call a llvm::Function by its name.
     *
     * @param callee Name of the callee function
     * @param args (optional) Function call arguments
     * @param name (optional) Name of the returned value
     * @return Wrapped llvm::Value returned from call
     */
    Val::Ptr callFunction(const std::string &callee, const Entity::Vec &args = {}, const std::string &name = "") const;

    /**
     * @brief Erase the function completely.
     *
     * @param wrap Wrapper of the function
     */
    void eraseFunction(const Func::Ptr &wrap);

    /// LOCALS ///

    /**
     * @brief Declare (Allocate) a local variable. Automatically adds llvm::AllocaInst to the parent (Func)
     *
     * Do <i>NOT</i> use this function to declare temporary variables created
     * by the compiler or other variables that don't require allocations.
     * Use <code>createLocal</code> instead.
     * 
     * @param type Type to allocate
     * @param name (optional) Name of the allocation instruction
     * @param future (optional) Future value to be assigned to the local variable
     * @param future_args (optional) Arguments if future value is a function
     * @return Wrapped allocation instruction
     */
    Local::Ptr declareLocal(const Ty::Ptr &type, const std::string &name = "", llvm::Value *future = nullptr, const ValueVec &future_args = ValueVec());

    /**
     * @brief Create a local variable that is <i>not</i> an allocation.
     *
     * @param type Type of the variable
     * @param value (optional) Value the variable holds
     * @return Wrapped variable
     */
    Local::Ptr createLocal(const Ty::Ptr &type, llvm::Value *value = nullptr);

    Val::Ptr createValue(const Ty::Ptr &type, llvm::Value *value = nullptr);

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
     * @return Store instruction returned by llvm::IRBuilder
     */
    llvm::StoreInst *storeValue(const Entity::Ptr &local, const Entity::Ptr &value) const;
    /**
     * @brief Store a value in a local variable.
     * 
     * @param local Local to be stored at (must be a pointer)
     * @param value Value to store in local
     * @return Store instruction returned by llvm::IRBuilder
     */
    llvm::StoreInst *storeValue(const Local::Ptr &local, llvm::Constant *value) const;

    /**
     * @brief Create an instruction for the future assignment of a local
     * 
     * @param local The local
     * @param value Value to be assigned
     */
    static void createFuture(Local &local, llvm::Value *value);
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
     *      Automatically appends to Func::locals.
     * 
     * @param wrap Wrapped llvm::StructType
     * @param name Name of the returned pointer
     * @return Local::Ptr - Wrapped alloca instruction
     */
    Local::Ptr allocateStruct(const Struct::Ptr &wrap, const std::string &name = "");
    /**
     * @brief Allocate the object of the struct type.
     *      Automatically appends to Func::locals.
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
    llvm::ReturnInst *createRet(llvm::BasicBlock *next = nullptr) const;
    /**
     * @brief Create a return instruction with a value.
     * 
     * @param value Value to return
     * @param next (optional) Next insertion point
     * @return ReturnInst * - Return Instruction returned from llvm::IRBuilder
     */
    llvm::ReturnInst *createRet(const Entity::Ptr &value, llvm::BasicBlock *next = nullptr) const;
    /**
     * @brief Create a return instruction with a constant.
     * 
     * @param value Value to return
     * @param next (optional) Next insertion point
     * @return ReturnInst * - Return Instruction returned from llvm::IRBuilder
     */
    llvm::ReturnInst *createRet(llvm::Constant *value, llvm::BasicBlock *next = nullptr) const;

    /**
     * @brief Create a block and set the insert point to it.
     * 
     * @param name (optional) Name of the block
     * @param insert (optional) Start insertion at this block
     * @return BasicBlock * 
     */
    llvm::BasicBlock *createBlock(const std::string &name = "", bool insert = false) const;

    /**
     * @brief Set the current insertion block.
     * 
     * @param block The insertion block
     */
    void setBlock(llvm::BasicBlock *block) const;

    /**
     * @brief Create a binary operation.
     * 
     * @param op Operation
     * @param LHS Left-Hand-Side
     * @param RHS Right-Hand-Side
     * @param name (optional) Name of the result
     * @return Local::Ptr - Result
     */
    Val::Ptr binaryOp(Op op, const Entity::Ptr &LHS, const Entity::Ptr &RHS, const std::string &name = "");

    /**
     * @brief Bitcast a pointer to a type.
     * 
     * @param ptr The original pointer 
     * @param to The destination type
     * @param name (optional) Name of the returned pointer
     * @return Local::Ptr - The returned pointer from the bitcast
     */
    Val::Ptr bitCast(const Entity::Ptr &ptr, const Ty::Ptr &to, const std::string &name = "");

    /**
     * @brief Jump to block.
     * 
     * @param block 
     * @return BranchInst *
     */
    constexpr llvm::BranchInst *jump(llvm::BasicBlock *block) const { return builder->CreateBr(block); }
    /**
     * @brief Jump to `then` if condition is true, else jump to `else´.
     * 
     * @param condition The condition
     * @param then The `then` block
     * @param else_ (optional) The `else` block
     * @return BranchInst *
     */
    llvm::BranchInst *jump(const Val::Ptr &condition, llvm::BasicBlock *then, llvm::BasicBlock *else_ = nullptr) const;

    /**
     * @brief Type cast a value.
     * 
     * @param value The value
     * @param to The destination type
     * @param name (optional) The name of the cast value
     * @return Local::Ptr
     */
    Val::Ptr typeCast(const Entity::Ptr &value, const Ty::Ptr &to, const std::string &name = "typecast");

    /**
     * @brief Get the pointer to an element of an array.
     * 
     * @param array The array
     * @param index The index of the element
     * @param name (optional) The name of the returned pointer
     * @return Local::Ptr
     */
    Local::Ptr getArrayElement(const Entity::Ptr &array, size_t index, const std::string &name = "");

    /**
     * @brief Get the pointer to an element of an array.
     * 
     * @param array The array
     * @param index The index of the element
     * @param name (optional) The name of the returned pointer
     * @return Local::Ptr
     */
    Local::Ptr getArrayElement(const Entity::Ptr &array, const Entity::Ptr &index, const std::string &name = "");

    /**
     * @brief Check whether the given pointer is a nullptr and return the result.
     * 
     * @param pointer The pointer 
     * @param name Name of the result
     * @return Local::Ptr,
     *          true: pointer == nullptr,
     *          false: pointer != nullptr
     */
    Val::Ptr compareToNull(const Entity::Ptr &pointer, const std::string &name = "");

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
    llvm::BasicBlock *ifStatement(const Condition& condition, const std::string &then_name = "then_block", const std::string &else_name = "else_block") const;

    /// GETTER ///

    /**
     * @brief Get the llvm::LLVMContext
     * 
     * @return LLVMContext * 
     */
    llvm::LLVMContext *getContext() const;
    
    /**
     * @brief Get the llvm::Module
     * 
     * @return Module * 
     */
    llvm::Module *getModule() const;
    
    /**
     * @brief Get the llvm::IRBuilder
     * 
     * @return IRBuilder<> * 
     */
    llvm::IRBuilder<> *getBuilder() const;
    
    /**
     * @brief Get the current wrapped parent llvm::Function
     * 
     * @return Func::Ptr
     */
    Func::Ptr getCurrentParent() const;
    
    /**
     * @brief Get the vector of Tys in this context
     * 
     * @return Ty::Vec & 
     */
    Ty::Vec &getTypes();
    
    /**
     * @brief Add a new Ty to this context.
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
    Wrapper(llvm::LLVMContext *context, llvm::Module *module, llvm::IRBuilder<> *builder, const std::string &targetTriple);

    llvm::LLVMContext *context;
    llvm::Module *module;
    llvm::IRBuilder<> *builder;

    Func::Ptr parent; // current parent function

    Func::Map functions;
    Struct::Map structs;
    Ty::Vec types;
};


// get the llvm::Value and type of entity for operations
// load: whether a Local should be loaded
static std::pair<llvm::Value *, Ty::Ptr> process(const Entity::Ptr &entity, bool load = true);

std::nullptr_t complain(const std::string&);

} // namespace wyvern