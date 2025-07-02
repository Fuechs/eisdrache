/**
 * @file wyvern.cpp
 * @brief Wyvern LLVM API Wrapper implementation
 * @version 0.1
 * 
 * @copyright Copyright (c) 2023-2025, Ari.
 * 
 */

#include "wyvern.hpp"

#include <ranges>
#include <utility>

namespace wyvern {

using namespace llvm;

/// ENTITY ///

Entity::Entity(Wrapper::Ptr wrapper) : wrapper(std::move(wrapper)) {}

Entity::~Entity() = default;

/// TY ///

Ty::Ty(Wrapper::Ptr wrapper) { this->wrapper = std::move(wrapper); }

Ty::Ptr Ty::create(const Wrapper::Ptr &wrapper, const Type *llvmTy) {
    Ptr that = nullptr;
    switch (llvmTy->getTypeID()) {
        case Type::IntegerTyID:
            that = std::make_shared<IntTy>(wrapper, llvmTy->getIntegerBitWidth());
            break;
        case Type::HalfTyID:
            that = std::make_shared<FloatTy>(wrapper, 16);
            break;
        case Type::FloatTyID:
            that = std::make_shared<FloatTy>(wrapper, 32);
            break;
        case Type::DoubleTyID:
            that = std::make_shared<FloatTy>(wrapper, 64);
            break;
        case Type::PointerTyID:
            that = std::make_shared<PtrTy>(wrapper, wrapper->getVoidTy());
            break;
        case Type::StructTyID:
            complain("Wrapper::Ty::Ty(): Can not construct Wrapper::Ty from a llvm::Type with ID: Type::StructTyID.");
        case Type::FunctionTyID:
            complain("Wrapper::Ty::Ty(): Can not construct Wrapper::Ty from a llvm::Type with ID: Type::FunctionTyID.");
        case Type::ArrayTyID:
            complain("Wrapper::Ty::Ty(): Can not construct Wrapper::Ty from a llvm::Type with ID: Type::ArrayTyID.");
        default:
            break;
    }

    return wrapper->addTy(that);
}

Ty::Ptr Ty::getPtrTo() {
    return wrapper->addTy(std::make_shared<PtrTy>(wrapper,  shared_from_this()));
}

size_t Ty::getBit() const { return 0; }

/// ALIAS TY ///

AliasTy::AliasTy(Wrapper::Ptr wrapper, std::string alias, Ty::Ptr type)
: Ty(std::move(wrapper)), alias(std::move(alias)), type(std::move(type)) { }

AliasTy::~AliasTy() { alias.clear(); }

size_t AliasTy::getBit() const { return type->getBit(); }

Type *AliasTy::getTy() const { return type->getTy(); }

bool AliasTy::isValidRHS(Ty::Ptr comp) const { return type->isValidRHS(comp); }

bool AliasTy::isEqual(Ty::Ptr comp) const { return type->isEqual(comp); }

/// VOID TY ///

VoidTy::VoidTy(Wrapper::Ptr wrapper) { this->wrapper = std::move(wrapper); }

Type *VoidTy::getTy() const { return Type::getVoidTy(*wrapper->getContext()); }

bool VoidTy::isValidRHS(const Ty::Ptr comp) const { return dynamic_cast<VoidTy *>(comp.get()); }

bool VoidTy::isEqual(const Ty::Ptr comp) const { return dynamic_cast<VoidTy *>(comp.get()); }

/// POINTER TY ///

PtrTy::PtrTy(Wrapper::Ptr wrapper, Ty::Ptr pointee)
    : Ty(std::move(wrapper)), pointee(std::move(pointee)) {}

Ty::Ptr &PtrTy::getPointeeTy() { return pointee; }

size_t PtrTy::getBit() const { return pointee->getBit(); }

Type *PtrTy::getTy() const { return PointerType::get(*wrapper->getContext(), 0); }

bool PtrTy::isValidRHS(const Ty::Ptr comp) const {
    // there are no valid binary operations for pointers
    return false;
}

bool PtrTy::isEqual(const Ty::Ptr comp) const {
    return comp->kind() == PTR                                                  // check whether comp is a pointer
        && pointee->isEqual(dynamic_cast<PtrTy *>(comp.get())->getPointeeTy()); // check whether pointee type is the same
}

/// INTEGER TY ///

IntTy::IntTy(Wrapper::Ptr wrapper, size_t bit, bool _signed)
    : Ty(std::move(wrapper)), bit(bit), _signed(_signed) {}

size_t IntTy::getBit() const { return bit; }

const bool &IntTy::getSigned() const { return _signed; }

Ty::Ptr IntTy::getSignedTy() const {
    return wrapper->addTy(std::make_shared<IntTy>(wrapper, bit, true));
}

Type *IntTy::getTy() const { return Type::getIntNTy(*wrapper->getContext(), bit); }

bool IntTy::isValidRHS(const Ty::Ptr comp) const { return isEqual(comp); }

bool IntTy::isEqual(const Ty::Ptr comp) const {
    if (comp->kind() != INT)
        return false;

    auto conv = dynamic_cast<IntTy *>(comp.get());
    return bit == conv->bit && _signed == conv->_signed;
}

/// FLOAT TY ///

FloatTy::FloatTy(Wrapper::Ptr wrapper, size_t bit) : Ty(std::move(wrapper)), bit(bit) {}

size_t FloatTy::getBit() const { return bit; }

Type *FloatTy::getTy() const {
    switch (bit) {
        case 16:    return Type::getHalfTy(*wrapper->getContext());
        case 32:    return Type::getFloatTy(*wrapper->getContext());
        case 64:    return Type::getDoubleTy(*wrapper->getContext());
        case 128:   return Type::getFP128Ty(*wrapper->getContext());
        default:    return complain("Wrapper::FloatTy::getTy(): Invalid amount of bits ("+std::to_string(bit)+").");
    }
}

bool FloatTy::isValidRHS(const Ty::Ptr comp) const { return isEqual(comp); }

bool FloatTy::isEqual(const Ty::Ptr comp) const {
    return comp->kind() == FLOAT
        && bit == dynamic_cast<FloatTy *>(comp.get())->bit;
}

/// VAL ///

Val::Val(Wrapper::Ptr wrapper, Ty::Ptr type, Value *value,bool immediate)
: Entity(std::move(wrapper)), immediate(immediate), type(std::move(type)), value(value) {}

Val::~Val() = default;

Val::Ptr Val::create(Wrapper::Ptr wrapper, Value *value) {
    return std::make_shared<Val>(std::move(wrapper), nullptr, value);
}

Val::Ptr Val::create(Wrapper::Ptr wrapper, const Ty::Ptr &type, Value *value, bool immediate) {
    return std::make_shared<Val>(std::move(wrapper), type, value, immediate);
}

Val::Ptr Val::dereference(const std::string &name) const {
    if (!type->isPtrTy())
        complain("wyvern::Val::dereference(): Value isn't a pointer.");

    Ty::Ptr loadTy = std::static_pointer_cast<PtrTy>(type)->getPointeeTy();
    LoadInst *load = wrapper->getBuilder()->CreateLoad(loadTy->getTy(), value, name);
    return wrapper->createValue(loadTy, load);
}

/// REFERENCE ///

Reference::Reference(Wrapper::Ptr wrapper, std::string symbol)
: Entity(std::move(wrapper)), symbol(std::move(symbol)) {}

Reference::~Reference() { symbol.clear(); }

Reference &Reference::operator=(const Reference &copy) {
    if (this == &copy)
        return *this;

    symbol = copy.symbol;
    wrapper = copy.wrapper;
    return *this;
}

Entity::Ptr Reference::getEntity() const {
    Entity::Ptr ret = wrapper->getFunc(symbol);

    if (!ret)
        ret = (*wrapper->getCurrentParent())[symbol];

    return ret;
}

/// LOCAL ///

Local::Local(Wrapper::Ptr wrapper, Ty::Ptr type, AllocaInst *ptr, Entity::Ptr initializer)
: Entity(std::move(wrapper)), ptr(ptr), type(std::move(type)), initializer(std::move(initializer)) {}

Local::Local(Wrapper::Ptr wrapper, Ty::Ptr type, AllocaInst *ptr, std::shared_ptr<Func> initializer, Entity::Vec args)
: Entity(std::move(wrapper)), ptr(ptr), type(std::move(type)),
    initializer(std::move(initializer)), args(std::move(args)) {}

Local::~Local() { args.clear(); }

Local::Ptr Local::create(Wrapper::Ptr wrapper, Ty::Ptr type, AllocaInst *ptr, Entity::Ptr initializer) {
    return std::make_shared<Local>(std::move(wrapper), std::move(type), ptr, std::move(initializer));
}

Local::Ptr Local::create(Wrapper::Ptr wrapper, Ty::Ptr type, AllocaInst *ptr, std::shared_ptr<Func> initializer, Entity::Vec args) {
    return std::make_shared<Local>(std::move(wrapper), std::move(type), ptr,
        std::move(initializer), std::move(args));
}

Local &Local::operator=(const Local &copy) {
    if (this == &copy)
        return *this;

    ptr = copy.ptr;
    type = copy.type;
    initializer = copy.initializer;
    args = copy.args;
    wrapper = copy.wrapper;
    return *this;
}

bool Local::operator==(const Local &comp) const { return ptr == comp.ptr; }

bool Local::operator==(const AllocaInst *comp) const { return ptr == comp; }

AllocaInst *Local::operator*() {
    initialize();
    return ptr;
}

void Local::setInitializer(Entity::Ptr value) { initializer = std::move(value); }

void Local::setInitializer(Func::Ptr function, Entity::Vec args) {
    initializer = std::move(function);
    args = std::move(args);
}

void Local::setTy(Ty::Ptr ty) { type = std::move(ty); }

AllocaInst *Local::getPtr() { return operator*(); }

Ty::Ptr Local::getTy() { return type; }

std::string Local::getName() const { return ptr->getName().str(); }

Val::Ptr Local::dereference(bool isImmediate, const std::string &name) {
    initialize();

    LoadInst *load = wrapper->getBuilder()->CreateLoad(type->getTy(), ptr, name);
    return Val::create(wrapper, type, load, isImmediate);
} 

void Local::initialize() {
    if (!initializer)
        return;

    if (auto value = std::static_pointer_cast<Val>(initializer))
        wrapper->getBuilder()->CreateStore(value->getValuePtr(), ptr);
    else if (auto arg = std::static_pointer_cast<Arg>(initializer))
        wrapper->getBuilder()->CreateStore(arg->getPtr(), ptr);
    else if (auto local = std::static_pointer_cast<Local>(initializer))
        wrapper->getBuilder()->CreateStore(local->dereference()->getValuePtr(), ptr);
    else { // must be a function
        auto function = std::static_pointer_cast<Func>(initializer);
        if (function->getTy()->isVoidTy()) {
            // should be a constructor
            // so has to insert itself as the first argument
            Val::Ptr this_as_value = wrapper->createValue(type->getPtrTo(), ptr);
            // constructor requires a pointer to the struct
            // a local would be dereferenced automatically
            args.insert(args.begin(), this_as_value);
            function->call(args);
        } else {
            // call the function and assign the result
            auto ret = function->call(args);
            wrapper->getBuilder()->CreateStore(ret->getValuePtr(), ptr);
        }
    }

    initializer = nullptr;
    args.clear();
}

/// CONDITION ///

Condition::Condition(Wrapper::Ptr wrapper, Op operation, Entity::Ptr lhs, Entity::Ptr rhs)
: Entity(std::move(wrapper)), operation(operation), lhs(std::move(lhs)), rhs(std::move(rhs)) {}

Condition::~Condition() = default;

Val::Ptr Condition::create() const {
    Value *l = process(lhs).first;
    Value *r = process(rhs).first;

    Value *cond = wrapper->getBuilder()->CreateCmp(getPredicate(), l, r, "cmp");

    return Val::create(wrapper, wrapper->getBoolTy(), cond);
}

CmpInst::Predicate Condition::getPredicate() const {
    Ty::Ptr ty = process(lhs).second;

    // compare with type of rhs
    if (!ty->isValidRHS(process(rhs).second))
        complain("Wrapper::Condition::getPredicate(): Incompatible types.");

    if (ty->isFloatTy()) {
        switch (operation) { // TODO: this assumes that the floats are ordered
            case EQU:   return CmpInst::Predicate::FCMP_OEQ;
            case NEQ:   return CmpInst::Predicate::FCMP_ONE;
            case LES:   return CmpInst::Predicate::FCMP_OLT;
            case LTE:   return CmpInst::Predicate::FCMP_OLE;
            case GRE:   return CmpInst::Predicate::FCMP_OGT;
            case GTE:   return CmpInst::Predicate::FCMP_OGE;
            default:    complain("Wrapper::Condition::getPredicate(): Invalid operation.");
        }
    } else if (ty->isIntTy()) {
        if (ty->isSignedTy())
            switch (operation) {
                case EQU:   return CmpInst::Predicate::ICMP_EQ;
                case NEQ:   return CmpInst::Predicate::ICMP_NE;
                case LES:   return CmpInst::Predicate::ICMP_SLT;
                case LTE:   return CmpInst::Predicate::ICMP_SLE;
                case GRE:   return CmpInst::Predicate::ICMP_SGT;
                case GTE:   return CmpInst::Predicate::ICMP_SGE;
                default:    complain("Wrapper::Condition::getPredicate(): Invalid operation.");
            }
        else
            switch (operation) {
                case EQU:   return CmpInst::Predicate::ICMP_EQ;
                case NEQ:   return CmpInst::Predicate::ICMP_NE;
                case LES:   return CmpInst::Predicate::ICMP_ULT;
                case LTE:   return CmpInst::Predicate::ICMP_ULE;
                case GRE:   return CmpInst::Predicate::ICMP_UGT;
                case GTE:   return CmpInst::Predicate::ICMP_UGE;
                default:    complain("Wrapper::Condition::getPredicate(): Invalid operation.");
            }
    } else
        complain("Wrapper::Condition::getPredicate(): Invalid type.");

    return CmpInst::Predicate::BAD_ICMP_PREDICATE; // random value
}

/// ARG ///

Arg::Arg(Ty::Ptr type, std::string name)
: Entity(nullptr), type(std::move(type)), name(std::move(name)) {}

Arg::~Arg() { name.clear(); }

Arg::Ptr Arg::create(const Ty::Ptr &type, const std::string &name) {
    return std::make_shared<Arg>(type, name);
}

Val::Ptr Arg::dereference(bool isImmediate, const std::string &name) const {
    if (!type->isPtrTy())
        complain("wyvern::Arg::dereference(): Argument isn't a pointer.");

    Ty::Ptr loadTy = std::static_pointer_cast<PtrTy>(type)->getPointeeTy();
    LoadInst *load = wrapper->getBuilder()->CreateLoad(loadTy->getTy(), ptr, name);
    return Val::create(wrapper, loadTy, load, isImmediate);
}

/// FUNC ///

Func::Func() {
    func = nullptr;
    type = nullptr;
    parameters = Arg::Vec();
    locals = Local::Map();
    wrapper = nullptr;
}

Func::Func(Wrapper::Ptr wrapper, Ty::Ptr type, const std::string &name, Arg::Vec parameters, bool entry)
: Entity(std::move(wrapper)), type(std::move(type)), parameters(std::move(parameters)), locals(Local::Map()) {

    std::vector<std::string> paramNames;
    std::vector<Type *> paramTypes;
    for (const auto &arg : this->parameters) {
        arg->setWrapper(this->wrapper);
        paramNames.push_back(arg->getName());
        paramTypes.push_back(arg->getTy()->getTy());
    }

    FunctionType *FT = FunctionType::get(this->type->getTy(), paramTypes, false);
    func = Function::Create(FT, Function::ExternalLinkage, name, *this->wrapper->getModule());

    for (size_t i = 0; i < func->arg_size(); i++) {
        func->getArg(i)->setName(paramNames[i]);
        this->parameters[i]->setPtr(func->getArg(i));
    }

    if (entry) {
        BasicBlock *entry_block = BasicBlock::Create(*this->wrapper->getContext(), "entry", func);
        this->wrapper->setBlock(entry_block);
    } else
        verifyFunction(*func);
}

Func::~Func() { locals.clear(); }

Func &Func::operator=(const Func &copy) {
    if (this == &copy)
        return *this;

    func = copy.func;
    type = copy.type;
    parameters = copy.parameters;
    locals = copy.locals;
    wrapper = copy.wrapper;
    return *this;
}

bool Func::operator==(const Func &comp) const { return func == comp.func; }

bool Func::operator==(const Function *comp) const { return func == comp; }

Entity::Ptr Func::operator[](const std::string &symbol) {
    if (locals.contains(symbol))
        return locals[symbol];

    auto x = std::ranges::find_if(parameters.begin(), parameters.end(),
        [&symbol] (const Arg::Ptr &param) { return param->getName() == symbol; });
    if (x != parameters.end())
        return *x;

    return complain("Wrapper::Func::operator[]: Symbol not found: %"+symbol+".");
}

Function *Func::operator*() const { return func; }

Arg::Ptr Func::arg(size_t index) { return parameters[index]; }

Val::Ptr Func::call(const Entity::Vec &args, const std::string &name) const {
    std::vector<Value *> raw_args = {};
    for (auto &arg : args)
        raw_args.push_back(process(arg).first);
    return this->call(raw_args, name);
}      
       
Val::Ptr Func::call(const std::vector<Value *> &args, const std::string &name) const {
    Value *ret = wrapper->getBuilder()->CreateCall(func, args, name);
    return Val::create(wrapper, type, ret);
}

Local::Ptr Func::addLocal(const Local::Ptr &local) {
    std::string symbol;
    if (local->getName() == "unnamed" || locals.contains(local->getName()))
        symbol = local->getName()+std::to_string(locals.size());
    else
        symbol = local->getName();
    locals[symbol] = local;
    return locals[symbol];
}

void Func::addAttr(Attribute attr, int64_t index) const {
    if (index < 0)
        func->addFnAttr(attr);
    else
        func->getArg(index)->addAttr(attr);
}

void Func::addAttr(Attribute::AttrKind attr, int64_t index) const {
    if (index < 0)
        func->addFnAttr(attr);
    else
        func->getArg(index)->addAttr(attr);
}

void Func::setCallingConv(const CallingConv::ID conv) const { func->setCallingConv(conv); }

void Func::setDoesNotThrow() const { func->setDoesNotThrow(); }

Ty::Ptr Func::getTy() { return type; }

std::string Func::getName() const { return func->getName().str(); }

Entity::Ptr Func::getLocal(const std::string &symbol) { return (*this)[symbol]; }

/// STRUCT ///

Struct::Struct() {
    name = "";
    type = nullptr;
    elements = Ty::Vec();
    wrapper = nullptr;
}

Struct::Struct(Wrapper::Ptr wrapper, const std::string &name, const Ty::Vec &elements) {
    std::vector<Type *> elementTypes = {};
    for (const Ty::Ptr &e : elements)
        elementTypes.push_back(e->getTy());
    this->name = name;
    this->type = StructType::create(elementTypes, name);
    this->elements = elements;
    this->wrapper = std::move(wrapper);
}

Struct::~Struct() { name.clear(); }

Struct &Struct::operator=(const Struct &copy) {
    if (this == &copy)
        return *this;

    name = copy.name;
    type = copy.type;
    elements = copy.elements;
    wrapper = copy.wrapper;
    return *this;
}

bool Struct::operator==(const Struct &comp) const { return type == comp.type; }

bool Struct::operator==(const Type *comp) const { return type == comp; }

Ty::Ptr Struct::operator[](size_t index) { return elements.at(index); }

StructType *Struct::operator*() const { return type; }

Local::Ptr Struct::allocate(const std::string &name) {
    return wrapper->declareLocal(shared_from_this(), name);
}

Func::Ptr Struct::createMemberFunc(const Ty::Ptr &type, const std::string &name, Arg::Vec args) {
    args.insert(args.begin(), Arg::create(getPtrTo(), "this"));
    return wrapper->declareFunction(type, this->name+"_"+name, args, true);
}

Type *Struct::getTy() const { return type; }

// can't do arithmetic operations with a struct
bool Struct::isValidRHS(const Ty::Ptr comp) const { return false; }

bool Struct::isEqual(const Ty::Ptr comp) const {
    return dynamic_cast<Struct *>(comp.get()) && dynamic_cast<Struct *>(comp.get())->type == type;
}

/// ARRAY ///

Array::Array(Wrapper::Ptr wrapper, const Ty::Ptr &elementTy, const std::string &name) {
    this->wrapper = std::move(wrapper);
    this->name = name;
    this->elementTy = elementTy;
    this->bufferTy = elementTy->getPtrTo();
    this->self = wrapper->declareStruct(name, {
        bufferTy,                   // TYPE* buffer
        wrapper->getSizeTy(),     // i64 size
        wrapper->getSizeTy(),     // i64 max
        wrapper->getSizeTy(),     // i64 factor
    });

    Func::Ptr malloc = wrapper->getFunc("malloc");
    if (!malloc)
        malloc = wrapper->declareFunction(wrapper->getUnsignedPtrTy(8), "malloc",
            {Arg::create(wrapper->getSizeTy())});

    Func::Ptr free = wrapper->getFunc("free");
    if (!free)
        free = wrapper->declareFunction(wrapper->getVoidTy(), "free",
            {Arg::create(wrapper->getUnsignedPtrTy(8))});

    Func::Ptr memcpy = wrapper->getFunc("memcpy");
    if (!memcpy)
        memcpy = wrapper->declareFunction(wrapper->getUnsignedPtrTy(8), "memcpy",
            {Arg::create(wrapper->getUnsignedPtrTy(8)),
                Arg::create(wrapper->getUnsignedPtrTy(8)),
                Arg::create(wrapper->getSizeTy())});

    { // get_buffer
    get_buffer = self->createMemberFunc(bufferTy, "get_buffer");
    Val::Ptr buffer = wrapper->getElementVal(get_buffer->arg(0), 0, "buffer");
    wrapper->createRet(buffer);
    }

    { // set_buffer
    set_buffer = self->createMemberFunc(wrapper->getVoidTy(), "set_buffer",
                                        {Arg::create(bufferTy, "buffer")});
    Val::Ptr buffer_ptr = wrapper->getElementPtr(set_buffer->arg(0), 0, "buffer_ptr");
    wrapper->storeValue(buffer_ptr, set_buffer->arg(1));
    wrapper->createRet();
    }

    { // get_size
    get_size = self->createMemberFunc(wrapper->getSizeTy(), "get_size");
    Val::Ptr size = wrapper->getElementVal(get_size->arg(0), 1, "size");
    wrapper->createRet(size);
    }

    { // set_size
    set_size = self->createMemberFunc(wrapper->getVoidTy(), "set_size",
                                      {Arg::create(wrapper->getSizeTy(), "size")});
    Val::Ptr size_ptr = wrapper->getElementPtr(set_size->arg(0), 1, "size_ptr");
    wrapper->storeValue(size_ptr, set_size->arg(1));
    wrapper->createRet();
    }

    { // get_max
    get_max = self->createMemberFunc(wrapper->getSizeTy(), "get_max");
    Val::Ptr max = wrapper->getElementVal(get_max->arg(0), 2, "max");
    wrapper->createRet(max);
    }

    { // set_max
    set_max = self->createMemberFunc(wrapper->getVoidTy(), "set_max",
                                     {Arg::create(wrapper->getSizeTy(), "max")});
    Val::Ptr max_ptr = wrapper->getElementPtr(set_max->arg(0), 1, "max_ptr");
    wrapper->storeValue(max_ptr, set_max->arg(1));
    wrapper->createRet();
    }

    { // get_factor
    get_factor = self->createMemberFunc(wrapper->getSizeTy(), "get_factor");
    Val::Ptr factor = wrapper->getElementVal(get_factor->arg(0), 3, "factor");
    wrapper->createRet(factor);
    }

    { // set_factor
    set_factor = self->createMemberFunc(wrapper->getVoidTy(), "set_factor",
                                        {Arg::create(wrapper->getSizeTy(), "factor")});
    Val::Ptr factor_ptr = wrapper->getElementPtr(set_factor->arg(0), 1, "factor_ptr");
    wrapper->storeValue(factor_ptr, set_factor->arg(1));
    wrapper->createRet();
    }

    { // constructor
    constructor = self->createMemberFunc(wrapper->getVoidTy(), "constructor");
    (**constructor)->setCallingConv(CallingConv::Fast);
    (**constructor)->setDoesNotThrow();
    set_buffer->call({constructor->arg(0)->getPtr(), **wrapper->getNullPtr(bufferTy)});
    set_size->call({constructor->arg(0)->getPtr(), **wrapper->getInt(64, 0)});
    set_max->call({constructor->arg(0)->getPtr(), **wrapper->getInt(64, 0)});
    set_factor->call({constructor->arg(0)->getPtr(), **wrapper->getInt(64, 16)});
    wrapper->createRet();
    }

    { // constructor_size
    constructor_size = self->createMemberFunc(wrapper->getVoidTy(), "constructor_size",
                                              {Arg::create(wrapper->getSizeTy(), "size")});
    Val::Ptr byteSize = wrapper->getInt(64, elementTy->getBit() / 8);
    Val::Ptr bytes = wrapper->binaryOp(MUL, constructor_size->arg(1), byteSize, "bytes");
    set_buffer->call({constructor_size->arg(0)->getPtr(), **malloc->call({bytes}, "buffer")});
    set_size->call({constructor_size->arg(0), constructor_size->arg(1)});
    set_max->call({constructor_size->arg(0)->getPtr(), **wrapper->getInt(64, 0)});
    set_factor->call({constructor_size->arg(0)->getPtr(), **wrapper->getInt(64, 16)});
    wrapper->createRet();
    }

    { // constructor_copy
    constructor_copy = self->createMemberFunc(wrapper->getVoidTy(), "constructor_copy",
                                              {Arg::create(self->getPtrTo(), "original")});
    // TODO: implement copy constructor
    wrapper->createRet();
    }

    { // destructor
    destructor = self->createMemberFunc(wrapper->getVoidTy(), "destructor");
    destructor->setCallingConv(CallingConv::Fast);
    destructor->setDoesNotThrow();
    BasicBlock *free_begin = wrapper->createBlock("free_begin");
    BasicBlock *free_close = wrapper->createBlock("free_close");
    Val::Ptr buffer = get_buffer->call({destructor->arg(0)}, "buffer");
    wrapper->jump(wrapper->compareToNull(buffer, "cond"), free_close, free_begin);
    wrapper->setBlock(free_begin);
    wrapper->bitCast(buffer, wrapper->getUnsignedPtrTy(8), "buffer_cast");
    free->call({buffer});
    wrapper->jump(free_close);
    wrapper->setBlock(free_close);
    wrapper->createRet();
    }

    { // resize
    resize = self->createMemberFunc(wrapper->getVoidTy(), "resize",
                                    {Arg::create(wrapper->getSizeTy(), "new_size")});
    BasicBlock *copy = wrapper->createBlock("copy");
    BasicBlock *empty = wrapper->createBlock("empty");
    BasicBlock *end = wrapper->createBlock("end");

    Val::Ptr byteSize = wrapper->getInt(64, elementTy->getBit() / 8);
    Val::Ptr bytes = wrapper->binaryOp(MUL, resize->arg(1), byteSize, "bytes");
    Val::Ptr new_buffer = malloc->call({bytes}, "new_buffer");
    Val::Ptr buffer = get_buffer->call({resize->arg(0)}, "buffer");
    Val::Ptr size = get_size->call({resize->arg(0)}, "size");
    wrapper->jump(wrapper->compareToNull(buffer, "cond"), empty, copy);

    wrapper->setBlock(copy);
    memcpy->call({new_buffer, buffer, size});
    free->call({buffer});
    wrapper->jump(end);

    wrapper->setBlock(empty);
    wrapper->storeValue(new_buffer, wrapper->getNullPtr(bufferTy));
    wrapper->jump(end);

    wrapper->setBlock(end);
    set_buffer->call({resize->arg(0)->getPtr(), **new_buffer});
    Val::Ptr max_ptr = wrapper->getElementPtr(resize->arg(0), 3, "max_ptr");
    wrapper->storeValue(max_ptr, resize->arg(1));
    wrapper->createRet();
    }

    { // is_valid_index
    is_valid_index = self->createMemberFunc(wrapper->getBoolTy(), "is_valid_index",
                                            {Arg::create(wrapper->getSizeTy(), "index")});
    Val::Ptr max = get_max->call({is_valid_index->arg(0)}, "max");
    Val::Ptr comparison = wrapper->binaryOp(LES, is_valid_index->arg(1), max, "equals");
    wrapper->createRet(comparison);
    }

    { // get_at_index
    get_at_index = self->createMemberFunc(elementTy, "get_at_index",
                                        {Arg::create(wrapper->getUnsignedTy(32), "index")});
    Val::Ptr buffer = get_buffer->call({get_at_index->arg(0)}, "buffer");
    Val::Ptr element_ptr = wrapper->getArrayElement(buffer, get_at_index->arg(1), "element_ptr");
    wrapper->createRet(element_ptr->dereference("element"));
    }

    { // set_at_index
    set_at_index = self->createMemberFunc(wrapper->getVoidTy(), "set_at_index",
        {Arg::create(wrapper->getUnsignedTy(32), "index"),
                Arg::create(elementTy, "value")});
    Val::Ptr buffer = get_buffer->call({set_at_index->arg(0)}, "buffer");
    Value *raw_ptr = wrapper->getBuilder()->CreateGEP(bufferTy->getTy(), buffer->getValuePtr(),
        {set_at_index->arg(1)->getPtr()}, "element_ptr");
    Val::Ptr element_ptr = wrapper->createValue(bufferTy, raw_ptr);
    wrapper->storeValue(element_ptr, set_at_index->arg(2));
    wrapper->createRet();
    }
}

Array::~Array() { name.clear(); }

Local::Ptr Array::allocate(const std::string &name) const {
    return wrapper->allocateStruct(self, name);
}

Val::Ptr Array::call(Member callee, const std::vector<Value *> &args, const std::string &name) const {
    switch (callee) {
        case GET_BUFFER:        return get_buffer->call(args, name);
        case SET_BUFFER:        return set_buffer->call(args, name);
        case GET_SIZE:          return get_size->call(args, name);
        case SET_SIZE:          return set_size->call(args, name);
        case GET_MAX:           return get_max->call(args, name);
        case SET_MAX:           return set_max->call(args, name);
        case GET_FACTOR:        return get_factor->call(args, name);
        case SET_FACTOR:        return set_factor->call(args, name);
        case CONSTRUCTOR:       return constructor->call(args, name);
        case CONSTRUCTOR_SIZE:  return constructor_size->call(args, name);
        case CONSTRUCTOR_COPY:  return constructor_copy->call(args, name);
        case DESTRUCTOR:        return destructor->call(args, name);
        case RESIZE:            return resize->call(args, name);
        case IS_VALID_INDEX:    return is_valid_index->call(args, name);
        case GET_AT_INDEX:      return get_at_index->call(args, name);
        case SET_AT_INDEX:      return set_at_index->call(args, name);
        default:                return complain("Wrapper::Array::call(): Callee not implemented.");
    }
}

Val::Ptr Array::call(Member callee, const Local::Vec &args, const std::string &name) const {
    std::vector<Value *> raw_args = {};
    for (auto &local : args)
        raw_args.push_back(local->getPtr());

    return call(callee, raw_args, name);
}

/// WRAPPER ///

Wrapper::~Wrapper() {
    delete builder;
    functions.clear();
    structs.clear();
    types.clear();
}

void Wrapper::initialize() {
    PassRegistry *registry = PassRegistry::getPassRegistry();
    InitializeAllTargetInfos();
    InitializeAllTargets();
    InitializeAllTargetMCs();
    InitializeNativeTarget();
    initializeTarget(*registry);
}

Wrapper::Ptr Wrapper::create(const std::string &moduleID, const std::string &targetTriple) {
    const auto context = new LLVMContext();
    // has to be this way because std::shared_ptr / make_shared cannot access the private constructor,
    // neither should the user be able to
    return Ptr(new Wrapper(context, new Module(moduleID, *context), new IRBuilder<>(*context), targetTriple));
}

void Wrapper::dump(raw_fd_ostream &os) const { module->print(os, nullptr); }

void Wrapper::dump(const std::string &filePath) const {
    std::error_code EC;
    raw_fd_ostream dumpFile(filePath, EC);
    module->print(dumpFile, nullptr);
}

/// TYPES ///

Ty::Ptr Wrapper::getVoidTy() { return addTy(std::make_shared<VoidTy>(shared_from_this())); }

Ty::Ptr Wrapper::getBoolTy() { return addTy(std::make_shared<IntTy>(shared_from_this(), 1)); }

Ty::Ptr Wrapper::getSizeTy() { return addTy(std::make_shared<IntTy>(shared_from_this(), 64)); }

Ty::Ptr Wrapper::getSignedTy(size_t bit) { return addTy(std::make_shared<IntTy>(shared_from_this(), bit, true)); }

Ty::Ptr Wrapper::getSignedPtrTy(size_t bit) { return addTy(std::make_shared<PtrTy>(shared_from_this(), getSignedTy(bit)));}

Ty::Ptr Wrapper::getSignedPtrPtrTy(size_t bit) { return addTy(std::make_shared<PtrTy>(shared_from_this(), getSignedPtrTy(bit))); }

Ty::Ptr Wrapper::getUnsignedTy(size_t bit) { return addTy(std::make_shared<IntTy>(shared_from_this(), bit)); }

Ty::Ptr Wrapper::getUnsignedPtrTy(size_t bit) { return addTy(std::make_shared<PtrTy>(shared_from_this(), getUnsignedTy(bit))); }

Ty::Ptr Wrapper::getUnsignedPtrPtrTy(size_t bit) { return addTy(std::make_shared<PtrTy>(shared_from_this(), getUnsignedPtrTy(bit))); }

Ty::Ptr Wrapper::getFloatTy(size_t bit) { return addTy(std::make_shared<FloatTy>(shared_from_this(), bit)); }

Ty::Ptr Wrapper::getFloatPtrTy(size_t bit) { return addTy(std::make_shared<PtrTy>(shared_from_this(), getFloatTy(bit))); }

Ty::Ptr Wrapper::getFloatPtrPtrTy(size_t bit) { return addTy(std::make_shared<PtrTy>(shared_from_this(), getFloatPtrTy(bit))); };

/// VALUES ///

Val::Ptr Wrapper::getBool(const bool value) {
    return createValue(getBoolTy(), builder->getInt1(value));
}

Val::Ptr Wrapper::getInt(const size_t bit, const uint64_t value) {
    return createValue(getUnsignedTy(bit), builder->getIntN(bit, value));
}

Val::Ptr Wrapper::getFloat(const double value) {
    return createValue(getFloatTy(64), ConstantFP::get(*context, APFloat(value)));
}

Val::Ptr Wrapper::getLiteral(const std::string &value, const std::string &name) {
    return Val::create(shared_from_this(), builder->CreateGlobalString(value, name));
}

Val::Ptr Wrapper::getNull() { return Val::create(shared_from_this(), nullptr); }

Val::Ptr Wrapper::getNullPtr(const Ty::Ptr &ptrTy) {
    return createValue(ptrTy, ConstantPointerNull::get(dyn_cast<PointerType>(ptrTy->getTy())));
}

/// FUNCTIONS ///

Func::Ptr Wrapper::declareFunction(const Ty::Ptr &type, const std::string &name, const Arg::Vec &parameters, bool entry) {
    functions[name] = std::make_shared<Func>(shared_from_this(), type, name, parameters, entry);
    return parent = functions.at(name);
}

Func::Ptr Wrapper::getWrap(const Function *function) {
    auto x = functions.find(function->getName().str());
    if (x == functions.end())
        complain("Could not find Wrapper::Func of @" + function->getName().str() + "().");
    return x->second;
}

bool Wrapper::verifyFunc(const Func::Ptr &wrap) {
    auto x = verifyFunction(***wrap, &errs()); // deref Func::Ptr, Func, llvm::Function

    if (x) // x is true if there are errors
        complain("Verification of @"+wrap->getName()+"() failed.");

    return x;
}

Val::Ptr Wrapper::callFunction(const std::string &callee, const ValueVec &args, const std::string &name) const {
    return functions.at(callee)->call(args, name);
}

Val::Ptr Wrapper::callFunction(const std::string &callee, const Entity::Vec &args, const std::string &name) const {
    return functions.at(callee)->call(args, name);
}

void Wrapper::eraseFunction(const Func::Ptr &wrap) {
    functions.erase(wrap->getName());
    if (parent == wrap)
        parent = nullptr; // TODO: there should be some parent for global variables
    (**wrap)->eraseFromParent();
}

/// LOCALS ///

Local::Ptr Wrapper::declareLocal(const Ty::Ptr &type, const std::string &name, const Entity::Ptr &initializer) {
    AllocaInst *alloca = builder->CreateAlloca(type->getTy(), nullptr, name);
    Local::Ptr local = parent->addLocal(Local::create(shared_from_this(), type, alloca, initializer));

    if (type->isPtrTy()) { // initialize pointers
        auto pointee = std::static_pointer_cast<PtrTy>(type)->getPointeeTy();
        // we're not declaring a new local here to avoid it being automatically dereferenced during initialization
        AllocaInst *deep_alloca = builder->CreateAlloca(pointee->getTy(), nullptr, name + "_deep");
        local->setInitializer(createValue(pointee, deep_alloca));
    }

    return local;
}

Val::Ptr Wrapper::createValue(const Ty::Ptr &type, Value *value) {
    return Val::create(shared_from_this(), type, value);
}

StoreInst *Wrapper::storeValue(const Entity::Ptr &local, const Entity::Ptr &value) const {
    auto [local_raw, local_type] = process(local, false);
    auto [value_raw, _] = process(value, false);

    if (!local_type->isPtrTy())
        return complain("Wrapper::storeValue(): Entity is not a pointer.");

    return builder->CreateStore(value_raw, local_raw);
}

StoreInst *Wrapper::storeValue(const Local::Ptr &local, Constant *value) const {
    if (!local->getTy()->isPtrTy())
        return complain("Wrapper::storeValue(): Local is not a pointer.");

    return builder->CreateStore(value, local->getPtr());
}

/// STRUCT TYPES ///

Struct::Ptr &Wrapper::declareStruct(const std::string &name, const Ty::Vec &elements) {
    structs[name] = make_shared<Struct>(shared_from_this(), name, elements);
    return structs.at(name);
}

Local::Ptr Wrapper::allocateStruct(const Struct::Ptr &wrap, const std::string &name) {
    AllocaInst *alloca = builder->CreateAlloca(**wrap, nullptr, name);
    return parent->addLocal(Local::create(shared_from_this(), wrap, alloca));
}

Local::Ptr Wrapper::allocateStruct(const std::string &typeName, const std::string &name) {
    Struct::Ptr &ref = structs.at(typeName);
    AllocaInst *alloca = builder->CreateAlloca(**ref, nullptr, name);
    return parent->addLocal(Local::create(shared_from_this(), ref->getPtrTo(), alloca));
}

Val::Ptr Wrapper::getElementPtr(const Entity::Ptr &parent, const size_t index, const std::string &name) {
    const auto &[raw, type] = process(parent, false);

    if (!type->isPtrTy())
        complain("Wrapper::getElementPtr(): Type of parent is not a pointer.");

    PtrTy::Ptr ptr = std::static_pointer_cast<PtrTy>(type);

    if (ptr->getPointeeTy()->kind() != Entity::STRUCT)
        complain("Wrapper::getElementPtr(): Type of parent is not a pointer to a struct.");

    Struct::Ptr ref = std::static_pointer_cast<Struct>(ptr->getPointeeTy());
    Value *gep = builder->CreateGEP(**ref, raw, {**getInt(32, 0), **getInt(32, index)}, name);
    return createValue((*ref)[index]->getPtrTo(), gep);
}

Val::Ptr Wrapper::getElementVal(const Entity::Ptr &parent, const size_t index, const std::string &name) {
    Val::Ptr ptr = getElementPtr(parent, index, name+"_ptr");
    return ptr->dereference(name);
}

/// BUILDER ///

ReturnInst *Wrapper::createRet(BasicBlock *next) const {
    ReturnInst *inst = builder->CreateRetVoid();
    if (next)
        builder->SetInsertPoint(next);
    return inst;
}

ReturnInst *Wrapper::createRet(const Entity::Ptr &value, BasicBlock *next) const {
    auto [raw, type] = process(value);

    ReturnInst *inst = builder->CreateRet(raw);
    if (next)
        builder->SetInsertPoint(next);

    if (type != parent->getTy())
        complain("Wrapper::createRet(): Return value has wrong type.");

    return inst;
}

ReturnInst *Wrapper::createRet(Constant *value, BasicBlock *next) const {
    ReturnInst *inst = builder->CreateRet(value);
    if (next)
        builder->SetInsertPoint(next);
    return inst;
}

BasicBlock *Wrapper::createBlock(const std::string &name, const bool insert) const {
    BasicBlock *BB = BasicBlock::Create(*context, name, **parent);
    if (insert)
        setBlock(BB);
    return BB;
}

void Wrapper::setBlock(BasicBlock *block) const {
    builder->SetInsertPoint(block);
}

Val::Ptr Wrapper::binaryOp(const Op op, const Entity::Ptr &LHS, const Entity::Ptr &RHS, const std::string &name) {
    auto [lv, ty] = process(LHS);
    auto [rv, rty] = process(RHS);

    if (!ty->isValidRHS(rty))
        complain("Wrapper::binaryOp(): LHS and RHS types differ too much.");

    switch (op) {
        case ADD:
            return ty->isFloatTy()
                ? createValue(ty, builder->CreateFAdd(lv, rv, name))
                : createValue(ty, builder->CreateAdd(lv, rv, name));

        case SUB:
            return ty->isFloatTy()
                ? createValue(ty, builder->CreateFSub(lv, rv, name))
                : createValue(ty, builder->CreateSub(lv, rv, name));

        case MUL:
            return ty->isFloatTy()
                ? createValue(ty, builder->CreateFMul(lv, rv, name))
                : createValue(ty, builder->CreateMul(lv, rv, name));

        case DIV:
            // the result of a division is always a float
            return createValue(getFloatTy(64), builder->CreateFDiv(lv, rv, name));

        case MOD:
            if (ty->isFloatTy())
                return createValue(ty, builder->CreateFRem(lv, rv, name));

            if (ty->isSignedTy())
                return createValue(ty, builder->CreateSRem(lv, rv, name));

            return createValue(ty, builder->CreateURem(lv, rv, name));

        case OR:  return createValue(ty, builder->CreateOr(lv, rv, name));
        case XOR: return createValue(ty, builder->CreateXor(lv, rv, name));
        case AND: return createValue(ty, builder->CreateAnd(lv, rv, name));
        case LSH: return createValue(ty, builder->CreateShl(lv, rv, name));
        case RSH: return createValue(ty, builder->CreateLShr(lv, rv, name));

        case EQU:
        case NEQ:
        case LES:
        case LTE:
        case GRE:
        case GTE:
            return Condition(shared_from_this(), op, LHS, RHS).create();

        default:
            return complain("Wrapper::binaryOp(): Operation (ID "+std::to_string(op)+") not implemented.");
    }
}

Val::Ptr Wrapper::bitCast(const Entity::Ptr &ptr, const Ty::Ptr &to, const std::string &name) {
    Value *value = process(ptr, false).first;
    Value *cast = builder->CreateBitCast(value, to->getTy(), name);
    return createValue(to, cast);
}

BranchInst *Wrapper::jump(const Val::Ptr &condition, BasicBlock *then, BasicBlock *else_) const {
    return builder->CreateCondBr(condition->getValuePtr(), then, else_);
}

Val::Ptr Wrapper::typeCast(const Entity::Ptr &value, const Ty::Ptr &to, const std::string &name) {
    auto [v, from] = process(value);
    if (from->isEqual(to)) // value is already that type -> no cast necessary
        return createValue(to, v);

    // 'from' is a float
    if (from->isFloatTy()) {
        if (to->isFloatTy())                                                        // FLOAT -> FLOAT
            return from->getBit() < to->getBit()
                ? createValue(to, builder->CreateFPExt(v, to->getTy(), name))
                : createValue(to, builder->CreateFPTrunc(v, to->getTy(), name));

        if (to->isSignedTy())                                                       // FLOAT -> SIGNED
            return createValue(to, builder->CreateFPToSI(v, to->getTy(), name));

        if (to->isPtrTy())                                                          // FLOAT -> POINTER
            return complain("Wrapper::typeCast(): Invalid type cast (Float -> Pointer).");
                                                                                    // FLOAT -> UNSIGNED
        return createValue(to, builder->CreateFPToUI(v, to->getTy(), name));
    }

    // 'from' is a signer integer
    if (from->isSignedTy()) {
        if (to->isFloatTy())                                                        // SIGNED -> FLOAT
            return createValue(to, builder->CreateSIToFP(v, to->getTy(), name));

        if (to->isPtrTy())                                                          // SIGNED -> POINTER
            return createValue(to, builder->CreateIntToPtr(v, to->getTy(), name));

        if (to->isSignedTy())                                                       // SIGNED -> SIGNED
            return from->getBit() < to->getBit()
                ? createValue(to, builder->CreateSExt(v, to->getTy(), name))
                : createValue(to, builder->CreateTrunc(v, to->getTy(), name));

        return from->getBit() < to->getBit()                                        // SIGNED -> UNSIGNED
            ? createValue(to, builder->CreateZExt(v, to->getTy(), name))
            : createValue(to, builder->CreateTrunc(v, to->getTy(), name));
    }

    // 'from' is a pointer
    if (from->isPtrTy()) {
        if (to->isFloatTy())                                                        // POINTER -> FLOAT
            return complain("Wrapper::typeCast(): Invalid type cast (Pointer -> Float).");

        if (to->isPtrTy())                                                          // POINTER -> POINTER
            return bitCast(value, to, name);
                                                                                    // POINTER -> INTEGER
        return createValue(to, builder->CreatePtrToInt(v, to->getTy(), name));
    }

    // 'from' is an unsigned integer

    if (to->isFloatTy())                                                            // UNSIGNED -> FLOAT
        return createValue(to, builder->CreateUIToFP(v, to->getTy(), name));

    if (to->isPtrTy())                                                              // UNSIGNED -> POINTER
        return createValue(to, builder->CreateIntToPtr(v, to->getTy(), name));
                                                                                    // UNSIGNED -> INTEGER
    return from->getBit() < to->getBit()
        ? createValue(to, builder->CreateZExt(v, to->getTy(), name))
        : createValue(to, builder->CreateTrunc(v, to->getTy(), name));

}

Val::Ptr Wrapper::getArrayElement(const Entity::Ptr &array, size_t index, const std::string &name) {
    auto [raw, ty] = process(array, false);
    Value *ptr = builder->CreateGEP(ty->getTy(), raw,{**getInt(32, index)}, name);
    return createValue(ty, ptr);
}

Val::Ptr Wrapper::getArrayElement(const Entity::Ptr &array, const Entity::Ptr &index, const std::string &name) {
    auto [raw, ty] = process(array, false);
    Value *index_value = process(index, false).first;

    Value *ptr = builder->CreateGEP(ty->getTy(), raw, {index_value}, name);
    return createValue(ty, ptr);
}

Val::Ptr Wrapper::compareToNull(const Entity::Ptr &pointer, const std::string &name) {
    auto [ptr, ty] = process(pointer, false);

    if (!ty->isPtrTy())
        complain("Wrapper::compareToNull(): Entity is not a pointer.");

    Value *cond = builder->CreateICmpEQ(ptr, **getNullPtr(ty), name);
    return createValue(getBoolTy(), cond);
}


// TODO: refactor this for Entity
Val::Ptr Wrapper::unaryOp(Op op, const Local::Ptr &expr, const std::string &name) {
    Val::Ptr load = expr->dereference();
    Ty::Ptr loadTy = load->getTy();

    switch (op) {
        case NEG: {
            if (loadTy->isFloatTy())
                return createValue(loadTy, builder->CreateFNeg(load->getValuePtr(), name));

            // the result of a negation is always a signed integer type
            auto cast = std::static_pointer_cast<IntTy>(loadTy);
            return createValue(cast ? cast->getSignedTy() : loadTy, builder->CreateNeg(load->getValuePtr(), name));
        }
        case NOT: return createValue(loadTy, builder->CreateNot(load->getValuePtr(), name));
        default:  return complain("Wrapper::unaryOp(): Operation not implemented.");
    }
}

BasicBlock *Wrapper::ifStatement(const Condition &condition, const std::string &then_name, const std::string &else_name) const {
    BasicBlock *then = createBlock(then_name);
    BasicBlock *else_ = createBlock(else_name);

    jump(condition.create(), then, else_);
    setBlock(then);

    return else_;
}

/// GETTER ///

LLVMContext *Wrapper::getContext() const { return context; }

Module *Wrapper::getModule() const { return module; }

IRBuilder<> *Wrapper::getBuilder() const { return builder; }

Func::Ptr Wrapper::getCurrentParent() const { return parent; }

Ty::Vec &Wrapper::getTypes() { return types; }

Ty::Ptr Wrapper::addTy(const Ty::Ptr &ty) {
    auto x = std::ranges::find_if(types.begin(), types.end(),
        [&ty] (const Ty::Ptr &e) { return ty->isEqual(e); });
    if (x != types.end())
        return *x;

    types.push_back(ty);
    return types.back();
}

Func::Ptr Wrapper::getFunc(const std::string &name, bool warn) {
    if (!functions.contains(name))
        return warn ? complain("Wrapper::getFunc(): Function @"+name+"() does not exist.") : nullptr;

    return functions[name];
}

void Wrapper::setParent(Func::Ptr func) { parent = std::move(func); }

/// PRIVATE ///

Wrapper::Wrapper(LLVMContext *context, Module *module, IRBuilder<> *builder, const std::string &targetTriple) {
    this->context = context;
    this->module = module;
    this->builder = builder;
    parent = nullptr;
    functions = Func::Map();
    structs = Struct::Map();

    auto targetOptions = TargetOptions();
    targetOptions.FloatABIType = FloatABI::Hard;
    const TargetMachine *targetMachine = nullptr;

    if (targetTriple.empty()) {
        auto engineBuilder = EngineBuilder();
        engineBuilder.setTargetOptions(targetOptions);
        targetMachine = engineBuilder.selectTarget();
    } else {
        std::string error;
        const Target *target = TargetRegistry::lookupTarget(targetTriple, error);
        if (!target)
            complain("TargetRegistry::lookupTarget() failed: " + error);
        targetMachine = target->createTargetMachine(targetTriple, "generic", "", targetOptions, {});
    }

    module->setTargetTriple(targetMachine->getTargetTriple().str());
    module->setDataLayout(targetMachine->createDataLayout());
}

std::pair<Value *, Ty::Ptr> process(const Entity::Ptr &entity, bool load) {
    switch (entity->kind()) {
        case Entity::VALUE: {
            auto convert = std::static_pointer_cast<Val>(entity);

            if (convert->isImmediate() || !load)
                return { convert->getValuePtr(), convert->getTy() };

            auto deref = convert->dereference();
            return { deref->getValuePtr(), deref->getTy() };
        }
        case Entity::LOCAL: {
            Value *value;
            auto convert = std::static_pointer_cast<Local>(entity);
            if (load) value = convert->dereference()->getValuePtr();
            else      value = convert->getPtr();
            return { value, convert->getTy() };
        }
        case Entity::ARG: {
            auto convert = std::static_pointer_cast<Arg>(entity);
            return { convert->getPtr(), convert->getTy() };
        }
        default:
            complain("wyvern::process(): Entity is not a value, local or argument.");
            return { nullptr, nullptr };
    }
}

std::nullptr_t complain(const std::string &message) {
    std::cerr << "\033[31mError\033[0m: " << message << "\n";
    exit(1);
    return nullptr; // for warnings
}


} // namespace wyvern
