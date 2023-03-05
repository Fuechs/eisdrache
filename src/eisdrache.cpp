/**
 * @file eisdrache.cpp
 * @author fuechs
 * @brief Eisdrache class implementation
 * @version 0.1
 * @date 2023-01-30
 * 
 * @copyright Copyright (c) 2023, Fuechs.
 * 
 */

#include "eisdrache.hpp"

namespace llvm {

/// EISDRACHE TY ///

Eisdrache::Ty::Ty(Eisdrache *eisdrache, size_t bit, size_t ptrDepth, bool isFloat, bool isSigned)
: eisdrache(eisdrache), bit(bit), ptrDepth(ptrDepth), 
    isFloat(isFloat), isSigned(isSigned), structTy(nullptr) {}

Eisdrache::Ty::Ty(Eisdrache *eisdrache, Struct &structTy, size_t ptrDepth)
: eisdrache(eisdrache), bit(0), ptrDepth(ptrDepth), isFloat(false), isSigned(false), structTy(&structTy) {}

Eisdrache::Ty &Eisdrache::Ty::operator=(const Ty &copy) {
    bit = copy.bit;
    ptrDepth = copy.ptrDepth;
    isFloat = copy.isFloat;
    isSigned = copy.isSigned;
    structTy = copy.structTy;
    eisdrache = copy.eisdrache;
    return *this;
}

bool Eisdrache::Ty::operator==(const Ty &comp) const {
    return (bit == comp.bit 
        && ptrDepth == comp.ptrDepth 
        && isFloat == comp.isFloat
        && isSigned == comp.isSigned
        && structTy == comp.structTy
        && eisdrache == comp.eisdrache);
}

bool Eisdrache::Ty::operator==(const Type *comp) const {
    return (bit == comp->getIntegerBitWidth() 
        && (bool) ptrDepth == comp->isPointerTy()
        && isFloat == comp->isFloatingPointTy());
}

Eisdrache::Ty Eisdrache::Ty::operator*() const {
    return Ty(eisdrache, bit, ptrDepth > 0 ? ptrDepth - 1 : 0, isFloat);    
}

Type *Eisdrache::Ty::getTy() const {
    if (ptrDepth > 0)
        return PointerType::get(*eisdrache->getContext(), 0);
    
    if (structTy)
        return **structTy;

    if (isFloat) 
        switch (bit) {
            case 16:    return Type::getHalfTy(*eisdrache->getContext());
            case 32:    return Type::getFloatTy(*eisdrache->getContext());
            case 64:    return Type::getDoubleTy(*eisdrache->getContext());
            default:    return complain("Eisdrache::Ty::getTy(): Invalid amount of bits ("+std::to_string(bit)+") for floating point type (expected 16|32|64).");
        }

    if (bit == 0)
        return Type::getVoidTy(*eisdrache->getContext());
    
    return Type::getIntNTy(*eisdrache->getContext(), bit);
}

Eisdrache::Ty Eisdrache::Ty::getPtrTo() const { return Ty(eisdrache, bit, ptrDepth + 1, isFloat); }

Eisdrache::Struct &Eisdrache::Ty::getStructTy() const {
    if (!structTy)
        Eisdrache::complain("Eisdrache::Ty::getStructTy(): Tried to get struct type, but there is none.");
    return *structTy;
}

bool Eisdrache::Ty::isFloatTy() const { return isFloat; }

bool Eisdrache::Ty::isSignedTy() const { return isSigned; }

bool Eisdrache::Ty::isPtrTy() const { return ptrDepth > 0; }

/// EISDRACHE LOCAL ///

Eisdrache::Local::Local(Eisdrache *eisdrache, Ty type, Value *ptr, Value *future)
: eisdrache(eisdrache), type(type), v_ptr(ptr), future(future) {}

Eisdrache::Local& Eisdrache::Local::operator=(const Local &copy) {
    v_ptr = copy.v_ptr;
    future = copy.future;
    eisdrache = copy.eisdrache;
    return *this;
}

bool Eisdrache::Local::operator==(const Local &comp) const { return v_ptr == comp.v_ptr; }

bool Eisdrache::Local::operator==(const Value *comp) const { return v_ptr == comp; }

AllocaInst *Eisdrache::Local::operator*() {
    if (!isAlloca())
        return complain("Eisdrache::Local::operator*(): Tried to get AllocaInst * of Value * (%"+v_ptr->getName().str()+").");
    return a_ptr;
}

void Eisdrache::Local::setPtr(Value *ptr) { v_ptr = ptr; }

void Eisdrache::Local::setFuture(Value *future) { this->future = future; }

AllocaInst *Eisdrache::Local::getAllocaPtr() { return operator*(); }

Value *Eisdrache::Local::getValuePtr() { return v_ptr; }

const Eisdrache::Ty &Eisdrache::Local::getTy() { return type; }

bool Eisdrache::Local::isAlloca() { return isa<AllocaInst>(v_ptr); }

Eisdrache::Local &Eisdrache::Local::loadValue(bool force, std::string name) {
    if ((!force && !isAlloca()) || !type.isPtrTy())
        return *this;

    if (isAlloca())
        invokeFuture();

    Ty loadTy = *type;
    LoadInst *load = eisdrache->getBuilder()->CreateLoad(loadTy.getTy(), 
        v_ptr, name.empty() ? v_ptr->getName().str()+"_load" : name);
    return eisdrache->getCurrentParent().addLocal(Local(eisdrache, loadTy, load));
}

void Eisdrache::Local::invokeFuture() {
    eisdrache->getBuilder()->CreateStore(future, v_ptr);
}

/// EISDRACHE FUNC ///

Eisdrache::Func::Func() {
    func = nullptr;
    type = Ty();
    parameters = Local::Vec();
    locals = Local::Vec();
    eisdrache = nullptr;
}

Eisdrache::Func::Func(Eisdrache *eisdrache, Ty type, std::string name, Ty::Map parameters, bool entry) {
    this->eisdrache = eisdrache;
    this->type = type;
    this->locals = Local::Vec();
    this->parameters = Local::Vec();

    std::vector<std::string> paramNames;
    std::vector<Type *> paramTypes;
    for (Ty::Map::value_type &param : parameters) {
        paramNames.push_back(param.first);
        paramTypes.push_back(param.second.getTy());
        this->parameters.push_back(Local(eisdrache, param.second));
    }

    FunctionType *FT = FunctionType::get(type.getTy(), paramTypes, false);
    func = Function::Create(FT, Function::ExternalLinkage, name, *eisdrache->getModule());

    for (size_t i = 0; i < func->arg_size(); i++) {  
        func->getArg(i)->setName(paramNames[i]);
        this->parameters[i].setPtr(func->getArg(i));
    }

    if (entry) {
        BasicBlock *entry = BasicBlock::Create(*eisdrache->getContext(), "entry", func);
        eisdrache->setBlock(entry);
    } else
        llvm::verifyFunction(*func);
}

Eisdrache::Func::~Func() { locals.clear(); }

Eisdrache::Func &Eisdrache::Func::operator=(const Func &copy) {
    func = copy.func;
    type = copy.type;
    parameters = copy.parameters;
    locals = copy.locals;
    eisdrache = copy.eisdrache;
    return *this;
}

bool Eisdrache::Func::operator==(const Func &comp) const { return func == comp.func; }

bool Eisdrache::Func::operator==(const Function *comp) const { return func == comp; }

const Eisdrache::Ty &Eisdrache::Func::operator[](Value *local) {
    if (isa<Argument>(local)) {
        for (Local &param : parameters)
            if (param.getValuePtr() == local) 
                return param.getTy();
        Eisdrache::complain("Eisdrache::Func::operator[](): Argument (Value) is not an existing argument.");
    }
    
    if (isa<AllocaInst>(local)) {
        for (Local &alloca : locals)
            if (alloca == local)
                return alloca.getTy();
        Eisdrache::complain("Eisdrache::Func::operator[](): AllocaInst (Value) is not an existing local.");
    }  
    
    Eisdrache::complain("Eisdrache::Func::operator[](): Value is not an Argument or AllocaInst");
    return type; // silence warning
}

Function *Eisdrache::Func::operator*() { return func; }

Eisdrache::Local &Eisdrache::Func::arg(size_t index) { return parameters[index]; }

Eisdrache::Local &Eisdrache::Func::call(ValueVec args, std::string name) { 
    Value *ret = eisdrache->getBuilder()->CreateCall(func, args, name); 
    return eisdrache->getCurrentParent().addLocal(Local(eisdrache, type, ret));
}
 
Eisdrache::Local &Eisdrache::Func::addLocal(Local local) { 
    locals.push_back(local); 
    return locals.back();
}

const Eisdrache::Ty &Eisdrache::Func::getTy() const { return type; }

/// EISDRACHE STRUCT ///

Eisdrache::Struct::Struct() {
    type = nullptr;
    ptr = Ty();
    elements = Ty::Vec();
    eisdrache = nullptr;
}

Eisdrache::Struct::Struct(Eisdrache *eisdrache, std::string name, Ty::Vec elements) {
    TypeVec elementTypes = TypeVec();
    for (Ty &e : elements)
        elementTypes.push_back(e.getTy());
    this->type = StructType::create(elementTypes, name);
    this->ptr = Ty(eisdrache, *this, 1);
    this->elements = elements;
    this->eisdrache = eisdrache;
}

Eisdrache::Struct::~Struct() {}

Eisdrache::Struct &Eisdrache::Struct::operator=(const Struct &copy) {
    type = copy.type;
    ptr = copy.ptr;
    elements = copy.elements;
    eisdrache = copy.eisdrache;
    return *this;
}

bool Eisdrache::Struct::operator==(const Struct &comp) const { return type == comp.type; }

bool Eisdrache::Struct::operator==(const Type *comp) const { return type == comp; }

const Eisdrache::Ty &Eisdrache::Struct::operator[](size_t index) const { return elements.at(index); }

StructType *Eisdrache::Struct::operator*() { return type; }

Eisdrache::Local &Eisdrache::Struct::allocate(std::string name) {
    return eisdrache->allocateStruct(*this, name);
} 

const Eisdrache::Ty &Eisdrache::Struct::getPtrTy() const { return ptr; }

/// EISDRACHE WRAPPER ///

Eisdrache::~Eisdrache() {
    delete builder;
    functions.clear();
}

void Eisdrache::initialize() {
    PassRegistry *registry = PassRegistry::getPassRegistry();
    InitializeAllTargetInfos();
    InitializeAllTargets();
    InitializeAllTargetMCs();
    InitializeNativeTarget();
    initializeTarget(*registry);
}

Eisdrache *Eisdrache::create(std::string moduleID, std::string targetTriple) {
    LLVMContext *context = new LLVMContext();
    return new Eisdrache(context, new Module(moduleID, *context), new IRBuilder<>(*context), targetTriple);
}

Eisdrache *Eisdrache::create(LLVMContext *context, std::string moduleID, std::string targetTriple) {
    return new Eisdrache(context, new Module(moduleID, *context), new IRBuilder<>(*context), targetTriple);
}

Eisdrache *Eisdrache::create(Module *module, std::string targetTriple) {
    return new Eisdrache(&module->getContext(), module, new IRBuilder<>(module->getContext()), targetTriple);
}

Eisdrache *Eisdrache::create(Module *module, IRBuilder<> *builder, std::string targetTriple) {
    return new Eisdrache(&module->getContext(), module, builder, targetTriple);
};

void Eisdrache::dump(raw_fd_ostream &os) { module->print(os, nullptr); }

/// TYPES ///

Eisdrache::Ty Eisdrache::getVoidTy() { return Ty(this); }

Eisdrache::Ty Eisdrache::getBoolTy() { return Ty(this, 1); }

Eisdrache::Ty Eisdrache::getSizeTy() { return Ty(this, 64); }

Eisdrache::Ty Eisdrache::getSignedTy(size_t bit) { return Ty(this, bit, 0, false, true); }

Eisdrache::Ty Eisdrache::getSignedPtrTy(size_t bit) { return Ty(this, bit, 1, false, true);}

Eisdrache::Ty Eisdrache::getSignedPtrPtrTy(size_t bit) { return Ty(this, bit, 2, false, true); }

Eisdrache::Ty Eisdrache::getUnsignedTy(size_t bit) { return Ty(this, bit, 0, false, true); }

Eisdrache::Ty Eisdrache::getUnsignedPtrTy(size_t bit) { return Ty(this, bit, 1, false, true); }

Eisdrache::Ty Eisdrache::getUnsignedPtrPtrTy(size_t bit) { return Ty(this, bit, 2, false, true); }

Eisdrache::Ty Eisdrache::getFloatTy(size_t bit) { return Ty(this, bit, 0, true, true); }

Eisdrache::Ty Eisdrache::getFloatPtrTy(size_t bit) { return Ty(this, bit, 1, true, true); }

Eisdrache::Ty Eisdrache::getFloatPtrPtrTy(size_t bit) { return Ty(this, bit, 2, true, true); };

/// VALUES ///

ConstantInt *Eisdrache::getBool(bool value) { return builder->getInt1(value); }

ConstantInt *Eisdrache::getInt(size_t bit, uint64_t value) { return builder->getIntN(bit, value); }

Value *Eisdrache::getNegative(ConstantInt *value) { 
    return builder->CreateSub(builder->getIntN(value->getBitWidth(), 0), value, "eisdrache_negate_");
}

ConstantFP *Eisdrache::getFloat(double value) { return ConstantFP::get(*context, APFloat(value)); }

Constant *Eisdrache::getLiteral(std::string value, std::string name) { return builder->CreateGlobalStringPtr(value, name); }

/// FUNCTIONS ///

Eisdrache::Func &Eisdrache::declareFunction(Ty type, std::string name, Ty::Vec parameters) {
    Ty::Map parsedParams = Ty::Map();
    for (Ty &param : parameters)
        parsedParams[std::to_string(parsedParams.size())] = param;
    functions[name] = Func(this, type, name, parsedParams);
    parent = &functions.at(name);
    return *parent;
}

Eisdrache::Func &Eisdrache::declareFunction(Ty type, std::string name, Ty::Map parameters, bool entry) {
    functions[name] = Func(this, type, name, parameters, entry);
    parent = &functions.at(name);
    return *parent;
}

Eisdrache::Func &Eisdrache::getWrap(Function *function) {
    for (Func::Map::value_type &wrap : functions)
        if (wrap.second == function)
            return wrap.second;
    complain("Could not find Eisdrache::Func of @" + function->getName().str() + "().");
    return functions.end()->second;
}

bool Eisdrache::verifyFunc(Func &wrap) { 
    return llvm::verifyFunction(**wrap); 
}

Eisdrache::Local &Eisdrache::callFunction(Func &wrap, ValueVec args, std::string name) { 
    return wrap.call(args, name);
}

Eisdrache::Local &Eisdrache::callFunction(std::string callee, ValueVec args, std::string name) { 
    return functions.at(callee).call(args, name);
}
 
/// LOCALS ///

Eisdrache::Local &Eisdrache::declareLocal(Ty type, std::string name, Value *value) {
    AllocaInst *alloca = builder->CreateAlloca(type.getTy(), nullptr, name);
    return parent->addLocal(Local(this, type.getPtrTo(), alloca, value));
}

Eisdrache::Local &Eisdrache::loadLocal(Local &local, std::string name) { return local.loadValue(); }

/// STRUCT TYPES ///

Eisdrache::Struct &Eisdrache::declareStruct(std::string name, Ty::Vec elements) {
    structs[name] = Struct(this, name, elements);
    return structs.at(name);
}

Eisdrache::Local &Eisdrache::allocateStruct(Struct &wrap, std::string name) {
    AllocaInst *alloca = builder->CreateAlloca(*wrap, nullptr, name);
    return parent->addLocal(Local(this, Ty(this, wrap, 1), alloca));
}

Eisdrache::Local &Eisdrache::allocateStruct(std::string typeName, std::string name) {
    Struct &ref = structs.at(typeName);
    AllocaInst *alloca = builder->CreateAlloca(*ref, nullptr, name);
    return parent->addLocal(Local(this, ref.getPtrTy(), alloca));
}

Eisdrache::Local &Eisdrache::getElementPtr(Local &parent, size_t index, std::string name) {
    Struct &ref = parent.getTy().getStructTy();
    Value *gep = builder->CreateGEP(*ref, parent.getValuePtr(), 
        {getInt(32, 0), getInt(32, index)}, name);
    return this->parent->addLocal(Local(this, ref[index].getPtrTo(), gep));
}

Eisdrache::Local &Eisdrache::getElementVal(Local &parent, size_t index, std::string name) {
    Local &ptr = getElementPtr(parent, index, name+"_ptr");
    return ptr.loadValue(true, name);
}

/// BUILDER ///

ReturnInst *Eisdrache::createRet(BasicBlock *next) {
    ReturnInst *inst = builder->CreateRetVoid();
    if (next)
        builder->SetInsertPoint(next);
    return inst;
}

ReturnInst *Eisdrache::createRet(Local &value, BasicBlock *next) {
    ReturnInst *inst = builder->CreateRet(value.loadValue().getValuePtr());
    if (next)
        builder->SetInsertPoint(next);
    return inst;
}

ReturnInst *Eisdrache::createRet(Constant *value, BasicBlock *next) {
    ReturnInst *inst = builder->CreateRet(value);
    if (next)
        builder->SetInsertPoint(next);
    return inst;
}

void Eisdrache::setBlock(BasicBlock *block) {
    builder->SetInsertPoint(block);
}

/// GETTER ///

LLVMContext *Eisdrache::getContext() { return context; }

Module *Eisdrache::getModule() { return module; }

IRBuilder<> *Eisdrache::getBuilder() { return builder; }

Eisdrache::Func &Eisdrache::getCurrentParent() { return *parent; }

/// PRIVATE ///

Eisdrache::Eisdrache(LLVMContext *context, Module *module, IRBuilder<> *builder, std::string targetTriple) {
    this->context = context;
    this->module = module;
    this->builder = builder;
    parent = nullptr;
    functions = Func::Map();
    structs = Struct::Map();

    TargetOptions targetOptions = TargetOptions();
    targetOptions.FloatABIType = FloatABI::Hard;
    TargetMachine *targetMachine = nullptr;

    if (targetTriple.empty()) {
        EngineBuilder engineBuilder = EngineBuilder();
        engineBuilder.setTargetOptions(targetOptions);
        targetMachine = engineBuilder.selectTarget();
    } else {
        std::string error;
        const Target *target = TargetRegistry::lookupTarget(targetTriple, error);
        if (!target) 
            complain("TargetRegistry::lookupTargt() failed: " + error);
        targetMachine = target->createTargetMachine(targetTriple, "generic", "", targetOptions, None);
    }

    module->setTargetTriple(targetMachine->getTargetTriple().str());
    module->setDataLayout(targetMachine->createDataLayout());
}

std::nullptr_t Eisdrache::complain(std::string message) {
    std::cerr << "\033[31mError\033[0m: " << message << "\n"; 
    exit(1);
    return nullptr; // for warnings
} 

} // namespace llvm