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
: eisdrache(eisdrache), bit(bit), ptrDepth(ptrDepth), isFloat(isFloat), isSigned(isSigned) {}

Eisdrache::Ty &Eisdrache::Ty::operator=(const Ty &copy) {
    bit = copy.bit;
    ptrDepth = copy.ptrDepth;
    isFloat = copy.isFloat;
    isSigned = copy.isSigned;
    eisdrache = copy.eisdrache;
    return *this;
}

bool Eisdrache::Ty::operator==(const Ty &comp) {
    return (bit == comp.bit 
        && ptrDepth == comp.ptrDepth 
        && isFloat == comp.isFloat);
}

bool Eisdrache::Ty::operator==(const Type *comp) {
    return (bit == comp->getIntegerBitWidth() 
        && (bool) ptrDepth == comp->isPointerTy()
        && isFloat == comp->isFloatingPointTy());
}

Eisdrache::Ty Eisdrache::Ty::operator*() {
    return Ty(eisdrache, bit, ptrDepth > 0 ? ptrDepth - 1 : 0, isFloat);    
}

Type *Eisdrache::Ty::getTy() {
    if (ptrDepth > 0)
        return PointerType::get(*eisdrache->getContext(), 0);

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

Eisdrache::Ty Eisdrache::Ty::getPtrTo() { return Ty(eisdrache, bit, ptrDepth + 1, isFloat); }

bool Eisdrache::Ty::isFloatTy() { return isFloat; }

bool Eisdrache::Ty::isSignedTy() { return isSigned; }

/// EISDRACHE LOCAL ///

Eisdrache::Local::Local(Eisdrache *eisdrache, Value *ptr, Value *future)
: eisdrache(eisdrache), v_ptr(ptr), future(future) {}

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
        return complain("Eisdrache::Local::operator*(): Tried to get AllocaInst * of Value * ('"+v_ptr->getName().str()+"').");
    return a_ptr;
}

AllocaInst *Eisdrache::Local::getAllocaPtr() { return operator*(); }

Value *Eisdrache::Local::getValuePtr() { return v_ptr; }

bool Eisdrache::Local::isAlloca() { return isa<AllocaInst>(v_ptr); }

Eisdrache::Ty &Eisdrache::Local::getTy() { return type; }

/// EISDRACHE FUNC ///

Eisdrache::Func::Func() {
    func = nullptr;
    type = nullptr;
    locals = Local::Vec();
    eisdrache = nullptr;
}

Eisdrache::Func::Func(Eisdrache *eisdrache, Type *type, std::string name, ParamMap parameters, bool entry) {
    this->eisdrache = eisdrache;
    this->locals = Local::Vec();

    std::vector<std::string> paramNames;
    std::vector<Type *> paramTypes;
    for (ParamMap::value_type &param : parameters) {
        paramNames.push_back(param.first);
        paramTypes.push_back(param.second);
    }

    FunctionType *FT = FunctionType::get(type, paramTypes, false);
    func = Function::Create(FT, Function::ExternalLinkage, name, *eisdrache->getModule());

    for (size_t i = 0; i < func->arg_size(); i++)  
        func->getArg(i)->setName(paramNames[i]);

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
    locals = copy.locals;
    eisdrache = copy.eisdrache;
    return *this;
}

bool Eisdrache::Func::operator==(const Func &comp) const { return func == comp.func; }

bool Eisdrache::Func::operator==(const Function *comp) const { return func == comp; }

Type *Eisdrache::Func::operator[](Value *local) {
    if (isa<Argument>(local)) {
        for (Argument &arg : func->args())
            if (&arg == local) 
                return arg.getType();
        Eisdrache::complain("Eisdrache::Func::operator[](): Argument (Value) is not an existing Argument.");
    } else if (isa<AllocaInst>(local)) {
        for (Local &alloca : locals)
            if (alloca == local)
                return (*alloca)->getAllocatedType();
        Eisdrache::complain("Eisdrache::Func::operator[](): AllocaInst (Value) is not an existing local.");
    }  
    
    return Eisdrache::complain("Eisdrache::Func::operator[](): Value is not a Argument or AllocaInst");
}

Function *Eisdrache::Func::operator*() { return func; }

Argument *Eisdrache::Func::arg(size_t index) { return func->getArg(index); }

Value *Eisdrache::Func::call(ValueVec args, std::string name) { 
    return eisdrache->callFunction(func, args, name); 
}

Eisdrache::Local &Eisdrache::Func::addLocal(Local local) { 
    locals.push_back(local); 
    return locals.back();
}

/// EISDRACHE STRUCT ///

Eisdrache::Struct::Struct() {
    type = nullptr;
    ptr = nullptr;
    eisdrache = nullptr;
}

Eisdrache::Struct::Struct(Eisdrache *eisdrache, std::string name, TypeVec elements) {
    this->type = StructType::create(elements, name);
    this->ptr = PointerType::get(type, 0);
    this->eisdrache = eisdrache;
}

Eisdrache::Struct::~Struct() {}

Eisdrache::Struct &Eisdrache::Struct::operator=(const Struct &copy) {
    type = copy.type;
    ptr = copy.ptr;
    eisdrache = copy.eisdrache;
    return *this;
}

bool Eisdrache::Struct::operator==(const Struct &comp) const { return type == comp.type; }

bool Eisdrache::Struct::operator==(const Type *comp) const { return type == comp; }

Type *Eisdrache::Struct::operator[](size_t index) { return type->getElementType(index); }

StructType *Eisdrache::Struct::operator*() { return type; }

Eisdrache::Local &Eisdrache::Struct::allocate(std::string name) {
    return eisdrache->allocateStruct(*this, name);
} 

PointerType *Eisdrache::Struct::getPtr() { return ptr; }

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

Eisdrache::Func &Eisdrache::declareFunction(Type *type, std::string name, TypeVec parameters) {
    Func::ParamMap parsedParams = Func::ParamMap();
    for (Type *&param : parameters)
        parsedParams[std::to_string(parsedParams.size())] = param;
    functions[name] = Func(this, type, name, parsedParams);
    parent = &functions.at(name);
    return *parent;
}

Eisdrache::Func &Eisdrache::declareFunction(Type *type, std::string name, Func::ParamMap parameters, bool entry) {
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

Value *Eisdrache::callFunction(Function *func, ValueVec args, std::string name) {
    return builder->CreateCall(func, args, name);
}

Value *Eisdrache::callFunction(Func &wrap, ValueVec args, std::string name) { 
    return wrap.call(args, name);
}

Value *Eisdrache::callFunction(std::string callee, ValueVec args, std::string name) { 
    return functions.at(callee).call(args, name);
}
 
/// LOCALS ///

Eisdrache::Local &Eisdrache::declareLocal(Type *type, std::string name, Value *value) {
    AllocaInst *alloca = builder->CreateAlloca(type, nullptr, name);
    return parent->addLocal(Local(this, alloca, value)); // TODO: signed values
}

Value *Eisdrache::loadLocal(Value *local, std::string name) {
    assert(!"unimplemented");
    // TODO: reimplement this with Local
    // if (isa<AllocaInst>(local) && futures.contains((AllocaInst *) local))
        // builder->CreateStore(futures[(AllocaInst *)local], local);
    return builder->CreateLoad((*parent)[local], local, name);
}

/// STRUCT TYPES ///

Eisdrache::Struct &Eisdrache::declareStruct(std::string name, TypeVec elements) {
    structs[name] = Struct(this, name, elements);
    return structs.at(name);
}

Eisdrache::Local &Eisdrache::allocateStruct(Struct &wrap, std::string name) {
    AllocaInst *alloca = builder->CreateAlloca(*wrap, nullptr, name);
    return parent->addLocal(Local(this, alloca));
}

Eisdrache::Local &Eisdrache::allocateStruct(std::string typeName, std::string name) {
    AllocaInst *alloca = builder->CreateAlloca(*structs.at(typeName), nullptr, name);
    return parent->addLocal(Local(this, alloca));
}

/// BUILDER ///

ReturnInst *Eisdrache::createRet(BasicBlock *next) {
    ReturnInst *inst = builder->CreateRetVoid();
    if (next)
        builder->SetInsertPoint(next);
    return inst;
}

ReturnInst *Eisdrache::createRet(Value *value, BasicBlock *next) {
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