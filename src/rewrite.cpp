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

#include "rewrite.hpp"

namespace llvm {

/// EISDRACHE FUNC ///

Eisdrache::Func::Func() {
    func = nullptr;
    type = nullptr;
    locals = LocalVec();
}

Eisdrache::Func::Func(Module *module, IRBuilder<> *builder, Type *type, std::string name, ParamMap parameters, bool entry) {
    this->locals = LocalVec();

    std::vector<std::string> paramNames;
    std::vector<Type *> paramTypes;
    for (ParamMap::value_type &param : parameters) {
        paramNames.push_back(param.first);
        paramTypes.push_back(param.second);
    }

    FunctionType *FT = FunctionType::get(type, paramTypes, false);
    func = Function::Create(FT, Function::ExternalLinkage, name, *module);

    for (size_t i = 0; i < func->arg_size(); i++)  
        func->getArg(i)->setName(paramNames[i]);

    if (entry) {
        BasicBlock *entry = BasicBlock::Create(builder->getContext(), "entry", func);
        builder->SetInsertPoint(entry);
    } else
        llvm::verifyFunction(*func);
}

Eisdrache::Func::~Func() { locals.clear(); }

Eisdrache::Func &Eisdrache::Func::operator=(const Func &copy) {
    func = copy.func;
    type = copy.type;
    locals = copy.locals;
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
        for (AllocaInst *&alloca : locals)
            if (alloca == local)
                return alloca->getAllocatedType();
        Eisdrache::complain("Eisdrache::Func::operator[](): AllocaInst (Value) is not an existing local.");
    }  
    
    return Eisdrache::complain("Eisdrache::Func::operator[](): Value is not a Argument or AllocaInst");
}

Argument *Eisdrache::Func::arg(size_t index) { return func->getArg(index); }

Value *Eisdrache::Func::call(IRBuilder<> *builder, ValueVec args, std::string name) { 
    return builder->CreateCall(func, args, name); 
}

/// EISDRACHE STRUCT ///

Eisdrache::Struct::Struct() {
    type = nullptr;
    ptr = nullptr;
}

Eisdrache::Struct::Struct(Module *module, IRBuilder<> *builder, std::string name, TypeVec elements) {
    this->type = StructType::create(elements, name);
    this->ptr = PointerType::get(type, 0);
}

Eisdrache::Struct::~Struct() {}

Eisdrache::Struct &Eisdrache::Struct::operator=(const Struct &copy) {
    type = copy.type;
    ptr = copy.ptr;
    return *this;
}

bool Eisdrache::Struct::operator==(const Struct &comp) const { return type == comp.type; }
bool Eisdrache::Struct::operator==(const Type *comp) const { return type == comp; }
Type *Eisdrache::Struct::operator[](size_t index) { return type->getElementType(index); }

AllocaInst *Eisdrache::Struct::allocate(IRBuilder<> *builder, std::string name) {
    return builder->CreateAlloca(type, nullptr, name);
} 

/// EISDRACHE WRAPPER ///w

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

Type *Eisdrache::getVoidTy() { return builder->getVoidTy(); }
IntegerType *Eisdrache::getBoolTy() { return builder->getInt1Ty(); }
IntegerType *Eisdrache::getSizeTy() { return builder->getInt64Ty(); }
IntegerType *Eisdrache::getIntTy(size_t bit) { return builder->getIntNTy(bit); }
PointerType *Eisdrache::getIntPtrTy(size_t bit) { return Type::getIntNPtrTy(*context, bit); }
PointerType *Eisdrache::getIntPtrPtrTy(size_t bit) { return PointerType::get(getIntPtrTy(bit), 0); }
Type *Eisdrache::getFloatTy(size_t bit) {
    switch (bit) {
        case 16:    return builder->getHalfTy();
        case 32:    return builder->getFloatTy();
        case 64:    return builder->getDoubleTy();
        default:    return complain("Invalid amount of bits for floating point type.");
    }
}
Type *Eisdrache::getFloatPtrTy(size_t bit) { return PointerType::get(getFloatTy(bit), 0); }
Type *Eisdrache::getFloatPtrPtrTy(size_t bit) { return PointerType::get(getFloatPtrTy(bit), 0); }

Type *Eisdrache::getElementTy(Value *value) {
    if (isa<AllocaInst>(value)) 
        return ((AllocaInst*) value)->getAllocatedType();
    
    if (isa<Constant>(value))
        return value->getType();
    
    if (isa<GlobalValue>(value))
        return ((GlobalValue *) value)->getValueType();
    
    return getLoadStoreType(value);
}

/// VALUES ///

ConstantInt *Eisdrache::getBool(bool value) { return builder->getInt1(value); }
ConstantInt *Eisdrache::getInt(size_t bit, size_t value) { return builder->getIntN(bit, value); }
ConstantFP *Eisdrache::getFloat(double value) { return ConstantFP::get(*context, APFloat(value)); }
Constant *Eisdrache::getLiteral(std::string value, std::string name) { return builder->CreateGlobalStringPtr(value, name); }

/// FUNCTIONS ///

Eisdrache::Func &Eisdrache::declareFunction(Type *type, std::string name, TypeVec parameters) {
    Func::ParamMap parsedParams = Func::ParamMap();
    for (Type *&param : parameters)
        parsedParams[std::to_string(parsedParams.size())] = param;
    functions[name] = Func(module, builder, type, name, parsedParams);
    return functions.at(name);
}

Eisdrache::Func &Eisdrache::declareFunction(Type *type, std::string name, Func::ParamMap parameters, bool entry) {
    functions[name] = Func(module, builder, type, name, parameters, entry);
    return functions.at(name);
}

Eisdrache::Func &Eisdrache::getWrap(Function *function) {
    for (Func::Map::value_type &wrap : functions)
        if (wrap.second == function)
            return wrap.second;
    complain("Could not find Eisdrache::Func of @" + function->getName().str() + "().");
    return functions.end()->second;
}

bool Eisdrache::verifyFunc(Func &wrap) { 
    return llvm::verifyFunction(*wrap.func); 
}

Value *Eisdrache::callFunction(Function *func, ValueVec args, std::string name) {
    return builder->CreateCall(func, args, name);
}

Value *Eisdrache::callFunction(Func &wrap, ValueVec args, std::string name) { 
    return wrap.call(builder, args, name);
}

Value *Eisdrache::callFunction(std::string callee, ValueVec args, std::string name) { 
    return functions.at(callee).call(builder, args, name);
}
 
/// LOCALS ///

AllocaInst *Eisdrache::declareLocal(Type *type, std::string name, Value *value) {
    AllocaInst *alloca = builder->CreateAlloca(type, nullptr, name);
    futures[alloca] = value;
    getWrap(builder->GetInsertBlock()->getParent()).locals.push_back(alloca);
    return alloca;
}

/// MEMORY ///

Value *Eisdrache::callMalloc(Type *type, Value *size, std::string name) { return nullptr; }
void Eisdrache::callFree(Type *type, Value *pointer) { return; }
Value *Eisdrache::callMemcpy(Type *type, Value *dest, Value *source, Value *size, std::string name) { return nullptr; }


/// STRUCT TYPES ///

Eisdrache::Struct &Eisdrache::declareStruct(std::string name, TypeVec elements) {
    structs[name] = Struct(module, builder, name, elements);
    return structs.at(name);
}

AllocaInst *Eisdrache::allocateStruct(Struct &wrap, std::string name) {
    AllocaInst *alloca = wrap.allocate(builder, name);
    getWrap(builder->GetInsertBlock()->getParent()).locals.push_back(alloca);
    return alloca;
}

AllocaInst *Eisdrache::allocateStruct(std::string typeName, std::string name) {
    AllocaInst *alloca = structs.at(typeName).allocate(builder, name);
    getWrap(builder->GetInsertBlock()->getParent()).locals.push_back(alloca);
    return alloca;
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

/// GETTER ///

LLVMContext *Eisdrache::getContext() { return context; }
Module *Eisdrache::getModule() { return module; }
IRBuilder<> *Eisdrache::getBuilder() { return builder; }

/// PRIVATE ///

Eisdrache::Eisdrache(LLVMContext *context, Module *module, IRBuilder<> *builder, std::string targetTriple) {
    this->context = context;
    this->module = module;
    this->builder = builder;
    functions = Func::Map();
    structs = Struct::Map();
    futures = FutureMap();
    memoryFunctions = MemoryFuncMap();

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