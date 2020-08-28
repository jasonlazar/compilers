#include "ast.hpp"

using llvm::Function;
using llvm::FunctionType;
using llvm::Value;
using llvm::LLVMContext;
using llvm::IRBuilder;
using llvm::Module;
using llvm::IntegerType;
using llvm::PointerType;
using llvm::ArrayType;
using llvm::Constant;
using llvm::GlobalVariable;
using llvm::GlobalValue;
using llvm::ConstantArray;
using llvm::BasicBlock;
using llvm::AllocaInst;
using llvm::GetElementPtrInst;
using llvm::outs;
using llvm::errs;

LLVMContext AST::TheContext;
IRBuilder<> AST::Builder(TheContext);
std::unique_ptr<Module> AST::TheModule;

Function *AST::ThePuti;
Function *AST::ThePutb;
Function *AST::ThePutc;
Function *AST::ThePuts;
Function *AST::TheGeti;
Function *AST::TheGetb;
Function *AST::TheGetc;
Function *AST::TheGets;
Function *AST::TheAbs;
Function *AST::TheOrd;
Function *AST::TheChr;
Function *AST::TheStrlen;
Function *AST::TheStrcmp;
Function *AST::TheStrcpy;
Function *AST::TheStrcat;

llvm::Type *AST::i1 = IntegerType::get(TheContext, 1);
llvm::Type *AST::i8 = IntegerType::get(TheContext, 8);
llvm::Type *AST::i16 = IntegerType::get(TheContext, 16);
llvm::Type *AST::i32 = IntegerType::get(TheContext, 32);
llvm::Type *AST::i64 = IntegerType::get(TheContext, 64);

llvm::Type* AST::translate(Type t) {
	switch(t->kind){
		case TYPE_INTEGER:
			return llvm::Type::getInt16Ty(AST::TheContext);
		case TYPE_CHAR:
			return llvm::Type::getInt8Ty(AST::TheContext);
		case TYPE_BOOLEAN:
			return llvm::Type::getInt8Ty(AST::TheContext);
		case TYPE_VOID:
			return llvm::Type::getVoidTy(AST::TheContext);
		case TYPE_IARRAY:
			return PointerType::get(translate(t->refType), 0);
		default:
			return nullptr;
	}
}

Value *AST::loadValue(Value *p)
{
	if (PointerType::classof(p->getType()))
		return Builder.CreateLoad(p, "var");
	else
		return p;
}

void AST::llvm_compile_and_dump() {
	// Initialize
	// Make the module, which holds all the code
	TheModule = llvm::make_unique<Module>("Tony program", TheContext);


	// Initialize global variables

	// Initialize library functions
	// stdio functions
	// puti
	FunctionType* puti_type =
		FunctionType::get(llvm::Type::getVoidTy(TheContext), {i16}, false);
	ThePuti =
		Function::Create(puti_type, Function::ExternalLinkage, "puti", TheModule.get());

	// putb
	FunctionType* putb_type =
		FunctionType::get(llvm::Type::getVoidTy(TheContext), {i8}, false);
	ThePutb =
		Function::Create(putb_type, Function::ExternalLinkage, "putb", TheModule.get());

	// putc
	FunctionType* putc_type =
		FunctionType::get(llvm::Type::getVoidTy(TheContext), {i8}, false);
	ThePutc =
		Function::Create(putc_type, Function::ExternalLinkage, "putc", TheModule.get());

	// puts
	FunctionType* puts_type =
		FunctionType::get(llvm::Type::getVoidTy(TheContext), {PointerType::get(i8, 0)}, false);
	ThePuts =
		Function::Create(puts_type, Function::ExternalLinkage, "puts", TheModule.get());

	// geti
	FunctionType* geti_type =
		FunctionType::get(llvm::Type::getInt16Ty(TheContext), {}, false);
	TheGeti =
		Function::Create(geti_type, Function::ExternalLinkage, "geti", TheModule.get());

	// getb
	FunctionType* getb_type =
		FunctionType::get(llvm::Type::getInt8Ty(TheContext), {}, false);
	TheGetb =
		Function::Create(getb_type, Function::ExternalLinkage, "getb", TheModule.get());

	// getc
	FunctionType* getc_type =
		FunctionType::get(llvm::Type::getInt8Ty(TheContext), {}, false);
	TheGetc =
		Function::Create(getc_type, Function::ExternalLinkage, "getc", TheModule.get());

	// gets
	FunctionType* gets_type =
		FunctionType::get(llvm::Type::getVoidTy(TheContext), {i16, PointerType::get(i8, 0)}, false);
	TheGets =
		Function::Create(gets_type, Function::ExternalLinkage, "gets", TheModule.get());

	// math functions
	// abs
	FunctionType* abs_type =
		FunctionType::get(llvm::Type::getInt16Ty(TheContext), {i16}, false);
	TheAbs =
		Function::Create(abs_type, Function::ExternalLinkage, "abs", TheModule.get());

	// stdlib functions
	// ord
	FunctionType* ord_type =
		FunctionType::get(llvm::Type::getInt16Ty(TheContext), {i8}, false);
	TheOrd =
		Function::Create(ord_type, Function::ExternalLinkage, "ord", TheModule.get());

	// chr
	FunctionType* chr_type =
		FunctionType::get(llvm::Type::getInt8Ty(TheContext), {i16}, false);
	TheChr =
		Function::Create(chr_type, Function::ExternalLinkage, "chr", TheModule.get());

	// string functions
	// strlen
	FunctionType* strlen_type =
		FunctionType::get(llvm::Type::getInt16Ty(TheContext), {PointerType::getUnqual(i8)}, false);
	TheStrlen =
		Function::Create(strlen_type, Function::ExternalLinkage, "strlen", TheModule.get());

	// strcmp
	FunctionType* strcmp_type =
		FunctionType::get(llvm::Type::getInt16Ty(TheContext), {PointerType::getUnqual(i8), PointerType::getUnqual(i8)}, false);
	TheStrcmp =
		Function::Create(strcmp_type, Function::ExternalLinkage, "strcmp", TheModule.get());

	// strcpy
	FunctionType* strcpy_type =
		FunctionType::get(llvm::Type::getVoidTy(TheContext), {PointerType::getUnqual(i8), PointerType::getUnqual(i8)}, false);
	TheStrcpy =
		Function::Create(strcpy_type, Function::ExternalLinkage, "strcpy", TheModule.get());

	// strcat
	FunctionType* strcat_type =
		FunctionType::get(llvm::Type::getVoidTy(TheContext), {PointerType::getUnqual(i8), PointerType::getUnqual(i8)}, false);
	TheStrcat =
		Function::Create(strcat_type, Function::ExternalLinkage, "strcat", TheModule.get());


	// Emit the program code.
	Value* main_func = compile();
	if (main_func->getName() != "main") {
		auto fun = TheModule->getFunction("main");
		main_func = TheModule->getFunction(main_func->getName());
		if (fun != NULL)
			fun->setName("_main");
		main_func->setName("main");
	}

	// Verify the IR.
	bool bad = llvm::verifyModule(*TheModule, &errs());
	if (bad) {
		std::cerr << "The IR is bad!" << std::endl;
		TheModule->print(errs(), nullptr);
		std::exit(1);
	}

	// Print out the IR.
	TheModule->print(outs(), nullptr);

}

Function* Header::compile() const {
	SymbolEntry* func = newFunction(id.c_str());
	if (!is_def)
		forwardFunction(func);
	openScope();
	currentScope->returnType = type;
	for (Formal* f : formal_list) {
		for (std::string id : f->getIdList()) {
			PassMode passmode = f->getRef() ? PASS_BY_REFERENCE : PASS_BY_VALUE;
			newParameter(id.c_str(), f->getType(), passmode, func);
		}
	}

	endFunctionHeader(func, type);

	Function *TheFunction = TheModule->getFunction(id);

	if (!TheFunction) {
		std::vector<llvm::Type*> types;
		for (Formal* f : formal_list) {
			size_t formal_size = f->getIdList().size();
			llvm::Type* formal_type = translate(f->getType());
			llvm::Type* ft = (f->getRef()) ? PointerType::getUnqual(formal_type) : formal_type;
			for (size_t i=0; i<formal_size; ++i)
				types.push_back(ft);
		}
		FunctionType* FT = FunctionType::get(translate(type), types, false);
		TheFunction = Function::Create(FT, Function::ExternalLinkage, id, TheModule.get());

		// Set names for all arguments
		auto it = TheFunction->arg_begin();
		for (Formal* f : formal_list) {
			for (std::string id : f->getIdList()) {
				it->setName(id);
				++it;
			}
		}
	}
	return TheFunction;
}

Function* FunctionDef::compile() const {
	Function* TheFunction = header->compile();

	if (!TheFunction)
		return nullptr;

	BasicBlock *BB = BasicBlock::Create(TheContext, "entry", TheFunction);
	Builder.SetInsertPoint(BB);

	for (auto &Arg : TheFunction->args()) {
		// Create an alloca for this variable.
		AllocaInst *Alloca = Builder.CreateAlloca(Arg.getType(), nullptr, Arg.getName());

		// Store the initial value into the alloca.
		Builder.CreateStore(&Arg, Alloca);

		// Add arguments to variable symbol table.
		SymbolEntry *se = lookupEntry(Arg.getName().str().c_str(), LOOKUP_ALL_SCOPES, true);
		se->alloca = Alloca;
	}

	for (Decl* d : decl_list) {
		Builder.SetInsertPoint(BB);
		d->compile();
	}

	Builder.SetInsertPoint(BB);

	for (Stmt* s : stmt_list) {
		s->compile();
    // Needs checking for one return statement in a block
	}

  if (!Builder.GetInsertBlock()->getTerminator()) {
    switch(header->getType()->kind){
  		case TYPE_INTEGER:
        // Builder.CreateRet(c16(0));
        fatal("In function %s, control reaches end of non-void function", header->getName().c_str());
        break;
  		case TYPE_CHAR:
        // Builder.CreateRet(c8(0));
        fatal("In function %s, control reaches end of non-void function", header->getName().c_str());
        break;
  		case TYPE_BOOLEAN:
        // Builder.CreateRet(c8(0));
        fatal("In function %s, control reaches end of non-void function", header->getName().c_str());
        break;
  		case TYPE_VOID:
        Builder.CreateRetVoid();
        break;
  		default:
        // Builder.CreateRet(llvm::ConstantPointerNull);
        fatal("In function %s, control reaches end of non-void function", header->getName().c_str());
  	}
  }

	closeScope();
	verifyFunction(*TheFunction, &errs());
	return TheFunction;
}

Function* FunctionDecl::compile() const {
	Function* TheFunction = header->compile();

	closeScope();
	if (!TheFunction)
		return nullptr;

	return TheFunction;
}

Value* VarDef::compile() const {
	for (std::string id : id_list) {
		SymbolEntry* se = newVariable(id.c_str(), type);
		AllocaInst *alloc = Builder.CreateAlloca(translate(type), nullptr, id);
		se->alloca = (void *) alloc;
	}
	return nullptr;
}

Value* Skip::compile() const {
  return nullptr;
}


Value* Assign::compile() const {
  if(Builder.GetInsertBlock()->getTerminator())
    return nullptr;

	Value* l = lval->compile();
	Value* r = rval->compile();
	if (rval->isString())
    Builder.CreateStore(r, l);
	return Builder.CreateStore(loadValue(r), l);
}

Value* Call::compile() const {
  if(isStmt) {
    if(Builder.GetInsertBlock()->getTerminator())
      return nullptr;
  }

	Function *CalleeF = TheModule->getFunction(name);

	SymbolEntry* e = lookupEntry(name.c_str(), LOOKUP_ALL_SCOPES, true);
	SymbolEntry* args = e->u.eFunction.firstArgument;
	// Iterate for each parameter
	std::vector<llvm::Value*> ArgsV;
	for (unsigned i = 0, e = parameters.size(); i != e; ++i) {
		if (args->u.eParameter.mode == PASS_BY_REFERENCE)
			ArgsV.push_back(parameters[i]->compile());
		else if (parameters[i]->isString())
			ArgsV.push_back(parameters[i]->compile());
		else
			ArgsV.push_back(loadValue(parameters[i]->compile()));
		args = args->u.eParameter.next;
		if (!ArgsV.back())
			return nullptr;
	}

	// return Builder.CreateCall(CalleeF, ArgsV, "calltmp");
	return Builder.CreateCall(CalleeF, ArgsV);
}

Value* Return::compile() const {
  if(Builder.GetInsertBlock()->getTerminator())
    return nullptr;

  Value* ret = loadValue(expr->compile());
  return Builder.CreateRet(ret);
}

Value* Exit::compile() const {
  if(Builder.GetInsertBlock()->getTerminator())
    return nullptr;

  return Builder.CreateRetVoid();
}

Value* Id::compile() const {
	SymbolEntry* e = lookupEntry(id.c_str(), LOOKUP_ALL_SCOPES, true);

	if (e->entryType == ENTRY_PARAMETER && e->u.eParameter.mode == PASS_BY_REFERENCE)
		return loadValue((AllocaInst*) e->alloca);

	return (AllocaInst *) e->alloca;
}

Value* ConstString::compile() const {
	std::vector<Constant*> values;
	for (char c : mystring)
		values.push_back(c8(c));
	values.push_back(c8('\0'));

	ArrayType* string_type = ArrayType::get(i8, values.size());
	GlobalVariable* TheString =
		new GlobalVariable(*TheModule, string_type, true, GlobalValue::InternalLinkage,
				ConstantArray::get(string_type, values), "string_constant");

	return GetElementPtrInst::CreateInBounds(string_type, TheString, {c32(0), c32(0)}, "str_ptr", Builder.GetInsertBlock());
}

Value* ConstInt::compile() const {
	return c16(num);
}

Value* ConstChar::compile() const {
	return c8(mychar);
}

Value* ConstBool::compile() const {
	return c8(boolean);
}

Value* UnOp::compile() const {
	Value* V = expr->compile();
	switch(op) {
		case UPLUS:
			return V;
		case UMINUS:
			return Builder.CreateNeg(V, "uminustmp");
		case NOT:
			return Builder.CreateNot(V, "nottmp");
		default:
			return nullptr;
	}
}


Value* BinOp::compile() const {
	Value* l = left->compile();
	Value* r = right->compile();

	l = loadValue(l);
	r = loadValue(r);

	Value* V;

	switch(op) {
		case PLUS:
			return Builder.CreateAdd(l, r, "addtmp");
		case MINUS:
			return Builder.CreateSub(l, r, "subtmp");
		case TIMES:
			return Builder.CreateMul(l, r, "multmp");
		case DIV:
			return Builder.CreateSDiv(l, r, "divtmp");
		case MOD:
			return Builder.CreateSRem(l, r, "modtmp");
		case EQ:
			V = Builder.CreateICmpEQ(l, r, "eqtmp");
			return Builder.CreateZExt(V, i8, "ext");
		case NEQ:
			V = Builder.CreateICmpNE(l, r, "neqtmp");
			return Builder.CreateZExt(V, i8, "ext");
		case GREATER:
			V = Builder.CreateICmpSGT(l, r, "gttmp");
			return Builder.CreateZExt(V, i8, "ext");
		case LESS:
			V = Builder.CreateICmpSLT(l, r, "lttmp");
			return Builder.CreateZExt(V, i8, "ext");
		case GEQ:
			V = Builder.CreateICmpSGE(l, r, "geqtmp");
			return Builder.CreateZExt(V, i8, "ext");
		case LEQ:
			V = Builder.CreateICmpSLE(l, r, "leqtmp");
			return Builder.CreateZExt(V, i8, "ext");
		case AND:
			return Builder.CreateAnd(l, r, "andtmp");
		case OR:
			return Builder.CreateOr(l, r, "ortmp");
		default:
			return nullptr;
	}
}
