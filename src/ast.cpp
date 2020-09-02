#include "ast.hpp"

#include <map>
#include <utility>

using llvm::Function;
using llvm::FunctionType;
using llvm::Value;
using llvm::LLVMContext;
using llvm::IRBuilder;
using llvm::Module;
using llvm::IntegerType;
using llvm::PointerType;
using llvm::ArrayType;
using llvm::StructType;
using llvm::GlobalVariable;
using llvm::GlobalValue;
using llvm::Constant;
using llvm::ConstantArray;
using llvm::ConstantPointerNull;
using llvm::BasicBlock;
using llvm::AllocaInst;
using llvm::LoadInst;
using llvm::GetElementPtrInst;
using llvm::outs;
using llvm::errs;

std::string AST::prepend = "-";

std::map<Function*, std::vector<Value*> *> inherited;

LLVMContext AST::TheContext;
IRBuilder<> AST::Builder(TheContext);
std::unique_ptr<Module> AST::TheModule;
std::unique_ptr<llvm::legacy::FunctionPassManager> AST::TheFPM;

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
Function *AST::TheInit;
Function *AST::TheMalloc;

llvm::Type *AST::i1 = IntegerType::get(TheContext, 1);
llvm::Type *AST::i8 = IntegerType::get(TheContext, 8);
llvm::Type *AST::i16 = IntegerType::get(TheContext, 16);
llvm::Type *AST::i32 = IntegerType::get(TheContext, 32);
llvm::Type *AST::i64 = IntegerType::get(TheContext, 64);
PointerType *AST::ListType;

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
		case TYPE_LIST:
			return ListType;
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

void AST::addLibFunctionPointers() {
	SymbolEntry* se;
	se = lookupEntry("puti", LOOKUP_ALL_SCOPES, true);
	se->alloca = (void *) ThePuti;
	se = lookupEntry("putb", LOOKUP_ALL_SCOPES, true);
	se->alloca = (void *) ThePutb;
	se = lookupEntry("putc", LOOKUP_ALL_SCOPES, true);
	se->alloca = (void *) ThePutc;
	se = lookupEntry("puts", LOOKUP_ALL_SCOPES, true);
	se->alloca = (void *) ThePuts;
	se = lookupEntry("geti", LOOKUP_ALL_SCOPES, true);
	se->alloca = (void *) TheGeti;
	se = lookupEntry("getb", LOOKUP_ALL_SCOPES, true);
	se->alloca = (void *) TheGetb;
	se = lookupEntry("getc", LOOKUP_ALL_SCOPES, true);
	se->alloca = (void *) TheGetc;
	se = lookupEntry("gets", LOOKUP_ALL_SCOPES, true);
	se->alloca = (void *) TheGets;
	se = lookupEntry("abs", LOOKUP_ALL_SCOPES, true);
	se->alloca = (void *) TheAbs;
	se = lookupEntry("ord", LOOKUP_ALL_SCOPES, true);
	se->alloca = (void *) TheOrd;
	se = lookupEntry("chr", LOOKUP_ALL_SCOPES, true);
	se->alloca = (void *) TheChr;
	se = lookupEntry("strlen", LOOKUP_ALL_SCOPES, true);
	se->alloca = (void *) TheStrlen;
	se = lookupEntry("strcmp", LOOKUP_ALL_SCOPES, true);
	se->alloca = (void *) TheStrcmp;
	se = lookupEntry("strcpy", LOOKUP_ALL_SCOPES, true);
	se->alloca = (void *) TheStrcpy;
	se = lookupEntry("strcat", LOOKUP_ALL_SCOPES, true);
	se->alloca = (void *) TheStrcat;
}

void AST::llvm_compile_and_dump(bool optimize) {
	// Initialize
	// Make the module, which holds all the code
	TheModule = llvm::make_unique<Module>("Tony program", TheContext);
	// Optimizations
	TheFPM = llvm::make_unique<llvm::legacy::FunctionPassManager>(TheModule.get());
	if (optimize) {
		TheFPM->add(llvm::createPromoteMemoryToRegisterPass());
		TheFPM->add(llvm::createInstructionCombiningPass());
		TheFPM->add(llvm::createReassociatePass());
		TheFPM->add(llvm::createGVNPass());
		TheFPM->add(llvm::createCFGSimplificationPass());
	}
	TheFPM->doInitialization();


	// Initialize Types
	StructType *NodeType = StructType::create(TheContext, "nodetype");
	NodeType->setBody({i64, PointerType::get(NodeType, 0)});
	ListType = PointerType::get(NodeType, 0);

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

	// garbage collector
	FunctionType *malloc_type =
		FunctionType::get(PointerType::get(i8, 0), {i64}, false);
	TheMalloc =
		Function::Create(malloc_type, Function::ExternalLinkage, "GC_malloc", TheModule.get());
	FunctionType *init_type =
		FunctionType::get(llvm::Type::getVoidTy(TheContext), {}, false);
	TheInit =
		Function::Create(init_type, Function::ExternalLinkage, "GC_init", TheModule.get());

	addLibFunctionPointers();

	// Emit the program code.
	Value* main_func = compile();
	auto fun = TheModule->getFunction("main");
	main_func = TheModule->getFunction(main_func->getName());
	if (fun != NULL)
		fun->setName("_main");

	FunctionType *main_type = FunctionType::get(i32, {}, false);
	Function *main =
		Function::Create(main_type, Function::ExternalLinkage, "main", TheModule.get());
	BasicBlock *BB = BasicBlock::Create(TheContext, "entry", main);
	Builder.SetInsertPoint(BB);
	Builder.CreateCall(TheInit, {});
	Builder.CreateCall(main_func, {});
	Builder.CreateRet(c32(0));

	// Verify the IR.
	bool bad = llvm::verifyModule(*TheModule, &errs());
	if (bad) {
		std::cerr << "The IR is bad!" << std::endl;
		TheModule->print(errs(), nullptr);
		std::exit(1);
	}

	// Optimize!
	TheFPM->run(*main);

	// Print out the IR.
	// TheModule->print(outs(), nullptr);

}

void Header::sem() {
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

	if (is_main) {
		if (type->kind != TYPE_VOID) fatal("Main function %s shouldn't have return type", id.c_str());
		if (formal_list.size() != 0) fatal("Main function %s shouldn't take any arguments",  id.c_str());
	}
}

Function* Header::compile() const {
	SymbolEntry* fun = lookupEntry(id.c_str(), LOOKUP_ALL_SCOPES, true);
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

	Function *TheFunction = TheModule->getFunction(id);

	std::string new_id = id;
	if (fun != nullptr) {
		if (fun->entryType == ENTRY_FUNCTION && fun->u.eFunction.isForward
				&& fun->nestingLevel == currentScope->nestingLevel) 
			return nullptr;
	}
	else if (TheFunction) {
		new_id = prepend + id;
		prepend += "-";
	}
	std::vector<llvm::Type*> types;
	for (Formal* f : formal_list) {
		size_t formal_size = f->getIdList().size();
		llvm::Type* formal_type = translate(f->getType());
		llvm::Type* ft = (f->getRef()) ? PointerType::getUnqual(formal_type) : formal_type;
		for (size_t i=0; i<formal_size; ++i)
			types.push_back(ft);
	}
	std::vector<Value*> *inh = new std::vector<Value *>;
	std::vector<std::string> names;
	Scope* scp = currentScope->parent;
	if (scp != nullptr) {
		SymbolEntry* e = scp->entries;

		while (e != nullptr) {
			if (lookupEntry(e->id, LOOKUP_ALL_SCOPES, true) == e) {
				switch(e->entryType) {
					case ENTRY_VARIABLE:
						newParameter(e->id, e->u.eVariable.type, PASS_BY_REFERENCE, func);
						types.push_back(PointerType::getUnqual(translate(e->u.eVariable.type)));
						inh->push_back((AllocaInst *) e->alloca);
						names.push_back(e->id);
						break;
					case ENTRY_PARAMETER:
						newParameter(e->id, e->u.eParameter.type, PASS_BY_REFERENCE, func);
						types.push_back(PointerType::getUnqual(translate(e->u.eVariable.type)));
						if (e->u.eParameter.mode == PASS_BY_VALUE)
							inh->push_back((AllocaInst *) e->alloca);
						else
							inh->push_back(loadValue((AllocaInst *) e->alloca));
						names.push_back(e->id);
						break;
					default:
						break;
				}
			}
			e = e->nextInScope;
		}
	}
	FunctionType* FT = FunctionType::get(translate(type), types, false);
	TheFunction = Function::Create(FT, Function::ExternalLinkage, new_id, TheModule.get());

	func->alloca = (void *) TheFunction;
	inherited.insert(std::make_pair(TheFunction, inh));

	// Set names for all arguments
	auto it = TheFunction->arg_begin();
	for (Formal* f : formal_list) {
		for (std::string id : f->getIdList()) {
			it->setName(id);
			++it;
		}
	}
	for (std::string name : names) {
		it->setName(name);
		++it;
	}

	endFunctionHeader(func, type);
	return TheFunction;
}

void FunctionDef::sem() {
	header->sem();
	for (Decl *d : decl_list) {
		d->sem();
	}
	for (Stmt *s : stmt_list) {
		s->sem();
	}

	//		printSymbolTable();

	closeScope();
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

	BasicBlock* InsertBB = Builder.GetInsertBlock();

	if (!InsertBB->getTerminator()) {
		if(header->getType()->kind == TYPE_VOID)
			Builder.CreateRetVoid();
		else {		
			warning("In function %s, control may reach end of non-void function", header->getName().c_str());
			if (!InsertBB->getFirstNonPHI() && (pred_begin(InsertBB) == pred_end(InsertBB)))
				InsertBB->eraseFromParent();
			else {
				switch(header->getType()->kind) {
					case TYPE_INTEGER:
						Builder.CreateRet(c16(0));
						break;
					case TYPE_CHAR:
						Builder.CreateRet(c8(0));
						break;
					case TYPE_BOOLEAN:
						Builder.CreateRet(c8(0));
						break;
					case TYPE_IARRAY:
						Builder.CreateRet(Constant::getNullValue(translate(header->getType()->refType)));
						break;
					case TYPE_LIST:
						Builder.CreateRet(ConstantPointerNull::get(ListType));
						break;
					default:
						;
				}
			}
		}
	}

	closeScope();
	verifyFunction(*TheFunction, &errs());
	return TheFunction;
}

void FunctionDecl::sem() {
	header->unset_def();
	header->sem();

	// printSymbolTable();

	closeScope();
}

Function* FunctionDecl::compile() const {
	Function* TheFunction = header->compile();

	closeScope();
	if (!TheFunction)
		return nullptr;

	return TheFunction;
}

void VarDef::sem() {
	for (std::string id : id_list) {
		newVariable(id.c_str(), type);
	}
}

Value* VarDef::compile() const {
	for (std::string id : id_list) {
		SymbolEntry* se = lookupEntry(id.c_str(), LOOKUP_ALL_SCOPES, true);
		if (se == nullptr) {
			se = newVariable(id.c_str(), type);
			AllocaInst *alloc = Builder.CreateAlloca(translate(type), nullptr, id);
			se->alloca = (void *) alloc;
		}
		else if (se->nestingLevel != currentScope->nestingLevel) {
			se = newVariable(id.c_str(), type);
			AllocaInst *alloc = Builder.CreateAlloca(translate(type), nullptr, id);
			se->alloca = (void *) alloc;
		}
		else {
			strcpy((char *) se->id, "");
			se = newVariable(id.c_str(), type);
			AllocaInst *alloc = Builder.CreateAlloca(translate(type), nullptr, "_"+id);
			se->alloca = (void *) alloc;
		}
	}
	return nullptr;
}

Value* Skip::compile() const {
	return nullptr;
}

void Assign::sem() {
	if (!lval->isLval()) {
		std::stringstream atom;
		atom << *lval << " is not assignable (not an L-value)";
		fatal(atom.str().c_str());
	}
	lval->sem();
	if (lval->isCharOfString()) {
		std::stringstream atom;
		atom << *lval << " is a char of const string and can't be assigned";
		fatal(atom.str().c_str());
	}
	rval->sem();
	if (!equalType(lval->getType(), rval->getType()))
	{
		std::stringstream atom;
		atom << "In assignment " << *lval << ":=" << *rval << ":\n";
		atom << "left is of type: " << lval->getType() << " and right is of type: " << rval->getType();
		fatal(atom.str().c_str());
	}
}

Value* Assign::compile() const {
	if(Builder.GetInsertBlock()->getTerminator())
		return nullptr;

	Value* l = lval->compile();
	Value* r = rval->compile();
	if (rval->isLval())
		return Builder.CreateStore(loadValue(r), l);
	else
		return Builder.CreateStore(r, l);
}

void Call::sem() {
	SymbolEntry* e = lookupEntry(name.c_str(), LOOKUP_ALL_SCOPES, true);
	if (e == nullptr)
		fatal("Unknown identifier: %s", name.c_str());
	if (e->entryType != ENTRY_FUNCTION) {
		fatal("%s is not a Function", name.c_str());
	}
	SymbolEntry* args = e->u.eFunction.firstArgument;
	std::vector<Expr*>::iterator itr = parameters.begin();
	while (args != NULL) {
		if (itr == parameters.end())
			fatal("%s Function needs more parameters", name.c_str());

		(*itr)->sem();
		if (!equalType((*itr)->getType(), args->u.eParameter.type)) {
			std::stringstream expr;
			expr << "In Function call of %s: " << *(*itr) << " is of type:" << (*itr)->getType()
				<< " and expected type:" << args->u.eParameter.type;
			fatal(expr.str().c_str(), name.c_str());
		}

		if (args->u.eParameter.mode == PASS_BY_REFERENCE && !(*itr)->isLval()) {
			std::stringstream expr;
			expr << "In Function call of %s expected L-value, got " << *(*itr) << " instead";
			fatal(expr.str().c_str(), name.c_str());
		}

		args = args->u.eParameter.next;
		itr++;
	}

	if (itr != parameters.end())
		fatal("%s Function call has more parameters", name.c_str());

	if (isStmt && !equalType(e->u.eFunction.resultType, typeVoid)) {
		fatal("%s Function call is used as a statement but function is not void", name.c_str());
	}
	else if (!isStmt) {
		if (equalType(e->u.eFunction.resultType, typeVoid))
			fatal("%s Function call is used as an expression but function is void", name.c_str());
		type = e->u.eFunction.resultType;
	}


}

Value* Call::compile() const {
	if(isStmt) {
		if(Builder.GetInsertBlock()->getTerminator())
			return nullptr;
	}

	SymbolEntry* e = lookupEntry(name.c_str(), LOOKUP_ALL_SCOPES, true);
	Function* CalleeF = (Function *) e->alloca;
	SymbolEntry* args = e->u.eFunction.firstArgument;
	// Iterate for each parameter
	std::vector<Value*> ArgsV;
	for (unsigned i = 0, e = parameters.size(); i != e; ++i) {
		if (args->u.eParameter.mode == PASS_BY_REFERENCE)
			ArgsV.push_back(parameters[i]->compile());
		else if (parameters[i]->isString())
			ArgsV.push_back(parameters[i]->compile());
		else if (parameters[i]->isLval())
			ArgsV.push_back(loadValue(parameters[i]->compile()));
		else
			ArgsV.push_back(parameters[i]->compile());
		args = args->u.eParameter.next;
		if (!ArgsV.back())
			return nullptr;
	}

	std::vector<Value*> * inh = inherited.find(CalleeF)->second;
	ArgsV.insert(ArgsV.end(), inh->begin(), inh->end());
	return Builder.CreateCall(CalleeF, ArgsV);
}

void Exit::sem() {
	if (!equalType(currentScope->returnType, typeVoid))
		fatal("Exit statement in non Void Function");
}

Value* Exit::compile() const {
	if(Builder.GetInsertBlock()->getTerminator())
		return nullptr;

	return Builder.CreateRetVoid();
}

void Return::sem() {
	expr->sem();
	if (!equalType(currentScope->returnType, expr->getType())) {
		std::stringstream expr;
		expr << "Return statement: " << *this << " must return type " << currentScope->returnType;
		fatal(expr.str().c_str());
	}
}

Value* Return::compile() const {
	if(Builder.GetInsertBlock()->getTerminator())
		return nullptr;

	Value* ret = (expr->isLval())? loadValue(expr->compile()) : expr->compile();
	return Builder.CreateRet(ret);
}

void Elsif::sem() {
	cond->sem();
	if (!equalType(cond->getType(), typeBoolean)) {
		std::stringstream expr;
		expr << "In statement: " << *this << ", condition: " << *cond
			<< " must be Boolean";
		fatal(expr.str().c_str());
	}
	for (Stmt* st : stmt_list)
		st->sem();
}

Value* Elsif::compile() const {
	if(Builder.GetInsertBlock()->getTerminator())
		return nullptr;

	for (Stmt* st : stmt_list)
		st->compile();

	return nullptr;
}

void If::sem() {
	cond->sem();
	if (!equalType(cond->getType(), typeBoolean)) {
		std::stringstream expr;
		expr << "In statement: " << *this << ", condition: " << *cond
			<< " must be Boolean";
		fatal(expr.str().c_str());
	}
	for (Stmt* st : statements)
		st->sem();
	for (Elsif* e : elsif_list)
		e->sem();
	for (Stmt *st : else_statements)
		st->sem();
}

Value* If::compile() const {
	if(Builder.GetInsertBlock()->getTerminator())
		return nullptr;

	Value *condition = loadValue(cond->compile());
	Value *if_cond = Builder.CreateICmpNE(condition, c8(0), "if_cond");

	Function *TheFunction = Builder.GetInsertBlock()->getParent();

	BasicBlock *ThenBB =
		BasicBlock::Create(TheContext, "then", TheFunction);
	BasicBlock *ElseBB =
		BasicBlock::Create(TheContext, "else", TheFunction);
	BasicBlock *AfterBB =
		BasicBlock::Create(TheContext, "endif", TheFunction);

	Builder.CreateCondBr(if_cond, ThenBB, ElseBB);
	Builder.SetInsertPoint(ThenBB);

	for (Stmt* st : statements)
		st->compile();

	if(!Builder.GetInsertBlock()->getTerminator())
		Builder.CreateBr(AfterBB);
	Builder.SetInsertPoint(ElseBB);

	for (Elsif* e : elsif_list) {
		Expr* exp = e->getCond();
		Value* condition = loadValue(exp->compile());
		Value *elsif_cond = Builder.CreateICmpNE(condition, c8(0), "elsif_cond");
		BasicBlock *ElsifThenBB =
			BasicBlock::Create(TheContext, "elsif_then", TheFunction);
		BasicBlock *NextElsifBB =
			BasicBlock::Create(TheContext, "next_elsif", TheFunction);

		Builder.CreateCondBr(elsif_cond, ElsifThenBB, NextElsifBB);
		Builder.SetInsertPoint(ElsifThenBB);

		e->compile();

		if(!Builder.GetInsertBlock()->getTerminator())
			Builder.CreateBr(AfterBB);
		Builder.SetInsertPoint(NextElsifBB);
	}

	for (Stmt *st : else_statements)
		st->compile();

	if(!Builder.GetInsertBlock()->getTerminator())
		Builder.CreateBr(AfterBB);
	Builder.SetInsertPoint(AfterBB);

	return nullptr;
}

void For::sem() {
	for (Simple* s : init)
		s->sem();

	cond->sem();
	if (!equalType(cond->getType(), typeBoolean)) {
		std::stringstream expr;
		expr << "In statement: " << *this << ", condition: " << *cond
			<< " must be Boolean";
		fatal(expr.str().c_str());
	}
	for (Simple* s : after)
		s->sem();

	for (Stmt* st : stmt_list)
		st->sem();
}

Value* For::compile() const {
	if(Builder.GetInsertBlock()->getTerminator())
		return nullptr;

	BasicBlock *PrevBB = Builder.GetInsertBlock();
	Function *TheFunction = PrevBB->getParent();

	BasicBlock *LoopBB =
		BasicBlock::Create(TheContext, "loop", TheFunction);
	BasicBlock *BodyBB =
		BasicBlock::Create(TheContext, "body", TheFunction);
	BasicBlock *AfterLoopBB =
		BasicBlock::Create(TheContext, "endfor", TheFunction);

	for (Simple* s : init)
		s->compile();

	Builder.CreateBr(LoopBB);
	Builder.SetInsertPoint(LoopBB);
	Value* condition = loadValue(cond->compile());
	Value *loop_cond = Builder.CreateICmpSGT(condition, c8(0), "loop_cond");

	Builder.CreateCondBr(loop_cond, BodyBB, AfterLoopBB);
	Builder.SetInsertPoint(BodyBB);
	for (Stmt* st : stmt_list)
		st->compile();
	for (Simple* s : after)
		s->compile();

	Builder.CreateBr(LoopBB);
	Builder.SetInsertPoint(AfterLoopBB);

	return nullptr;
}

void Id::sem() {
	SymbolEntry* e = lookupEntry(id.c_str(), LOOKUP_ALL_SCOPES, true);
	if (e == nullptr)
		fatal("Unknown identifier: %s", id.c_str());
	switch (e->entryType) {
		case ENTRY_VARIABLE:
			type = e->u.eVariable.type;
			break;
		case ENTRY_PARAMETER:
			type = e->u.eParameter.type;
			break;
		default:
			fatal("%s is not a Variable or a Parameter", id.c_str());
	}
}

Value* Id::compile() const {
	SymbolEntry* e = lookupEntry(id.c_str(), LOOKUP_ALL_SCOPES, true);

	if (e->entryType == ENTRY_PARAMETER && e->u.eParameter.mode == PASS_BY_REFERENCE)
		return loadValue((AllocaInst*) e->alloca);

	return (AllocaInst *) e->alloca;
}

void ConstString::sem() {
	type = typeIArray(typeChar);
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

void ArrayItem::sem() {
	array->sem();
	Type t = array->getType();
	if (t->kind != TYPE_IARRAY) {
		std::stringstream atom;
		atom << *array << " is not an Array";
		fatal(atom.str().c_str());
	}
	type = t->refType;
}

Value* ArrayItem::compile() const {
	Value* arr = array->compile();
	Value* position = loadValue(pos->compile());
	Value* pos32 = Builder.CreateSExt(position, i32, "exttmp");
	LoadInst* ld = Builder.CreateLoad(arr, "ldtmp");
	return Builder.CreateInBoundsGEP(ld, pos32, "geptmp");
}

void ConstInt::sem() {
	type = typeInteger;
}

Value* ConstInt::compile() const {
	return c16(num);
}

void ConstChar::sem() {
	type = typeChar;
}

Value* ConstChar::compile() const {
	return c8(mychar);
}

void ConstBool::sem() {
	type = typeBoolean;
}

Value* ConstBool::compile() const {
	return c8(boolean);
}

void UnOp::sem() {
	expr->sem();

	switch (op) {
		case UPLUS:
		case UMINUS:
			if (!equalType(expr->getType(), typeInteger)) {
				std::stringstream expr_stream;
				expr_stream << "In expression:" << *this << ", " << *expr << " is not Integer";
				fatal(expr_stream.str().c_str());
			}
			type = typeInteger;
			break;
		case NOT:
			if (!equalType(expr->getType(), typeBoolean)) {
				std::stringstream expr_stream;
				expr_stream << "In expression: " << *this << ", " << *expr << " is not Boolean";
				fatal(expr_stream.str().c_str());
			}
			type = typeBoolean;
			break;
		case IS_NIL:
			if (!equalType(expr->getType(), typeList(typeAny))) {
				std::stringstream expr_stream;
				expr_stream << "In expression: " << *this << ", " << *expr << " is not List";
				fatal(expr_stream.str().c_str());
			}
			type = typeBoolean;
			break;
		case HEAD:
			if (!equalType(expr->getType(), typeList(typeAny))) {
				std::stringstream expr_stream;
				expr_stream << "In expression: " << *this << ", " << *expr << " is not List";
				fatal(expr_stream.str().c_str());
			}
			type = expr->getType()->refType;
			if (type->kind == TYPE_ANY)
				fatal("You cannot get the head of nil");
			break;
		case TAIL:
			if (!equalType(expr->getType(), typeList(typeAny))) {
				std::stringstream expr_stream;
				expr_stream << "In expression: " << *this << ", " << *expr << " is not List";
				fatal(expr_stream.str().c_str());
			}
			type = typeList(expr->getType()->refType);
			if (type->refType->kind == TYPE_ANY)
				fatal("You cannot get the tail of nil");
			break;
	}
}
Value* UnOp::compile() const {
	Value* V = expr->compile();
	if (expr->isLval())
		V = loadValue(V);
	switch(op) {
		case UPLUS:
			return V;
		case UMINUS:
			return Builder.CreateNeg(V, "uminustmp");
		case NOT:
			V = Builder.CreateICmpEQ(V, c8(0), "eqtmp");
			return Builder.CreateZExt(V, i8, "nottmp");
		case IS_NIL:
			V = Builder.CreatePtrToInt(V, i64, "listptr");
			V = Builder.CreateICmpEQ(V, c64(0), "eqtmp");
			return Builder.CreateZExt(V, i8, "isniltmp");
		case HEAD:
			V = Builder.CreateGEP(V, {c32(0), c32(0)}, "headptr");
			V = Builder.CreateLoad(V, "head");
			if (PointerType::classof(translate(expr->getType()->refType)))
				return Builder.CreateIntToPtr(V, translate(expr->getType()->refType), "inttoptrtmp");
			else
				return Builder.CreateTrunc(V, translate(expr->getType()->refType), "trunctmp");
		case TAIL:
			V = Builder.CreateGEP(V, {c32(0), c32(1)}, "tailptr");
			return Builder.CreateLoad(V, "tail");
		default:
			return nullptr;
	}
}

void BinOp::sem() {
	left->sem();
	right->sem();

	switch(op) {
		case PLUS:
		case MINUS:
		case TIMES:
		case DIV:
		case MOD:
			if (!equalType(left->getType(), typeInteger)) {
				std::stringstream expr_stream;
				expr_stream << "In expression: " << *this << ", " << *left << " is not an Integer";
				fatal(expr_stream.str().c_str());
			}
			if (!equalType(right->getType(), typeInteger)) {
				std::stringstream expr_stream;
				expr_stream << "In expression: " << *this << ", " << *right << " is not an Integer";
				fatal(expr_stream.str().c_str());
			}
			type = typeInteger;
			break;
		case EQ:
		case NEQ:
		case GREATER:
		case LESS:
		case GEQ:
		case LEQ:
			if (!equalType(left->getType(), right->getType())) {
				std::stringstream expr_stream;
				expr_stream << "In expression: " << *this << ", " << *left << " and " << *right << " are not the same type";
				fatal(expr_stream.str().c_str());
			}
			if (equalType(left->getType(), typeIArray(typeAny)))
				fatal("You cannot compare arrays");
			else if (equalType(left->getType(), typeList(typeAny)))
				fatal("You cannot compare lists");
			type = typeBoolean;
			break;
		case AND:
		case OR:
			if (!equalType(right->getType(), typeBoolean)) {
				std::stringstream expr_stream;
				expr_stream << "In expression: " << *this << ", " << *right << " is not a Boolean";
				fatal(expr_stream.str().c_str());
			}
			if (!equalType(left->getType(), typeBoolean)) {
				std::stringstream expr_stream;
				expr_stream << "In expression: " << *this << ", " << *left << " is not a Boolean";
				fatal(expr_stream.str().c_str());
			}
			type = typeBoolean;
			break;
		case CONS:
			if (!equalType(right->getType(), typeList(left->getType()))) {
				std::stringstream expr_stream;
				expr_stream << "In expression: " << *this << ", " << *right << " is not of type " << left->getType() << " List";
				fatal(expr_stream.str().c_str());
			}
			type = typeList(left->getType());
			break;
	}
}

Value* BinOp::compile() const {
	Value* l = left->compile();
	Value* r = right->compile();

	if (left->isLval())
		l = loadValue(l);
	if (right->isLval())
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
		case CONS:
			Value* p = Builder.CreateCall(TheMalloc, {c64(16)}, "malloctmp");
			Value* n = Builder.CreateBitCast(p, ListType, "nodetmp");
			Value* h = Builder.CreateGEP(n, {c32(0), c32(0)}, "headptr");
			Value* lSExt;
			if (PointerType::classof(l->getType()))
				lSExt = Builder.CreatePtrToInt(l, i64, "listptr");
			else 
				lSExt = Builder.CreateSExt(l, i64, "ext");
			Builder.CreateStore(lSExt, h);
			Value* t = Builder.CreateGEP(n, {c32(0), c32(1)}, "tailptr");
			Builder.CreateStore(r, t);
			return n;
	}
	return nullptr;
}

void New::sem() {
	type = typeIArray(ref);
	size->sem();
	if (!equalType(size->getType(), typeInteger)) {
		fatal("Integer size expected for new operator");
	}
}
Value* New::compile() const {
	Value* s = loadValue(size->compile());
	std::vector<Value*> ArgsV;
	Value* multmp = Builder.CreateMul(s, c16(sizeOfType(ref)), "multmp");
	ArgsV.push_back(Builder.CreateSExt(multmp, i64, "exttmp"));
	Value* ptr = Builder.CreateCall(TheMalloc, ArgsV, "malloctmp");
	return Builder.CreateBitCast(ptr, PointerType::getUnqual(translate(ref)), "bitctemp");
}

void Nil::sem() {
	type = typeList(typeAny);
}

Value* Nil::compile() const {
	Value* nil_list = ConstantPointerNull::get(ListType);
	return nil_list;
}
