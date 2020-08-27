#include "ast.hpp"

using llvm::Function;
using llvm::FunctionType;
using llvm::Value;
using llvm::LLVMContext;
using llvm::IRBuilder;
using llvm::Module;
using llvm::IntegerType;
using llvm::PointerType;
using llvm::BasicBlock;
using llvm::AllocaInst;
using llvm::outs;
using llvm::errs;

LLVMContext AST::TheContext;
using llvm::outs;
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
		default:
			return nullptr;
	}
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
			for (size_t i=0; i<formal_size; ++i)
				types.push_back(translate(f->getType()));
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
	}

	Builder.CreateRetVoid();

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

Value* ConstInt::compile() const {
	return c16(num);
}

Value* ConstChar::compile() const {
	return c8(mychar);
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
			return Builder.CreateICmpEQ(l, r, "eqtmp");
		case NEQ:
			return Builder.CreateICmpNE(l, r, "neqtmp");
		case GREATER:
			return Builder.CreateICmpSGT(l, r, "gttmp");
		case LESS:
			return Builder.CreateICmpSLT(l, r, "lttmp");
		case GEQ:
			return Builder.CreateICmpSGE(l, r, "geqtmp");
		case LEQ:
			return Builder.CreateICmpSLE(l, r, "leqtmp");
		case AND:
			return Builder.CreateAnd(l, r, "andtmp");
		case OR:
			return Builder.CreateOr(l, r, "ortmp");
		default:
			return nullptr;
	}
}

Value* ConstBool::compile() const {
	return c8(boolean);
}

Value* Call::compile() const {
	Function *CalleeF = TheModule->getFunction(name);
	// Look up the name in the global module table
	if (!CalleeF) {
		std::cout << "Unkown function referenced" << std::endl;
		return nullptr;
	}

	// Iterate for each parameter
	std::vector<llvm::Value*> ArgsV;
	for (unsigned i = 0, e = parameters.size(); i != e; ++i) {
		ArgsV.push_back(parameters[i]->compile());
		if (!ArgsV.back())
			return nullptr;
	}

	// return Builder.CreateCall(CalleeF, ArgsV, "calltmp");
	return Builder.CreateCall(CalleeF, ArgsV);
}
