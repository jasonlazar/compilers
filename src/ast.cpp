#include "ast.hpp"

llvm::LLVMContext AST::TheContext;
llvm::IRBuilder<> AST::Builder(TheContext);
std::unique_ptr<llvm::Module> AST::TheModule;

llvm::Function *AST::ThePuti;
llvm::Function *AST::ThePutb;
llvm::Function *AST::ThePutc;
llvm::Function *AST::ThePuts;
llvm::Function *AST::TheGeti;
llvm::Function *AST::TheGetb;
llvm::Function *AST::TheGetc;
llvm::Function *AST::TheGets;
llvm::Function *AST::TheAbs;
llvm::Function *AST::TheOrd;
llvm::Function *AST::TheChr;
llvm::Function *AST::TheStrlen;
llvm::Function *AST::TheStrcmp;
llvm::Function *AST::TheStrcpy;
llvm::Function *AST::TheStrcat;

llvm::Type *AST::i1;
llvm::Type *AST::i8;
llvm::Type *AST::i16;
llvm::Type *AST::i32;
llvm::Type *AST::i64;

llvm::Type* translate(Type t) {
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
