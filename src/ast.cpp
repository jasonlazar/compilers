#include "ast.hpp"

llvm::LLVMContext AST::TheContext;
llvm::IRBuilder<> AST::Builder(TheContext);
std::unique_ptr<llvm::Module> AST::TheModule;

llvm::Type *AST::i8;
llvm::Type *AST::i32;
llvm::Type *AST::i64;
