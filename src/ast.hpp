#ifndef __AST_HPP__
#define __AST_HPP__

#include <iostream>
#include <vector>
#include <string>
#include <sstream>

#ifdef __cplusplus
extern "C" {
#endif

#include "symbol.h"
#include "error.h"

#ifdef __cplusplus
}
#endif

#include "printers.hpp"

#include <llvm/IR/Value.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Utils.h>

extern bool o_flag;

class AST {
	public:
		virtual ~AST() {}
		virtual void printOn(std::ostream& out) const = 0;
		virtual void sem() = 0;
		virtual llvm::Value* compile() const {
			return nullptr;
		};

		void llvm_compile_and_dump(bool optimize=true);

		static llvm::Type* translate(Type t);
		static llvm::Value* loadValue(llvm::Value* p);
		static void printIR(std::string& outfile) {
			llvm::raw_ostream *out;
			std::error_code EC;
			out = new llvm::raw_fd_ostream(outfile, EC);
			TheModule->print(*out, nullptr);
		}

	protected:
		static llvm::LLVMContext TheContext;
		static llvm::IRBuilder<> Builder;
		static std::unique_ptr<llvm::Module> TheModule;
		static std::unique_ptr<llvm::legacy::FunctionPassManager> TheFPM;

		static llvm::Function *ThePuti;
		static llvm::Function *ThePutb;
		static llvm::Function *ThePutc;
		static llvm::Function *ThePuts;
		static llvm::Function *TheGeti;
		static llvm::Function *TheGetb;
		static llvm::Function *TheGetc;
		static llvm::Function *TheGets;
		static llvm::Function *TheAbs;
		static llvm::Function *TheOrd;
		static llvm::Function *TheChr;
		static llvm::Function *TheStrlen;
		static llvm::Function *TheStrcmp;
		static llvm::Function *TheStrcpy;
		static llvm::Function *TheStrcat;
		static llvm::Function *TheInit;
		static llvm::Function *TheMalloc;

		static llvm::Type *i1;
		static llvm::Type *i8;
		static llvm::Type *i16;
		static llvm::Type *i32;
		static llvm::Type *i64;
		static llvm::PointerType *ListType;

		static std::string prepend;

		static void addLibFunctionPointers();

		static llvm::ConstantInt* c1(bool b) {
			return llvm::ConstantInt::get(TheContext, llvm::APInt(1, b, true));
		}
		static llvm::ConstantInt* c8(char c) {
			return llvm::ConstantInt::get(TheContext, llvm::APInt(8, c, true));
		}
		static llvm::ConstantInt* c16(int n) {
			return llvm::ConstantInt::get(TheContext, llvm::APInt(16, n, true));
		}
		static llvm::ConstantInt* c32(int n) {
			return llvm::ConstantInt::get(TheContext, llvm::APInt(32, n, true));
		}
		static llvm::ConstantInt* c64(int n) {
			return llvm::ConstantInt::get(TheContext, llvm::APInt(64, n, true));
		}
};

inline std::ostream& operator << (std::ostream& out, const AST &t){
	t.printOn(out);
	return out;
}

class Decl : public AST {

};

class Stmt : public AST {

};

class Expr : public AST {
	public:
		Type getType() {
			return type;
		}

		virtual bool isLval() {
			return false;
		}

		virtual bool isString() {
			return false;
		}

	protected:
		Type type;
};

class Formal : public AST {
	public:
		Formal(bool byreference, Type t, std::vector<std::string>& ids) :
			ref(byreference), type(t), id_list(ids) {}

		~Formal() {}

		virtual void printOn(std::ostream& out) const override {
			if (ref) out << "Ref ";
			out << type <<"(";
			bool first = true;
			for (std::string id : id_list){
				if (!first) out << ", ";
				first = false;
				out << id;
			}
			out << ")";
		}

		bool getRef() {
			return ref;
		}

		Type getType() {
			return type;
		}

		const std::vector<std::string>& getIdList() const {
			return id_list;
		}

		virtual void sem() override {}

	private:
		bool ref;
		Type type;
		std::vector<std::string> id_list;
};

class Header : public AST {
	public:
		Header(Type t, std::string name, const std::vector<Formal*>& formal_l = std::vector<Formal*>()) :
			type(t), id(name), formal_list(formal_l), is_def(true), is_main(false) {}

		~Header(){
			for (Formal* f : formal_list) delete f;
		}

		virtual void printOn(std::ostream& out) const override {
			out << "Header(";
			out << type << " ";
			out << id << " Parameters(";
			bool first = true;
			for (Formal* f : formal_list){
				if (!first) out << ", ";
				first = false;
				out << *f;
			}
			out << "))";
		}

		virtual void sem() override; 

		virtual llvm::Function* compile() const override;

		void set_main() {
			is_main = true;
		}

		void unset_def() {
			is_def = false;
		}

		std::string getName() {
			return id;
		}

		Type getType() {
			return type;
		}

	private:
		Type type;
		std::string id;
		std::vector<Formal*> formal_list;
		bool is_def;
		bool is_main;
};

class Atom : public Expr {
	public:
		virtual bool isCharOfString() {
			return false;
		}
};

class FunctionDef : public Decl {
	public:
		FunctionDef(Header *h, std::vector<Decl*>& decl_l, std::vector<Stmt*>& stmt_l) :
			header(h), decl_list(decl_l), stmt_list(stmt_l), is_main(false) {}

		~FunctionDef() {
			delete header;
			for (Decl* d : decl_list) delete d;
			for (Stmt* s : stmt_list) delete s;
		}

		virtual void printOn(std::ostream& out) const override {
			out << "FunctionDef( ";
			out << *header;
			for (Decl *d : decl_list) {
				out << ", ";
				out << *d;
			}
			for (Stmt *s : stmt_list) {
				out << ", ";
				out << *s;
			}
			out << ")";
		}

		virtual void sem() override;

		virtual llvm::Function* compile() const override;

		void set_main(){
			is_main = true;
			header->set_main();
		}

	private:
		Header* header;
		std::vector<Decl*> decl_list;
		std::vector<Stmt*> stmt_list;
		bool is_main;
};

class FunctionDecl : public Decl {
	public:
		FunctionDecl(Header *h) : header(h) {}

		~FunctionDecl() {
			delete header;
		}

		virtual void printOn(std::ostream& out) const override {
			out << "FunctionDecl(" << header << ")";
		}

		virtual void sem() override; 

		virtual llvm::Function* compile() const override;

	private:
		Header* header;
};

class VarDef : public Decl {
	public:
		VarDef(Type t, std::vector<std::string>& ids) :
			type(t), id_list(ids) {}

		~VarDef() {}

		virtual void printOn(std::ostream& out) const override {
			out << "VarDef(" << type << "(";
			bool first = true;
			for (std::string id : id_list) {
				if (!first) out << ", ";
				first = false;
				out << id;
			}
			out << "))";
		}

		virtual void sem() override; 

		virtual llvm::Value* compile() const override;

	private:
		Type type;
		std::vector<std::string> id_list;
};

class Simple : public Stmt {
};

class Skip : public Simple {
	public:
		virtual void printOn(std::ostream& out) const override {
			out << "Skip";
		}
		virtual void sem() override {}

		virtual llvm::Value* compile() const override;
};

class Assign : public Simple {
	public:
		Assign(Atom* lhs, Expr* rhs) :
			lval(lhs), rval(rhs) {}

		~Assign() {
			delete lval;
			delete rval;
		}

		virtual void printOn(std::ostream& out) const override {
			out << "Assign(" << *lval << ", " << *rval << ")";
		}

		virtual void sem() override; 

		virtual llvm::Value* compile() const override;

	private:
		Atom* lval;
		Expr* rval;
};

class Call : public Simple, public Atom {
	public:
		Call(std::string id, const std::vector<Expr*>& exprs = std::vector<Expr*>()) :
			name(id), parameters(exprs) {}

		~Call() {
			for (Expr* e : parameters) delete e;
		}

		void setStmt(bool b) {
			isStmt = b;
		}

		virtual void printOn(std::ostream& out) const override {
			out << "Call(" << name << ", Args(";
			bool first = true;
			for (Expr* e : parameters) {
				if (!first) out << ", ";
				first = false;
				out << *e;
			}
			out << "))";
		}

		virtual void sem() override; 

		virtual llvm::Value* compile() const override;

	private:
		std::string name;
		std::vector<Expr*> parameters;
		bool isStmt;
};

class Exit : public Stmt {
	public:
		virtual void printOn(std::ostream& out) const override {
			out << "Exit";
		}

		virtual void sem() override; 

		virtual llvm::Value* compile() const override;
};

class Return : public Stmt {
	public:
		Return(Expr* ex) : expr(ex) {}

		virtual void printOn(std::ostream& out) const override {
			out << "Return(" << *expr << ")";
		}

		virtual void sem() override; 

		virtual llvm::Value* compile() const override;

	private:
		Expr* expr;
};

class Elsif : public Stmt {
	public:
		Elsif(Expr* expr, const std::vector<Stmt*>& statements) :
			cond(expr), stmt_list(statements) {}

		~Elsif() {
			delete cond;
			for (Stmt* st : stmt_list) delete st;
		}

		virtual void printOn(std::ostream& out) const override {
			out << "Elsif(" << *cond << ", (";
			bool first = true;
			for (Stmt* st : stmt_list) {
				if (!first) out << ", ";
				first = false;
				out << *st;
			};
			out << "))";
		}

		virtual void sem() override; 

		virtual llvm::Value* compile() const override;

		Expr* getCond() {
			return cond;
		}

	private:
		Expr* cond;
		std::vector<Stmt*> stmt_list;
};

class If : public Stmt {
	public:
		If(Expr* expr, std::vector<Stmt*>& stmts, std::vector<Elsif*>& elsifs, const std::vector<Stmt*>& els = std::vector<Stmt*>()) :
			cond(expr), statements(stmts), elsif_list(elsifs), else_statements(els) {}

		~If() {
			delete cond;
			for (Stmt* st : statements) delete st;
			for (Elsif* e : elsif_list) delete e;
			for (Stmt* st : else_statements) delete st;
		}

		virtual void printOn(std::ostream& out) const override {
			out << "If(" << *cond << ", (";
			bool first = true;
			for (Stmt* st : statements){
				if (!first) out << ", ";
				first = false;
				out << *st;
			}
			out << ")";
			for (Elsif* e : elsif_list) out << ", " << *e;
			if (else_statements.size() > 0) {
				out << ", Else(";
				first = true;
				for (Stmt *st : else_statements){
					if (!first) out << ", ";
					first = false;
					out << *st;
				}
				out << ")";
			}
			out << ")";
		}

		virtual void sem() override; 

		virtual llvm::Value* compile() const override;

	private:
		Expr* cond;
		std::vector<Stmt*> statements;
		std::vector<Elsif*> elsif_list;
		std::vector<Stmt*> else_statements;
};

class For : public Stmt {
	public:
		For(std::vector<Simple*>& in, Expr* c, std::vector<Simple*>& incr, std::vector<Stmt*>& stmts) :
			init(in), cond(c), after(incr), stmt_list(stmts) {}

		~For() {
			for (Simple* s : init) delete s;
			delete cond;
			for (Simple* s : after) delete s;
			for (Stmt* st : stmt_list) delete st;
		}

		virtual void printOn(std::ostream& out) const override {
			out << "For(Initializers(";
			bool first = true;
			for (Simple* s : init) {
				if (!first) out << ", ";
				first = false;
				out << *s;
			}
			out << "), Condition(" << *cond << "), After_stmts(";
			first = true;
			for (Simple* s : after) {
				if (!first) out << ", ";
				first = false;
				out << *s;
			}
			out << "), Body(";
			first = true;
			for (Stmt* st : stmt_list) {
				if (!first) out << ", ";
				first = false;
				out << *st;
			}
			out << "))";
		}

		virtual void sem() override; 

		virtual llvm::Value* compile() const override;

	private:
		std::vector<Simple*> init;
		Expr* cond;
		std::vector<Simple*> after;
		std::vector<Stmt*> stmt_list;
};

class Id : public Atom {
	public:
		Id(std::string name) : id(name) {}

		virtual void printOn(std::ostream& out) const override {
			out << "Id(" << id << ")";
		}

		virtual bool isLval() override {
			return true;
		}

		virtual void sem() override; 

		virtual llvm::Value* compile() const override;

	private:
		std::string id;
};

class ConstString : public Atom {
	public:
		ConstString(std::string val) : mystring(val) {}

		virtual void printOn(std::ostream& out) const override {
			out << "ConstString(\"";
			for (char c : mystring)
				printChar(out, c);
			out << "\")";
		}

		virtual void sem() override; 

		virtual llvm::Value* compile() const override;

		virtual bool isString() override {
			return true;
		}

	private:
		std::string mystring;
};

class ArrayItem : public Atom {
	public:
		ArrayItem(Atom* a, Expr* e) :
			array(a), pos(e) {}

		~ArrayItem() {
			delete array;
			delete pos;
		}

		virtual void printOn(std::ostream& out) const override {
			out << "ArrayItem(" << *array << ", " << *pos << ")";
		}

		virtual void sem() override; 

		virtual llvm::Value* compile() const override;

		virtual bool isLval() override {
			return true;
		}

		virtual bool isCharOfString() override {
			return array->isString();
		}

	private:
		Atom* array;
		Expr* pos;
};

class ConstInt : public Expr {
	public:
		ConstInt(int n) : num(n) {}

		virtual void printOn(std::ostream& out) const override {
			out << "ConstInt(" << num << ")";
		}

		virtual void sem() override; 

		virtual llvm::Value* compile() const override;

	private:
		int num;
};

class ConstChar : public Expr {
	public:
		ConstChar(char c) : mychar(c) {}

		virtual void printOn(std::ostream& out) const override {
			out << "ConstChar(\'";
			printChar(out, mychar);
			out << "\')";
		}

		virtual void sem() override; 

		virtual llvm::Value* compile() const override;

	private:
		char mychar;
};

class ConstBool : public Expr {
	public:
		ConstBool(bool b) :
			boolean(b) {}

		virtual void printOn(std::ostream& out) const override {
			out << "ConstBool(";
			out << (boolean ? "true" : "false") << ")";
		}

		virtual void sem() override; 

		virtual llvm::Value* compile() const override;

	private:
		bool boolean;
};

class UnOp : public Expr {
	public:
		UnOp(unary_ops o,  Expr* e) :
			op(o), expr(e) {}

		virtual void printOn(std::ostream& out) const override {
			out << "UnOp(" << op << "(" << *expr << "))";
		}

		virtual void sem() override; 

		virtual llvm::Value* compile() const override;

	private:
		unary_ops op;
		Expr* expr;
};

class BinOp : public Expr {
	public:
		BinOp(Expr* l, binary_ops o, Expr* r) :
			left(l), op(o), right(r) {}

		virtual void printOn(std::ostream& out) const override {
			out << "BinOp(" << op << "(";
			out << *left << ", " << *right << "))";
		}

		virtual void sem() override; 

		virtual llvm::Value* compile() const override;

	private:
		Expr* left;
		binary_ops op;
		Expr* right;
};

class New : public Expr {
	public:
		New(Type t, Expr* e) :
			ref(t), size(e) {}

		virtual void printOn(std::ostream& out) const override {
			out << "New(" << type << ", " << *size << ")";
		}

		virtual void sem() override; 

		virtual llvm::Value* compile() const override;

	private:
		Type ref;
		Expr* size;
};

class Nil : public Expr {
	public:
		virtual void printOn(std::ostream& out) const override {
			out << "Nil";
		}

		virtual void sem() override; 

		virtual llvm::Value* compile() const override;
};

typedef std::vector<std::string> Id_List;
typedef std::vector<Decl*> Decl_List;
typedef std::vector<Stmt*> Stmt_List;
typedef std::vector<Formal*> Formal_List;
typedef std::vector<Elsif*> Elsif_List;
typedef std::vector<Simple*> Simple_List;
typedef std::vector<Expr*> Expr_List;

#endif
