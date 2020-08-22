#ifndef __AST_HPP__
#define __AST_HPP__

#include <iostream>
#include <vector>
#include <string>

#include "symbol.h"
#include "error.h"

#include "printers.hpp"


class AST {
	public:
		virtual ~AST() {}
		virtual void printOn(std::ostream& out) const = 0;
		virtual void sem() {}
};

inline std::ostream& operator << (std::ostream& out, const AST &t){
	t.printOn(out);
	return out;
}

class Decl : public AST {
	public:
		virtual void sem() override {}

};

class Stmt : public AST {

};

class Expr : public AST {
	public:
		Type getType() { return type; }

	protected:
		Type type;
};

class Formal : public AST {
	public:
		Formal(bool byreference, Type t, std::vector<std::string>& ids):
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
			type(t), id(name), formal_list(formal_l), is_main(false) {}

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

		virtual void sem() override {
			SymbolEntry* func = newFunction(id.c_str(), type);
			for (Formal* f : formal_list) {
				for (std::string id : f->getIdList()) {
					PassMode passmode = f->getRef() ? PASS_BY_REFERENCE : PASS_BY_VALUE;
					newParameter(id.c_str(), type, passmode, func);
				}
			}

			if (is_main) {
				if (type->kind != TYPE_VOID) fatal("Main function %s shouldn't have return type", id.c_str());
				if (formal_list.size() != 0) fatal("Main function %s shouldn't take any arguments",  id.c_str());
			}

			printSymbolTable();

		}

		void set_main() {
			is_main = true;
		}

	private:
		Type type;
		std::string id;
		std::vector<Formal*> formal_list;
		bool is_main;
};

class Atom : public Expr {
};

class FunctionDef : public Decl {
	public:
		FunctionDef(Header *h, std::vector<Decl*>& decl_l, std::vector<Stmt*>& stmt_l):
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

		virtual void sem() override {
			openScope();
			header->sem();
			for (Decl *d : decl_list) {
				d->sem();
			}
			closeScope();
		}

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

	private:
		std::string name;
		std::vector<Expr*> parameters;
};

class Exit : public Stmt {
	public:
		virtual void printOn(std::ostream& out) const override {
			out << "Exit";
		}
};

class Return : public Stmt {
	public:
		Return(Expr* ex) : expr(ex) {}

		virtual void printOn(std::ostream& out) const override {
			out << "Return(" << *expr << ")";
		}

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
			};
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

	private:
		char mychar;
};

class UnOp : public Expr {
	public:
		UnOp(unary_ops o,  Expr* e) :
			op(o), expr(e) {}

		virtual void printOn(std::ostream& out) const override {
			out << "UnOp(" << op << "(" << *expr << "))";
		}

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

	private:
		Expr* left;
		binary_ops op;
		Expr* right;
};

class ConstBool : public Expr {
	public:
		ConstBool(bool b) :
			boolean(b) {}

		virtual void printOn(std::ostream& out) const override {
			out << "ConstBool(";
			out << (boolean ? "true" : "false") << ")";
		}

	private:
		bool boolean;
};

class New : public Expr {
	public:
		New(Type t, Expr* e) :
			type(t), size(e) {}

		virtual void printOn(std::ostream& out) const override {
			out << "New(" << type << ", " << *size << ")";
		}

	private:
		Type type;
		Expr* size;
};

class Nil : public Expr {
	public:
		virtual void printOn(std::ostream& out) const override {
			out << "Nil";
		}
};

typedef std::vector<std::string> Id_List;
typedef std::vector<Decl*> Decl_List;
typedef std::vector<Stmt*> Stmt_List;
typedef std::vector<Formal*> Formal_List;
typedef std::vector<Elsif*> Elsif_List;
typedef std::vector<Simple*> Simple_List;
typedef std::vector<Expr*> Expr_List;

#endif
