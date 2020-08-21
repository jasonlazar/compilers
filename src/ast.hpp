#ifndef __AST_HPP__
#define __AST_HPP__

#include <iostream>
#include <vector>
#include <string>

#include "symbol.h"

enum unary_ops {UPLUS, UMINUS, NOT, IS_NIL, HEAD, TAIL};

enum binary_ops {PLUS, MINUS, TIMES, DIV, MOD, EQ, NEQ, GREATER, LESS, GEQ, LEQ, AND, OR, CONS};

inline std::ostream& operator << (std::ostream& out, Type t){
	switch(t->kind) {
	case TYPE_VOID:
		out << "VOID";
		break;
	case TYPE_INTEGER:
		out << "Int";
		break;
	case TYPE_BOOLEAN:
		out << "Bool";
		break;
	case TYPE_CHAR:
		out << "Char";
		break;
	case TYPE_IARRAY:
		out << "Array of " << t->refType;
		break;
	case TYPE_LIST:
		out << "List of " << t->refType;
		break;
	default:
		out << "Type";
	}
	return out;
}

inline std:: ostream& operator << (std::ostream& out, unary_ops op){
	switch(op) {
		case UPLUS:
			out << "+"; break;
		case UMINUS:
			out << "-"; break;
		case NOT:
			out << "not"; break;
		case IS_NIL:
			out << "nil?"; break;
		case HEAD:
			out << "head"; break;
		case TAIL:
			out << "tail"; break;
	}
	return out;
}

inline std::ostream& operator << (std::ostream& out, binary_ops op){
	switch(op) {
		case PLUS:
			out << "+"; break;
		case MINUS:
			out << "-"; break;
		case TIMES:
			out << "*"; break;
		case DIV:
			out << "/"; break;
		case MOD:
			out << "mod"; break;
		case EQ:
			out << "="; break;
		case NEQ:
			out << "<>"; break;
		case GREATER:
			out << ">"; break;
		case LESS:
			out << "<"; break;
		case GEQ:
			out << ">="; break;
		case LEQ:
			out << "<="; break;
		case AND:
			out << "and"; break;
		case OR:
			out << "or"; break;
		case CONS:
			out << "#"; break;
	}
	return out;
}

class AST {
	public:
		virtual ~AST() {}
		virtual void printOn(std::ostream& out) const = 0;
};

inline std::ostream& operator << (std::ostream& out, const AST &t){
	t.printOn(out);
	return out;
}

class Decl : public AST {
};

class Stmt : public AST {
	public:
		Stmt() {}
		virtual void printOn(std::ostream& out) const override {
			out << "Stmt";
		}
};

class Expr : public AST {
	public:
		virtual void printOn(std::ostream& out) const override{
			out << "Expr";
		}
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

	private:
		bool ref;
		Type type;
		std::vector<std::string> id_list;
};

class Header : public AST {
	public:
		Header(Type t, std::string name, const std::vector<Formal*>& formal_l = std::vector<Formal*>()) :
			type(t), id(name), formal_list(formal_l) {}

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

	private:
		Type type;
		std::string id;
		std::vector<Formal*> formal_list;
};

class Atom : public Expr {
	public:
		virtual void printOn(std::ostream& out) const override {
			out << "Atom";
		}
};

class FunctionDef : public Decl {
	public:
		FunctionDef(Header *h, std::vector<Decl*>& decl_l, std::vector<Stmt*>& stmt_l):
			header(h), decl_list(decl_l), stmt_list(stmt_l) {}

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

	private:
		Header* header;
		std::vector<Decl*> decl_list;
		std::vector<Stmt*> stmt_list;
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
	public:
		virtual void printOn(std::ostream& out) const override {
			out << "Simple";
		}
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
			out << "ConstString(\"" << mystring << "\")";
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
			out << "ConstChar(\'" << mychar << "\')";
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
