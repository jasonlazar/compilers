#ifndef __AST_HPP__
#define __AST_HPP__

#include <iostream>
#include <vector>
#include <string>
#include <sstream>

#include "symbol.h"
#include "error.h"

#include "printers.hpp"


class AST {
	public:
		virtual ~AST() {}
		virtual void printOn(std::ostream& out) const = 0;
		virtual void sem() = 0;
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

		virtual void sem() override {
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

			// std::cout << (func->u.eFunction.isForward ? "Forward Function: " : "Function: " ) << func->id << std::endl;

		}

		void set_main() {
			is_main = true;
		}

		void unset_def() {
			is_def = false;
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
		virtual bool isString() {
			return false;
		}

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

		virtual void sem() override {
			header->sem();
			for (Decl *d : decl_list) {
				d->sem();
			}
			for (Stmt *s : stmt_list) {
				s->sem();
			}

			printSymbolTable();

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

		virtual void sem() override {
			header->unset_def();
			header->sem();

			printSymbolTable();

			closeScope();
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

		virtual void sem() override {
			for (std::string id : id_list) {
				newVariable(id.c_str(), type);
			}
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
		virtual void sem() override {}
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

		virtual void sem() override {
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

		virtual void sem() override {
			SymbolEntry* e = lookupEntry(name.c_str(), LOOKUP_ALL_SCOPES, true);
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

		virtual void sem() override {
			if (!equalType(currentScope->returnType, typeVoid))
				fatal("Exit statement in non Void Function");
		}
};

class Return : public Stmt {
	public:
		Return(Expr* ex) : expr(ex) {}

		virtual void printOn(std::ostream& out) const override {
			out << "Return(" << *expr << ")";
		}

		virtual void sem() override {
			expr->sem();
			if (!equalType(currentScope->returnType, expr->getType())) {
				std::stringstream expr;
				expr << "Return statement: " << *this << " must return type " << currentScope->returnType;
				fatal(expr.str().c_str());
			}
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

		virtual void sem() override {
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

		virtual void sem() override {
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

		virtual void sem() override {
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

		virtual void sem() override {
			SymbolEntry* e = lookupEntry(id.c_str(), LOOKUP_ALL_SCOPES, true);
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

		virtual void sem() override {
			type = typeIArray(typeChar);
		}

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

		virtual bool isLval() override {
			return true;
		}

		virtual bool isCharOfString() override {
			return array->isString();
		}

		virtual void sem() override {
			array->sem();
			Type t = array->getType();
			if (t->kind != TYPE_IARRAY) {
				std::stringstream atom;
				atom << *array << " is not an Array";
				fatal(atom.str().c_str());
			}
			type = t->refType;
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

		virtual void sem() override {
			type = typeInteger;
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

		virtual void sem() override {
			type = typeChar;
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

		virtual void sem() override {
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

		virtual void sem() override {
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

		virtual void sem() override {
			type = typeBoolean;
		}

	private:
		bool boolean;
};

class New : public Expr {
	public:
		New(Type t, Expr* e) :
			ref(t), size(e) {}

		virtual void printOn(std::ostream& out) const override {
			out << "New(" << type << ", " << *size << ")";
		}

		virtual void sem() override {
			type = typeIArray(ref);
			size->sem();
			if (!equalType(size->getType(), typeInteger)) {
				fatal("Integer size expected for new operator");
			}
		}

	private:
		Type ref;
		Expr* size;
};

class Nil : public Expr {
	public:
		virtual void printOn(std::ostream& out) const override {
			out << "Nil";
		}

		virtual void sem() override {
			type = typeList(typeAny);
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
