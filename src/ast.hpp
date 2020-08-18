#ifndef __AST_HPP__
#define __AST_HPP__

#include <iostream>
#include <vector>
#include <string>

enum Type {TYPE_VOID, TYPE};

inline std::ostream& operator << (std::ostream& out, Type t){
	switch(t) {
	case TYPE_VOID:
		out << "VOID";
		break;
	default:
		out << "Type";
	}
	return out;
}

class AST {
	public:
		virtual ~AST() {}
		virtual void printOn(std::ostream &out) const = 0;
};

inline std::ostream& operator << (std::ostream &out, const AST &t){
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

class Formal : public AST {
	public:
		Formal(Type t, std::vector<std::string>& ids):
			type(t), id_list(ids) {}

		~Formal() {}

		virtual void printOn(std::ostream& out) const override {
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

		virtual void printOn(std::ostream &out) const override {
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

class FunctionDef : public Decl {
	public:
		FunctionDef(Header *h, std::vector<Decl*>& decl_l, std::vector<Stmt*>& stmt_l):
			header(h), decl_list(decl_l), stmt_list(stmt_l) {}

		~FunctionDef() {
			delete header;
			for (Decl* d : decl_list) delete d;
			for (Stmt* s : stmt_list) delete s;
		}

		virtual void printOn(std::ostream &out) const override {
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

		virtual void printOn(std::ostream &out) const override {
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

		virtual void printOn(std::ostream &out) const override {
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

typedef std::vector<std::string> Id_List;
typedef std::vector<Decl*> Decl_List;
typedef std::vector<Stmt*> Stmt_List;
typedef std::vector<Formal*> Formal_List;

#endif
