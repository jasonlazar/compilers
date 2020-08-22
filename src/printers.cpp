#include <iostream>

#include "printers.hpp"

std::ostream& operator << (std::ostream& out, Type t){
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

std:: ostream& operator << (std::ostream& out, unary_ops op){
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

std::ostream& operator << (std::ostream& out, binary_ops op){
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

void printChar(std::ostream& out, char c){
	switch(c){
		case '\n':
			out << "\\n";	return;
		case '\r':
			out << "\\r";	return;
		case '\t':
			out << "\\t";	return;
		case '\'':
			out << "\\\'";	return;
		case '\"':
			out << "\\\"";	return;
		case '\0':
			out << "\\0";	return;
		default:
			out << c;
	}
}
