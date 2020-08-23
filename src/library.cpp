#include "library.hpp"

extern "C"
{
#include "symbol.h"
}

void Library::load_library_functions() {
	// puti
	SymbolEntry* puti = newFunction("puti");
	forwardFunction(puti);
	openScope();
	newParameter("n", typeInteger, PASS_BY_VALUE, puti);
	endFunctionHeader(puti, typeVoid);
	closeScope();

	// putb
	SymbolEntry* putb = newFunction("putb");
	forwardFunction(putb);
	openScope();
	newParameter("b", typeBoolean, PASS_BY_VALUE, putb);
	endFunctionHeader(putb, typeVoid);
	closeScope();

	// putc
	SymbolEntry* putc = newFunction("putc");
	forwardFunction(putc);
	openScope();
	newParameter("c", typeChar, PASS_BY_VALUE, putc);
	endFunctionHeader(putc, typeVoid);
	closeScope();

	// puts
	SymbolEntry* puts = newFunction("puts");
	forwardFunction(puts);
	openScope();
	newParameter("s", typeIArray(typeChar), PASS_BY_VALUE, puts);
	endFunctionHeader(puts, typeVoid);
	closeScope();

	// geti
	SymbolEntry* geti = newFunction("geti");
	forwardFunction(geti);
	openScope();
	endFunctionHeader(geti, typeInteger);
	closeScope();

	// getb
	SymbolEntry* getb = newFunction("getb");
	forwardFunction(getb);
	openScope();
	endFunctionHeader(getb, typeBoolean);
	closeScope();

	// getc
	SymbolEntry* getc = newFunction("getc");
	forwardFunction(getc);
	openScope();
	endFunctionHeader(getc, typeChar);
	closeScope();

	// gets
	SymbolEntry* gets = newFunction("gets");
	forwardFunction(gets);
	openScope();
	newParameter("n", typeInteger, PASS_BY_VALUE, gets);
	newParameter("s", typeIArray(typeChar), PASS_BY_VALUE, gets);
	endFunctionHeader(gets, typeVoid);
	closeScope();

	// abs
	SymbolEntry* abs = newFunction("abs");
	forwardFunction(abs);
	openScope();
	newParameter("n", typeInteger, PASS_BY_VALUE, abs);
	endFunctionHeader(abs, typeInteger);
	closeScope();

	// ord
	SymbolEntry* ord = newFunction("ord");
	forwardFunction(ord);
	openScope();
	newParameter("c", typeChar, PASS_BY_VALUE, ord);
	endFunctionHeader(ord, typeInteger);
	closeScope();

	// chr
	SymbolEntry* chr = newFunction("chr");
	forwardFunction(chr);
	openScope();
	newParameter("n", typeInteger, PASS_BY_VALUE, chr);
	endFunctionHeader(chr, typeChar);
	closeScope();

	// strlen
	SymbolEntry* strlen = newFunction("strlen");
	forwardFunction(strlen);
	openScope();
	newParameter("s", typeIArray(typeChar), PASS_BY_VALUE, strlen);
	endFunctionHeader(strlen, typeInteger);
	closeScope();

	// strcmp
	SymbolEntry* strcmp = newFunction("strcmp");
	forwardFunction(strcmp);
	openScope();
	newParameter("s1", typeIArray(typeChar), PASS_BY_VALUE, strcmp);
	newParameter("s2", typeIArray(typeChar), PASS_BY_VALUE, strcmp);
	endFunctionHeader(strcmp, typeInteger);
	closeScope();

	// strcpy
	SymbolEntry* strcpy = newFunction("strcpy");
	forwardFunction(strcpy);
	openScope();
	newParameter("trg", typeIArray(typeChar), PASS_BY_VALUE, strcpy);
	newParameter("src", typeIArray(typeChar), PASS_BY_VALUE, strcpy);
	endFunctionHeader(strcpy, typeVoid);
	closeScope();

	// strcat
	SymbolEntry* strcat = newFunction("strcat");
	forwardFunction(strcat);
	openScope();
	newParameter("trg", typeIArray(typeChar), PASS_BY_VALUE, strcat);
	newParameter("src", typeIArray(typeChar), PASS_BY_VALUE, strcat);
	endFunctionHeader(strcat, typeVoid);
	closeScope();
}
