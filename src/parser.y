%{
#include <iostream>

#include "lexer.hpp"
#include "ast.hpp"

int linecount=1;
%}

%define parse.error verbose

%token T_and	"and"
%token T_bool 	"bool"
%token T_char	"char"
%token T_decl	"decl"
%token T_def	"def"
%token T_else	"else"
%token T_elsif	"elsif"
%token T_end	"end"
%token T_exit	"exit"
%token T_false	"false"
%token T_for	"for"
%token T_head	"head"
%token T_if		"if"
%token T_int	"int"
%token T_list	"list"
%token T_mod	"mod"
%token T_new	"new"
%token T_nil	"nil"
%token T_isnil	"nil?"
%token T_not	"not"
%token T_or		"or"
%token T_ref	"ref"
%token T_return	"return"
%token T_skip	"skip"
%token T_tail	"tail"
%token T_true	"true"

%token T_add		"+"
%token T_sub		"-"
%token T_mul		"*"
%token T_div		"/"
%token T_cons		"#"
%token T_eq			"="
%token T_neq		"<>"
%token T_less		"<"
%token T_greater	">"
%token T_leq		"<="
%token T_geq		">="

%token T_lpar	"("
%token T_rpar	")"
%token T_lbr	"["
%token T_rbr	"]"
%token T_comma	","
%token T_sem	";"
%token T_col	":"
%token T_assign	":="

%token<id> T_id
%token T_num
%token T_constchar
%token T_string

%left "or"
%left "and"
%nonassoc "not"
%nonassoc "=" "<>" ">" "<" "<=" ">="
%right "#"
%left "+" "-"
%left "*" "/" "mod"

%union {
	FunctionDef* func;
	FunctionDecl* funcdecl;
	Stmt* stmt;
	Formal* formal;
	Header* header;
	VarDef* vardef;
	Id_List* id_list;
	Decl_List* decl_list;
	Stmt_List* stmt_list;
	Formal_List* formal_list;
	Decl* decl;

	Type type;
	char* id;
}

%type<func> func_def
%type<decl_list> def_list
%type<stmt_list> stmt_list
%type<decl> def
%type<header> header
%type<formal> formal
%type<formal_list> formal_list
%type<id_list> id_list
%type<funcdecl> func_decl
%type<vardef> var_def
%type<stmt> stmt

%%

program:
	func_def {
		std::cout << "AST: " << *$1 << std::endl;
	}
;

func_def:
	"def" header ":" def_list stmt_list "end" { $$ = new FunctionDef($2, *$4, *$5); }
;

def_list:
	/* nothing*/	{ $$ = new Decl_List(); }
|	def_list def	{ $1->push_back($2); $$ = $1; } 
;

stmt_list:
	stmt			{ $$ = new Stmt_List(1, $1); }
|	stmt_list stmt	{ $1->push_back($2); $$ = $1; }
;

def:
	func_def	{ $$ = $1; }
|	func_decl	{ $$ = $1; }
|	var_def		{ $$ = $1; }
;

header:
	type T_id "(" formal formal_list ")"	{ $5->insert($5->begin(), $4); $$ = new Header(TYPE, $2, *$5); }
|	type T_id "(" ")"						{ $$ = new Header(TYPE, $2); }
|	T_id "(" formal formal_list ")"			{ $4->insert($4->begin(), $3); $$ = new Header(TYPE_VOID, $1, *$4); }
|	T_id "(" ")"							{ $$ = new Header(TYPE_VOID, $1); }
;

formal:
	"ref" type T_id id_list	{ $4->insert($4->begin(), $3); $$ = new Formal(TYPE, *$4); }
|	type T_id id_list { $3->insert($3->begin(), $2); $$ = new Formal(TYPE, *$3); }
;

formal_list:
	/* nothing */			{ $$ = new Formal_List(); }
|	formal_list ";" formal	{ $1->push_back($3); $$ = $1; }
;

id_list:
	/* nothing */		{ $$ = new Id_List(); }
|	id_list "," T_id	{ $1->push_back($3); $$ =$1; }
;

type:
	"int"
|	"bool"
|	"char"
|	type "[" "]"
|	"list" "[" type "]"
;

func_decl:
	"decl" header	{ $$ = new FunctionDecl($2); }
;

var_def:
	type T_id id_list { $3->insert($3->begin(), $2); $$ = new VarDef(TYPE, *$3); }
;

stmt:
	simple															{ $$ = new Stmt(); }
|	"exit"															{ $$ = new Stmt(); }
|	"return" expr													{ $$ = new Stmt(); }
|	"if" expr ":" stmt_list elsif_list "end"						{ $$ = new Stmt(); }
|	"if" expr ":" stmt_list elsif_list "else" ":" stmt_list "end"	{ $$ = new Stmt(); }
|	"for" simple_list ";" expr ";" simple_list ":" stmt_list "end"	{ $$ = new Stmt(); }
;

elsif_list:
	/* nothing */
|	elsif_list elsif_stmt
;

elsif_stmt:
	"elsif" expr ":" stmt_list
;

simple:
	"skip"
|	atom ":=" expr
|	call
;

simple_list:
	simple
|	simple_list "," simple
;

call:
	T_id "(" expr expr_list ")"
|	T_id "(" ")"
;

expr_list:
	/* nothing */
|	expr_list "," expr
;

atom:
	T_id
|	T_string
|	atom "[" expr "]"
|	call
;

expr:
	atom
|	T_num
|	T_constchar
|	"(" expr ")"
|	"+" expr
|	"-" expr
|	expr "+" expr
|	expr "-" expr
|	expr "*" expr
|	expr "/" expr
|	expr "mod" expr
|	expr "=" expr
|	expr "<>" expr
|	expr "<" expr
|	expr ">" expr
|	expr "<=" expr
|	expr ">=" expr
|	"true"
|	"false"
|	"not" expr
|	expr "and" expr
|	expr "or" expr
|	"new" type "[" expr "]"
|	"nil"
|	"nil?" "(" expr ")"
|	expr "#" expr
|	"head" "(" expr ")"
|	"tail" "(" expr ")"
;

%%

int main() {
	int result = yyparse();
// 	if (result == 0) std::cout << "Success.\n";
	return result;
}
