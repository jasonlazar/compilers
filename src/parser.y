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
%token<str> T_string

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
	Elsif_List* elsif_list;
	Simple_List* simple_list;
	Expr_List* expr_list;
	Decl* decl;
	Simple* simple;
	Elsif* elsif;
	Expr* expr;
	Atom* atom;
	Call* call;

	Type type;
	char* id;
	std::string* str;
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
%type<simple> simple
%type<elsif_list> elsif_list
%type<elsif> elsif_stmt
%type<simple_list> simple_list
%type<call> call
%type<expr_list> expr_list
%type<type> type
%type<atom> atom
%type<expr> expr

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
	type T_id "(" formal formal_list ")"	{ $5->insert($5->begin(), $4); $$ = new Header($1, $2, *$5); }
|	type T_id "(" ")"						{ $$ = new Header($1, $2); }
|	T_id "(" formal formal_list ")"			{ $4->insert($4->begin(), $3); $$ = new Header(TYPE_VOID, $1, *$4); }
|	T_id "(" ")"							{ $$ = new Header(TYPE_VOID, $1); }
;

formal:
	"ref" type T_id id_list	{ $4->insert($4->begin(), $3); $$ = new Formal(true, $2, *$4); }
|	type T_id id_list { $3->insert($3->begin(), $2); $$ = new Formal(false, $1, *$3); }
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
	"int"				{ $$ = TYPE_INT; }
|	"bool"				{ $$ = TYPE_BOOL; }
|	"char"				{ $$ = TYPE_CHAR; }
|	type "[" "]"		{ $$ = TYPE_ARRAY; }
|	"list" "[" type "]"	{ $$ = TYPE_LIST; }
;

func_decl:
	"decl" header	{ $$ = new FunctionDecl($2); }
;

var_def:
	type T_id id_list { $3->insert($3->begin(), $2); $$ = new VarDef($1, *$3); }
;

stmt:
	simple															{ $$ = $1; }
|	"exit"															{ $$ = new Exit(); }
|	"return" expr													{ $$ = new Return($2); }
|	"if" expr ":" stmt_list elsif_list "end"						{ $$ = new If($2, *$4, *$5); }
|	"if" expr ":" stmt_list elsif_list "else" ":" stmt_list "end"	{ $$ = new If($2, *$4, *$5, *$8); }
|	"for" simple_list ";" expr ";" simple_list ":" stmt_list "end"	{ $$ = new For(*$2, $4, *$6, *$8); }
;

elsif_list:
	/* nothing */			{ $$ = new Elsif_List(); }
|	elsif_list elsif_stmt	{ $1->push_back($2); $$ = $1; }
;

elsif_stmt:
	"elsif" expr ":" stmt_list	{ $$ = new Elsif($2, *$4); }
;

simple:
	"skip"			{ $$ = new Skip(); }
|	atom ":=" expr	{ $$ = new Assign($1, $3); }
|	call			{ $$ = $1; }
;

simple_list:
	simple					{ $$ = new Simple_List(1, $1); }
|	simple_list "," simple	{ $1->push_back($3); $$ = $1; }
;

call:
	T_id "(" expr expr_list ")"	{ $4->insert($4->begin(), $3); $$ = new Call($1, *$4); }
|	T_id "(" ")"				{ $$ = new Call($1); }
;

expr_list:
	/* nothing */		{ $$ = new Expr_List(); }
|	expr_list "," expr	{ $1->push_back($3); }
;

atom:
	T_id				{ $$ = new Id($1); }
|	T_string			{ $$ = new ConstString(*$1); }
|	atom "[" expr "]"	{ $$ = new ArrayItem($1, $3); }
|	call				{ $$ = $1; }
;

expr:
	atom	{  $$ = $1; }
|	T_num	{  $$ = new Expr; }
|	T_constchar	{  $$ = new Expr; }
|	"(" expr ")"	{  $$ = new Expr; }
|	"+" expr	{  $$ = new Expr; }
|	"-" expr	{  $$ = new Expr; }
|	expr "+" expr	{  $$ = new Expr; }
|	expr "-" expr	{  $$ = new Expr; }
|	expr "*" expr	{  $$ = new Expr; }
|	expr "/" expr	{  $$ = new Expr; }
|	expr "mod" expr	{  $$ = new Expr; }
|	expr "=" expr	{  $$ = new Expr; }
|	expr "<>" expr	{  $$ = new Expr; }
|	expr "<" expr	{  $$ = new Expr; }
|	expr ">" expr	{  $$ = new Expr; }
|	expr "<=" expr	{  $$ = new Expr; }
|	expr ">=" expr	{  $$ = new Expr; }
|	"true"	{  $$ = new Expr; }
|	"false"	{  $$ = new Expr; }
|	"not" expr	{  $$ = new Expr; }
|	expr "and" expr	{  $$ = new Expr; }
|	expr "or" expr	{  $$ = new Expr; }
|	"new" type "[" expr "]"	{  $$ = new Expr; }
|	"nil"	{  $$ = new Expr; }
|	"nil?" "(" expr ")"	{  $$ = new Expr; }
|	expr "#" expr	{  $$ = new Expr; }
|	"head" "(" expr ")"	{  $$ = new Expr; }
|	"tail" "(" expr ")"	{  $$ = new Expr; }
;

%%

int main() {
	int result = yyparse();
// 	if (result == 0) std::cout << "Success.\n";
	return result;
}
