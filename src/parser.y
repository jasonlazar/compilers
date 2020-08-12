%{
#include <iostream>
#include "lexer.hpp"

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

%token T_id
%token T_num
%token T_constchar
%token T_string

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

%left "or"
%left "and"
%nonassoc "not"
%nonassoc "=" "<>" ">" "<" "<=" ">="
%right "#"
%left "+" "-"
%left "*" "/" "mod"

%%

program:
	func_def
;

func_def:
	"def" header ":" def_list stmt_list "end"
;

def_list:
	/* nothing*/
|	def def_list
;

stmt_list:
	stmt
|	stmt_list stmt
;

def:
	func_def
|	func_decl
|	var_def
;

header:
	type T_id "(" formal formal_list ")"
|	type T_id "(" ")"
|	T_id "(" formal formal_list ")"
|	T_id "(" ")"
;

formal:
	"ref" var_def
|	var_def
;

formal_list:
	/* nothing */
|	formal_list ";" formal
;

id_list:
	/* nothing */
|	id_list "," T_id
;

type:
	"int"
|	"bool"
|	"char"
|	type "[" "]"
|	"list" "[" type "]"
;

func_decl:
	"decl" header
;

var_def:
	type T_id id_list
;

stmt:
	simple
|	"exit"
|	"return" expr
|	"if" expr ":" stmt_list elsif_list "end"
|	"if" expr ":" stmt_list elsif_list "else" ":" stmt_list "end"
|	"for" simple_list ";" expr ";" simple_list ":" stmt_list "end"
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
 	if (result == 0) std::cout << "Success.\n";
	return result;
}
