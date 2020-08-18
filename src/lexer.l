%option noyywrap
%x COMMENT

%{
#include <cstring>

#include "ast.hpp"
#include "lexer.hpp"
#include "parser.hpp"

%}

L	[A-Za-z]
D	[0-9]
Hex	({D}|[A-Fa-f])
Esc	\\([tnr0\'\"\\]|x{Hex}{Hex})
W	[ \r\t]

%%

"and"		{ return T_and; }
"bool"		{ return T_bool; }
"char"		{ return T_char; }
"decl"		{ return T_decl; }
"def"		{ return T_def; }
"else"		{ return T_else; }
"elsif"		{ return T_elsif; }
"end"		{ return T_end; }
"exit"		{ return T_exit; }
"false"		{ return T_false; }
"for"		{ return T_for; }
"head"		{ return T_head; }
"if"		{ return T_if; }
"int"		{ return T_int; }
"list"		{ return T_list; }
"mod"		{ return T_mod; }
"new"		{ return T_new; }
"nil"		{ return T_nil; }
"nil?"		{ return T_isnil; }
"not"		{ return T_not; }
"or"		{ return T_or; }
"ref"		{ return T_ref; }
"return"	{ return T_return; }
"skip"		{ return T_skip; }
"tail"		{ return T_tail; }
"true"		{ return T_true; }

"+"		{ return T_add; }
"-"		{ return T_sub; }
"*"		{ return T_mul; }
"/"		{ return T_div; }
"#"		{ return T_cons; }
"="		{ return T_eq; }
"<>"	{ return T_neq; }
"<"		{ return T_less; }
">"		{ return T_greater; }
"<="	{ return T_leq; }
">="	{ return T_geq; }

"("		{ return T_lpar; }
")"		{ return T_rpar; }
"["		{ return T_lbr; }
"]"		{ return T_rbr; }
","		{ return T_comma; }
";"		{ return T_sem; }
":"		{ return T_col; }
":="	{ return T_assign; }

{L}({L}|{D}|[_?])*	{ yylval.id = strdup(yytext); return T_id; }
{D}+		{ return T_num; }
\'({Esc}|[^\'\"\\])\'	{ return T_constchar; }
\"({Esc}|[^\'\"\r\n\\])*\"	{ return T_string; }

\n		{ linecount++; }
{W}+	{ /* nothing */ }
%.*\n	{ linecount++; }

"<*"			{ BEGIN(COMMENT); }
<COMMENT>"*>"	{ BEGIN(INITIAL); }
<COMMENT>"*"	{ /* nothing */ }
<COMMENT>\n		{ linecount++; }
<COMMENT>[^*\n]	{ /* nothing */ }

.       { char str[30];
		  strcpy(str, "Illegal token ");
		  yyerror(strcat(str, yytext));
		}

%%

void yyerror(char const *msg){
	fprintf(stderr, "error in line %d: %s\n", linecount, msg);
	exit(1);
}
