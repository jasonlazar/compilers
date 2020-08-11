%option noyywrap
%x COMMENT

%{
enum tokens {
	T_eof = 0,
	T_and = 1001,
	T_bool,
	T_char,
	T_decl,
	T_def,
	T_else,
	T_elsif,
	T_end,
	T_exit,
	T_false,
	T_for,
	T_head,
	T_if,
	T_int,
	T_list,
	T_mod,
	T_new,
	T_nil,
	T_isnil,
	T_not,
	T_or,
	T_ref,
	T_return,
	T_skip,
	T_tail,
	T_true,
	T_id,
	T_num,
	T_constchar,
	T_string
};

enum operators {
	T_add = 1031,
	T_sub,
	T_mul,
	T_div,
	T_cons,
	T_eq,
	T_neq,
	T_less,
	T_greater,
	T_leq,
	T_geq,
	T_lpar,
	T_rpar,
	T_lbr,
	T_rbr,
	T_comma,
	T_sem,
	T_col,
	T_assign
};
%}

L	[A-Za-z]
D	[0-9]
Hex	({D}|[A-Fa-f])
Esc	\\([tnr0\'\"\\]|x{Hex}{Hex})
W	[ \r\n\t]

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

{L}({L}|{D}|[_?])*	{ return T_id; }
{D}+		{ return T_num; }
\'({Esc}|[^\'\"\\])\'	{ return T_constchar; }
\"({Esc}|[^\'\"\r\n\\])*\"	{ return T_string; }

{W}+	{ /* nothing */ }
%.*\n	{ /* nothing */ }

"<*"			{ BEGIN(COMMENT); }
<COMMENT>"*>"	{ BEGIN(INITIAL); }
<COMMENT>"*"	{ /* nothing */ }
<COMMENT>[^*]	{ /* nothing */ }

.       { fprintf(stderr, "Illegal character with code %d\n", yytext[0]);
          exit(1);
        }
%%

int main () {
  int token;
  do {
    token = yylex();
    printf("token=%d, lexeme=\"%s\"\n", token, yytext);
  } while (token != T_eof);
}
