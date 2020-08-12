#ifndef __LEXER_HPP__
#define __LEXER_HPP__
#include <string>

int yylex();
void yyerror(char const *msg);

extern int linecount;

#endif
