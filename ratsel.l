%option yylineno

%{
	#include<stdio.h>
	#include<string.h>
	#include"y.tab.h"
	#include<math.h>
	extern int var_count;
	

%}

%%
"#include<stdio.h>" {return HEADER;}
"if"	{printf("usage of keyword \"%s\" is prohibited\n",yytext);exit(0);}
"int"	{yylval.data_type=0;return INT;}
"str" {yylval.data_type=1; return STR;}
"dec"	{yylval.data_type=3; return DEC;}
"tf"  {yylval.data_type=4; return TF;}
"list" {yylval.data_type=5; return LIST;}
"arr"  {yylval.data_type=6; return ARRAY;}
"main"	{return MAIN;}
"("	{return LB;}
")"	{return RB;}
"{"	{return LCB;}
"}"	{return RCB;}
"["     {return LSB;}
"]"     {return RSB;} 
","	{return COMA;}
";"	{return SC;}
":"     {return CO;}
"+" {return ADD;}
"-" {return SUBTRACT;}
"/" {return DIV;}
"*" {return MULTIPLY;}
"wenn"  {return WENN;}
"dann" {return DANN;}
"otherwise" {return OTHERWISE;}
"wenndann" {return WENNDANN;}
"when" {return WHEN;}
"then" {return THEN;}
"endwhen" {return ENDWHEN;}
"recur" {return RECUR;}
"upto" {return UPTO;}
"flo" {return FLO;}
"AND" {return AND;}
"OR" {return OR;}
"~" {return NOT;}
"==" {return ET;}  
"~=" {return NET;}
"<" {return LT;} 
">" {return GT;} 
"<=" {return LET;}
">=" {return GET;}
"="	{return EQ;}
"out" {return OUT;}
"continue" {return CONTINUE;}
"lesen" {return LESEN;}
"aus" {return AUS;}
"Func" {return FUNC;} 
"Start" {return START;}
"Finish" {return FINISH;}
"ret" {return RET;}
"Replace" {return REPLACE;}
"call" {return CALL;}
[\"] {return QUOTES;}
"MkOwn" {return MKOWN;}
[a-zA-Z]+  {strcpy(yylval.var_name,yytext);return VAR;}
[0-9]+(\.[0-9]+)? {yylval.var_num=atoi(yytext); return VAR_NUM;}
[\n\t ]+  {/*new line or space*/}
. {printf("invalid character sequence %s\n",yytext); exit(0);}
%%


int yywrap(void)
{
    return 1;
}

