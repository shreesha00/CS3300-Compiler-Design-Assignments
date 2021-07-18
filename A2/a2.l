escape_sequence (\\(x{hex_digit}+|{octal_digit}|{octal_digit}{octal_digit}|{octal_digit}{octal_digit}{octal_digit}|['"\?\\abfnrtv]))
non_zero_digit [1-9]
digit [0-9]
octal_digit [0-7] 
hex_digit [0-9a-fA-F]
unsigned_suffix (u|U)
l_suffix (l|L)
ll_suffix (ll|LL)
hex_prefix (0x|0X)
int_suffix ({unsigned_suffix}{l_suffix}?|{unsigned_suffix}{ll_suffix}?|{l_suffix}{unsigned_suffix}?|{ll_suffix}{unsigned_suffix}?)
decimal_constant ({non_zero_digit}{digit}*)
octal_constant ("0"{octal_digit}*)
hex_constant ({hex_prefix}{hex_digit}+)
int_constant (({decimal_constant}|{octal_constant}|{hex_constant}){int_suffix}?)
float_suffix (f|l|F|L)
hex_digit_sequence ({hex_digit}+)
sign [+-]
binary_exp_part ([p|P]{sign}?{digit}+)
hex_frac_constant ({hex_digit_sequence}?"."{hex_digit_sequence}|{hex_digit_sequence}".")
digit_sequence ({digit}+)
exponent_part ([e|E]{sign}?{digit_sequence})
frac_constant ({digit_sequence}?"."{digit_sequence}|{digit_sequence}".")
hex_fp_constant ({hex_prefix}{hex_frac_constant}{binary_exp_part}{float_suffix}?|{hex_prefix}{hex_digit_sequence}{binary_exp_part}{float_suffix}?)
dec_fp_constant ({frac_constant}{exponent_part}?{float_suffix}?|{digit_sequence}{exponent_part}{float_suffix}?)
fp_constant ({dec_fp_constant}|{hex_fp_constant})
char_prefix (u|U|L)
char_constant ({char_prefix}?"'"([^\\\n]|{escape_sequence})+"'")
%{
	#include <stdlib.h>
	#include "y.tab.h"
	void yyerror(char *);
	extern char mytext[];
	int commentError = 0;
%}

%%
"/*"													{
															commentError = 1;
															//because comments need to reported, we need to have a C section to catch errors here
															int c;
															while ((c = input()) != 0)	//keep reading input as long as end of file is not obtained
															{
																if(c == '*')	//if a "*" is encountered, go into this if
																{
																	while((c = input()) == '*');	//consume any subsequent "*"'s if any
																	if(c == '/')	//if next charecter is "/", comment successfully consumed
																		break;
																	//else continue consuming further charecters till either end of file or "*" is obtained again
																}
															}
															commentError = 0; 
														}
"//".*													{ /*consume everything after // */ }
"break"													{ return BREAK; }
"case"													{ return CASE; }
"continue"												{ return CONTINUE; }
"default"												{ return DEFAULT; }
"do" 													{ return DO; }
"else"													{ return ELSE; }
"extern"												{ return EXTERN; }
"float"													{ return FLOAT; }
"if"													{ return IF; }
"int"													{ return INT; }
"return"												{ return RETURN; }
"sizeof"												{ return SIZEOF; }
"struct"												{ return STRUCT; }
"switch"												{ return SWITCH; }
"void"													{ return VOID; }
"while"													{ return WHILE; }
"printf"                                                { return PRINTF; }
"\"%d\""                                     			{ return FRMT_SPEC; }
[a-zA-Z_][a-zA-Z_0-9]*									{ 
															strcpy(mytext, yytext);
															return ID; 
														}
{fp_constant}											{ return FLOATCONSTANT; }	//followed the documentation for these rules
{int_constant}											{ return INTCONSTANT; }	//followed the documentation for these rules
{char_constant}											{ return INTCONSTANT; } //followed the official documentation
"<="													{ return LEQ; }		//leq
">=" 													{ return GEQ; }		//geq
"==" 													{ return ISEQ; }	//equality check
"<=>"													{ return THREEWAY; }//three way comp
"!="													{ return NEQ; }		//neq
"&&"													{ return LOGAND; }	//logical and
"||"													{ return LOGOR; }	//logical or
"++"													{ return INCR; }	//post increment
"--"													{ return DECR; }	//post decrement
"->" 													{ return STRUCTPOINT; }	//structure pointer access
"+"														{ return PLUS; }	//addition
"-"														{ return MINUS; }	//subtraction
"*" 													{ return STAR; }	//multiplication / unary dereference
"&" 													{ return AND; }		//binary and / unary address of
"/" 													{ return DIV; }		//division
"%" 													{ return MOD; }		//modulo
"!" 													{ return NOT; }		//unary not
"<" 													{ return LT; } 		//less than
">"														{ return GT; } 		//greater than
"="														{ return EQUAL; }	//assignment
"." 													{ return STRUCTNORM; }	//structure field access
"("														{ return LEFTBRACE; }	//left braces
")"														{ return RIGHTBRACE; }	//right braces
"["														{ return LEFTBRACKET; }	//array subscript left
"]"														{ return RIGHTBRACKET; }	//array subscript right
"{"														{ return LEFTCURLY; }	//left curly braces
"}"														{ return RIGHTCURLY; }	//right curly brace
"," 													{ return COMMA; }	//comma
";"														{ return SEMICOLON; }	//semicolon
":"														{ return COLON; }	//colon
[ \t\n\v\f]+											{ /* whitespace */ } 
.														{ printf("error\n"); exit(0); }
%%

int yywrap(void) 
{
	if(commentError == 1)
	{
		printf("lexer error\n");
		exit(0);
	}
	return 1;
	
}