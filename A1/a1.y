//Keyword Tokens
%token	BREAK CASE CHAR CONTINUE DEFAULT DO DOUBLE
%token	ELSE EXTERN FLOAT FOR IF INT LONG RETURN SHORT
%token	SIZEOF STRUCT SWITCH VOID WHILE

//Identifier Token
%token	ID

//Token for constants
%token	INTCONSTANT FLOATCONSTANT

//String Token
%token	STRING

//Semicolon & COLON
%token SEMICOLON COLON

//Operator Tokens
%token	PLUS MINUS STAR DIV AND OR MOD XOR
%token	NOT TILDE
%token	LEQ GEQ LT GT THREEWAY ISEQ NEQ
%token	LOGAND LOGOR
%token	INCR DECR
%token	EQUAL 
%token	STRUCTPOINT STRUCTNORM
%token	LEFTBRACE RIGHTBRACE LEFTBRACKET RIGHTBRACKET COMMA LEFTCURLY RIGHTCURLY

//precedence and associativity rules
%left	LOGOR
%left	LOGAND
%left	OR
%left	XOR
%left	AND
%left	ISEQ NEQ
%left	LT GT GEQ LEQ THREEWAY
%left	PLUS MINUS
%left	STAR DIV MOD
%right	UNARY //dummy token to distinguish between binary and non-binary operators

%nonassoc IFX	//to remove the shift reduce conflict associated with the if-else ambiguity
%nonassoc ELSE

%union	//the semantic value union associated with the below tokens, used to generate the max if-else depth
{
	long long int value;
}

%type<value> if
%type<value> if_else
%type<value> statement
%type<value> while_loop
%type<value> do_while_loop
%type<value> for_loop
%type<value> new_scope
%type<value> switch_case_statement
%type<value> switch_case
%type<value> conditionals
%type<value> iterators
%type<value> statement_or_declaration_list
%type<value> statement_or_declaration
%type<value> function_definition
%type<value> function_definition_or_declaration
%type<value> program

%start c_program	//starting non-terminal

%{
	#include <stdio.h>
	#include <string.h>
	#include <stdlib.h>
	void yyerror(char *);
	int yylex(void);
	char mytext[100];
	extern char *yytext;
	long long int ifsWithoutElse = 0;
	long long int maxIfElseDepth = 0;
	long long int globalDeclarations = 0;
	long long int functionDefinitions = 0;
	long long int pointerDeclarations = 0;
	long long int integerConstants = 0;
	extern FILE* yyin;
	int commentError = 0;
%}

%%
//the first set of rules correspond to expression parsing
/*********************************************************************/
terminating_expr	
	: ID | LEFTBRACE expr RIGHTBRACE | STRING | INTCONSTANT | FLOATCONSTANT;

//highest precedence group and left associative
group_one
	: terminating_expr | group_one LEFTBRACKET expr RIGHTBRACKET | function_call 
	| group_one STRUCTNORM ID | group_one STRUCTPOINT ID | group_one INCR | group_one DECR;
	| LEFTBRACE type_name RIGHTBRACE LEFTCURLY init_list RIGHTCURLY
	| LEFTBRACE type_name RIGHTBRACE LEFTCURLY init_list COMMA RIGHTCURLY;

function_call	
	: group_one LEFTBRACE RIGHTBRACE | group_one LEFTBRACE expr RIGHTBRACE;

//second highest precedence and right associative
group_two
	: group_one | INCR group_two | DECR group_two | SIZEOF group_two 
	| SIZEOF LEFTBRACE type_name RIGHTBRACE
	| PLUS cast_expr %prec UNARY
	| MINUS cast_expr %prec UNARY
	| AND cast_expr %prec UNARY
	| TILDE cast_expr %prec UNARY
	| STAR cast_expr %prec UNARY
	| NOT cast_expr %prec UNARY;

cast_expr	: group_two | LEFTBRACE type_name RIGHTBRACE cast_expr
//next set of precedences and associativities
//includes binary +, *, &, |, &&, ||, %, <, >, <=, >=, <=> (new), ==, !=, ^
//these ambiguities are resolved using %left and %right token's described above
group_three	
	: cast_expr 
	| group_three PLUS group_three 
	| group_three DIV group_three 
	| group_three MINUS group_three 
	| group_three STAR group_three 
	| group_three MOD group_three
	| group_three XOR group_three
	| group_three AND  group_three 
	| group_three OR group_three 
	| group_three LOGAND group_three
	| group_three LOGOR group_three
	| group_three LT group_three
	| group_three GT group_three
	| group_three LEQ group_three 
	| group_three GEQ group_three
	| group_three THREEWAY group_three
	| group_three ISEQ group_three
	| group_three NEQ group_three;

//next precedence operator i.e. = is considered
//right associative
group_four
	: group_three | group_two EQUAL group_four;

//final group, contains the lowest precedence , operator and is left associative
expr
	: group_four | expr COMMA group_four;

//declarations //heavily reffered to from ANSI-C documentation for some rules
/*****************************************************************************/
declaration	
	: specifier_or_extern_list SEMICOLON // declarations like "int;" are valid in C
	| specifier_or_extern_list init_or_declarator_list SEMICOLON;

//binary supports arbitrary chaining of extern/type_specifier in declaration such as "long int float extern short extern long double a;"
specifier_or_extern_list
	: EXTERN specifier_or_extern_list | type_specifier specifier_or_extern_list | EXTERN | type_specifier;

//list of declarations or declarations with initialization
//comma is left associative
init_or_declarator_list
	: init_or_declarator 
	| init_or_declarator_list COMMA init_or_declarator;

init_or_declarator
	: declarator EQUAL init 
	| declarator;

//declarator must support all derived, primary and user data types
declarator
	: pointerlist decl //declarations with arbitrary level of pointer indirection
	{
		pointerDeclarations++;
	}
	| decl;	//plain declarations

//one or more pointer * applications
pointerlist
	: STAR %prec UNARY | pointerlist STAR %prec UNARY;

decl
	: ID 
	| LEFTBRACE declarator RIGHTBRACE //any bracing of a valid declarator is a valid, e.g: int **a -> int (*(*a))
	| decl LEFTBRACKET RIGHTBRACKET //arbitrary level of array declarations, e.g int *a[][][];
	| decl LEFTBRACKET group_four RIGHTBRACKET //can have any group_four expression inside, e.g int a[b=c=d=4];
	| decl LEFTBRACE parameter_list RIGHTBRACE //function declarations like f(int, int*) and other versions without variable names
	| decl LEFTBRACE RIGHTBRACE	//function declarations like void f();

type_name
	: specifier_list abstract_declarator
	| specifier_list;

parameter_list
	: paramater_declaration
	| parameter_list COMMA paramater_declaration;

paramater_declaration
	: specifier_or_extern_list declarator
	| specifier_or_extern_list 
	| specifier_or_extern_list abstract_declarator;

abstract_declarator
	: pointerlist direct_abstract_declarator
	| pointerlist
	| direct_abstract_declarator;

direct_abstract_declarator 
	: LEFTBRACE abstract_declarator RIGHTBRACE
	| LEFTBRACKET group_three RIGHTBRACKET //supports designated initialization, like {1, 2, 3, [10] = 10};
	| direct_abstract_declarator bracketed_group_four
	| direct_abstract_declarator braced_parameter_list;

braced_parameter_list
	: LEFTBRACE RIGHTBRACE
	| LEFTBRACE parameter_list RIGHTBRACE;

bracketed_group_four
	: LEFTBRACKET RIGHTBRACKET
	| LEFTBRACKET group_four RIGHTBRACKET;

init
	: LEFTCURLY init_list RIGHTCURLY //initializing arrays in C for arrays
	| LEFTCURLY init_list COMMA RIGHTCURLY //dangling , is valid C syntax.
	| group_four;

init_list 								
	: init	// this recursive nature is required to consider multi-dimentional arrays
	| init_list COMMA init;

type_specifier
	: VOID | CHAR | SHORT | INT | LONG | FLOAT | DOUBLE | struct_specifier;

struct_specifier
	: STRUCT LEFTCURLY struct_declaration_list RIGHTCURLY
	| STRUCT ID LEFTCURLY struct_declaration_list RIGHTCURLY
	| STRUCT ID;

struct_declaration_list
	: struct_declaration
	| struct_declaration_list struct_declaration;

struct_declaration
	: specifier_list SEMICOLON
	| specifier_list struct_declarator_list SEMICOLON;

struct_declarator_list
	: struct_declarator
	| struct_declarator_list COMMA struct_declarator;

struct_declarator
	:  COLON group_three
	| declarator COLON group_three
	| declarator;

specifier_list
	: type_specifier specifier_list
	| type_specifier;

//This section deals with statements
/*****************************************************************/
statement
	: SEMICOLON
	{
		$$ = 0;
	}
	| expr SEMICOLON
	{
		$$ = 0;
	}
	| iterators 
	{
		$$ = $1;
	}
	| conditionals
	{
		$$ = $1;
	}
	| new_scope
	{
		$$ = $1;
	}
	| switch_case_statement
	{
		$$ = $1;
	}
	| jumps
	{
		$$ = 0;
	};

new_scope
	: LEFTCURLY RIGHTCURLY
	{
		$$ = 0;
	}
	| LEFTCURLY statement_or_declaration_list RIGHTCURLY
	{
		$$ = $2;
	};

statement_or_declaration_list
	: statement_or_declaration
	{
		$$ = $1;
	}
	| statement_or_declaration_list statement_or_declaration
	{
		if($1 > $2)
		{
			$$ = $1;
		}
		else
		{
			$$ = $2;
		}
	};

statement_or_declaration
	: statement
	{
		$$ = $1;
	}
	| declaration
	{
		$$ = 0;
	};

jumps
	: CONTINUE SEMICOLON
	| BREAK SEMICOLON
	| RETURN SEMICOLON
	| RETURN expr SEMICOLON;

conditionals
	: if
	{
		$$ = $1;
	}
	| if_else
	{
		$$ = $1;
	}
	| switch_case
	{
		$$ = $1;
	};

if
	: IF LEFTBRACE expr RIGHTBRACE statement %prec IFX //removes 1 shift/reduce conflict associated with if-else ambiguity
	{
		$$ = $5;
		ifsWithoutElse++;
	};	

if_else
	: IF LEFTBRACE expr RIGHTBRACE statement ELSE statement
	{
		if($5 > $7 + 1)
		{
			$$ = $5;
		}
		else
		{
			$$ = $7 + 1;
		}
	};

switch_case
	: SWITCH LEFTBRACE expr RIGHTBRACE statement
	{
		$$ = $5;
	};

switch_case_statement
	: CASE group_three COLON statement
	{
		$$ = $4;
	}
	| DEFAULT COLON statement
	{
		$$ = $3;
	};

iterators
	: while_loop
	{
		$$ = $1;
	}
	| for_loop
	{
		$$ = $1;
	}
	| do_while_loop
	{
		$$ = $1;
	};

while_loop
	: WHILE LEFTBRACE expr RIGHTBRACE statement
	{
		$$ = $5;
	};

for_loop
	: FOR LEFTBRACE expr SEMICOLON expr SEMICOLON RIGHTBRACE statement
	{
		$$ = $8;
	}
	| FOR LEFTBRACE SEMICOLON expr SEMICOLON RIGHTBRACE statement
	{
		$$ = $7;
	}
	| FOR LEFTBRACE expr SEMICOLON SEMICOLON RIGHTBRACE statement
	{
		$$ = $7;
	}
	| FOR LEFTBRACE SEMICOLON SEMICOLON RIGHTBRACE statement
	{
		$$ = $6;
	}
	| FOR LEFTBRACE expr SEMICOLON expr SEMICOLON expr RIGHTBRACE statement
	{
		$$ = $9;
	}
	| FOR LEFTBRACE SEMICOLON expr SEMICOLON expr RIGHTBRACE statement
	{
		$$ = $8;
	}
	| FOR LEFTBRACE expr SEMICOLON SEMICOLON expr RIGHTBRACE statement
	{
		$$ = $8;
	}
	| FOR LEFTBRACE SEMICOLON SEMICOLON expr RIGHTBRACE statement
	{
		$$ = $7;
	}
	| FOR LEFTBRACE declaration expr SEMICOLON expr RIGHTBRACE statement
	{
		$$ = $8;
	}
	| FOR LEFTBRACE declaration SEMICOLON expr RIGHTBRACE statement
	{
		$$ = $7;
	}
	| FOR LEFTBRACE declaration expr SEMICOLON RIGHTBRACE statement
	{
		$$ = $7;
	}
	| FOR LEFTBRACE declaration SEMICOLON RIGHTBRACE statement
	{
		$$ = $6;
	};

do_while_loop
	: DO statement WHILE LEFTBRACE expr RIGHTBRACE SEMICOLON
	{
		$$ = $2;
	};

/******************************************************************/

//program is a sequence of function definitions or declarations
c_program
	: program 
	{
		maxIfElseDepth = $1;
	};

program
	: function_definition_or_declaration
	{
		$$ = $1;
	}
	| program function_definition_or_declaration
	{
		if($1 > $2)
		{
			$$ = $1;
		}
		else
		{
			$$ = $2;
		}
	};

function_definition_or_declaration
	: function_definition
	{
		$$ = $1;
		globalDeclarations++;
		functionDefinitions++;
	}
	| declaration
	{
		$$ = 0;
		globalDeclarations++;
	};

function_definition
	: specifier_or_extern_list declarator new_scope
	{
		$$ = $3;
	};
%%
void yyerror(char *s) 
{
	if(commentError != 1)
    	printf("***parsing terminated*** [syntax error]\n");
}

int main(int argc, char* argv[]) 
{
	if(argc != 2)
	{
		printf("***process terminated*** [input error]: invalid number of command-line arguments\n");
		return 0;
	}
	FILE* f = fopen(argv[1], "r");
	if(f == NULL)
	{
		printf("***process terminated*** [input error]: no such file %s exists\n", argv[1]);
		return 0;
	}
	yyin = f;
    int a = yyparse();
	if(a == 0)
	{
		printf("***parsing successful***\n");
		printf("#global_declarations = %lli\n", globalDeclarations);
		printf("#function_definitions = %lli\n", functionDefinitions);
		printf("#integer_constants = %lli\n", integerConstants);
		printf("#pointers_declarations = %lli\n", pointerDeclarations);
		printf("#ifs_without_else = %lli\n", ifsWithoutElse);
		printf("if-else max-depth = %lli\n", maxIfElseDepth);
	}
    return 0;
}

