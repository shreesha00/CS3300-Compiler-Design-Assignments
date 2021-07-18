/*
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
*/

%union 
{
    struct node* n;
}

%start program

%{
	#include <stdio.h>
	#include <string.h>
	#include <stdlib.h>
    #define MAX 10
	void yyerror(char *);
	int yylex(void);
	char mytext[100];
	extern char *yytext;
    extern int commentError;
    struct node
    {
        char label[100]; //label name
        int isLeaf; //isLeaf = 1 -> Leaf node. Else isLeaf = 0;
        struct node *children[MAX]; //at max 10 children for each node
        long long int depth; //depth represents the height of it's subtree.
        long long int lp; //longest path in subtree
    };
    //int yydebug = 1;
    inline long long int max(long long int a, long long int b)
    {
        if(a > b)
        {
            return a;
        }
        else
        {
            return b;
        }
    }
    void printtree(struct node* n, long long int level)
    {
        for(int i = 0; i < level ; i++)
        {
            printf(".");
        }
        printf("%s\n", n->label);
        if(n->isLeaf == 1)
        {
            return;
        }
        else
        {
            for(int i = 0; i < MAX && n->children[i] != NULL; i++)
            {
                printtree(n->children[i], level + 1);
            }
        }
    }
    long long int longestPath = 0;
    long long int longestPathIf = 0;
    long long int longestPathWhile = 0;
    long long int longestPathSwitch = 0;
    long long int longestPathMain = 0;
    int isMain = 0;
%}

//Keyword Tokens
%token<n> BREAK CASE CONTINUE DEFAULT DO
%token<n> ELSE EXTERN FLOAT IF INT RETURN
%token<n> SIZEOF STRUCT SWITCH VOID WHILE

//Token for printf and format specifier
%token<n> PRINTF FRMT_SPEC

//Identifier Token
%token<n> ID

//Token for constants
%token<n> INTCONSTANT FLOATCONSTANT

//Semicolon  COLON
%token<n> SEMICOLON COLON

//Operator Tokens
%token<n> PLUS MINUS STAR DIV AND MOD
%token<n> NOT
%token<n> LEQ GEQ LT GT THREEWAY ISEQ NEQ
%token<n> LOGAND LOGOR
%token<n> INCR DECR
%token<n> EQUAL 
%token<n> STRUCTPOINT STRUCTNORM
%token<n> LEFTBRACE RIGHTBRACE LEFTBRACKET RIGHTBRACKET COMMA LEFTCURLY RIGHTCURLY

//non-terminals
%type<n> program;
%type<n> decl_list;
%type<n> decl;
%type<n> struct_decl;
%type<n> var_decl;
%type<n> type_spec;
%type<n> extern_spec;
%type<n> func_decl;
%type<n> params;
%type<n> param_list;
%type<n> param;
%type<n> stmt_list;
%type<n> stmt;
%type<n> while_stmt;
%type<n> dowhile_stmt;
%type<n> print_stmt;
//%type<n> expr_stmt;
%type<n> format_specifier;
%type<n> compound_stmt;
%type<n> local_decls;
%type<n> local_decl;
%type<n> if_stmt;
%type<n> return_stmt;
%type<n> break_stmt;
%type<n> continue_stmt;
%type<n> switch_stmt;
%type<n> compound_case;
%type<n> single_case;
%type<n> default_case;
%type<n> assign_stmt;
%type<n> Pexpr;
%type<n> incr_stmt;
%type<n> decr_stmt;
%type<n> expr;
%type<n> arg_list;
%type<n> args;
%type<n> integerLit;
%type<n> floatLit;
%type<n> identifier; 

%nonassoc IFX	//to remove the shift reduce conflict associated with the if-else ambiguity
%nonassoc ELSE
%%
program
    : decl_list
    { 
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "program",
            .isLeaf = 0, 
            .children = {[0] = $1},
            .depth = ($1)->depth + 1,
            .lp = max(($1)->lp, ($1)->depth + 1)
        };
        $$ = a;
        //printtree($$, 0);
        longestPath = $$->lp;
    };

decl_list
    : decl 
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "decl_list",
            .isLeaf = 0, 
            .children = {[0] = $1},
            .depth = ($1)->depth + 1,
            .lp = max(($1)->lp, ($1)->depth + 1)
        };
        $$ = a;
    }
    | decl_list decl
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "decl_list",
            .isLeaf = 0, 
            .children = {[0] = $1, [1] = $2},
            .depth = max(($1)->depth, ($2)->depth) + 1,
            .lp = max(max(($1)->lp, ($2)->lp), ($1)->depth + ($2)->depth + 1)
        };
        $$ = a;
    };

decl
    : var_decl
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "decl",
            .isLeaf = 0, 
            .children = {[0] = $1},
            .depth = ($1)->depth + 1,
            .lp = max(($1)->lp, ($1)->depth + 1)
        };
        $$ = a;
    }
    | func_decl
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "decl",
            .isLeaf = 0, 
            .children = {[0] = $1},
            .depth = ($1)->depth + 1,
            .lp = max(($1)->lp, ($1)->depth + 1)
        };
        $$ = a;
    }
    | struct_decl
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "decl",
            .isLeaf = 0, 
            .children = {[0] = $1},
            .depth = ($1)->depth + 1,
            .lp = max(($1)->lp, ($1)->depth + 1)
        };
        $$ = a;
    };

struct_decl
    : STRUCT identifier LEFTCURLY local_decls RIGHTCURLY SEMICOLON
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "STRUCT",
            .isLeaf = 1, 
            .children = {},
            .depth = 1,
            .lp = 1
        };
        $1 = a; 
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "LEFTCURLY",
            .isLeaf = 1, 
            .children = {},
            .depth = 1,
            .lp = 1
        };
        $3 = b;
        struct node *c = (struct node*)malloc(sizeof(struct node));
        *c = (struct node)
        {
            .label = "RIGHTCURLY",
            .isLeaf = 1, 
            .children = {},
            .depth = 1,
            .lp = 1
        };
        $5 = c;
        struct node *d = (struct node*)malloc(sizeof(struct node));
        *d = (struct node)
        {
            .label = "SEMICOLON",
            .isLeaf = 1, 
            .children = {},
            .depth = 1,
            .lp = 1
        };
        $6 = d;
        struct node *e = (struct node*)malloc(sizeof(struct node));
        *e = (struct node)
        {
            .label = "struct_decl",
            .isLeaf = 0, 
            .children = {[0] = $1, [1] = $2, [2] = $3, [3] = $4, [4] = $5, [5] = $6},
            .depth = max(($2)->depth, ($4)->depth) + 1,
            .lp = max(max(($2)->lp, ($4)->lp), ($2)->depth + ($4)->depth + 1)
        };
        $$ = e;
    };

var_decl
    : type_spec identifier SEMICOLON
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "SEMICOLON",
            .isLeaf = 1, 
            .children = {},
            .depth = 1,
            .lp = 1
        };
        $3 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "var_decl",
            .isLeaf = 0, 
            .children = {[0] = $1, [1] = $2, [2] = $3},
            .depth = max(($1)->depth, ($2)->depth) + 1,
            .lp = max(max(($1)->lp, ($2)->lp), ($1)->depth + ($2)->depth + 1)
        };
        $$ = b;
    }
    | type_spec identifier COMMA var_decl
    {
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "COMMA",
            .isLeaf = 1, 
            .children = {},
            .depth = 1,
            .lp = 1
        };
        $3 = b;
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "var_decl",
            .isLeaf = 0, 
            .children = {[0] = $1, [1] = $2, [2] = $3, [3] = $4},
            .depth = max(($1)->depth, ($4)->depth) + 1,
            .lp = max(max(($1)->lp, ($4)->lp), ($1)->depth + ($4)->depth + 1)
        };
        $$ = a;
    }
    | type_spec identifier LEFTBRACKET integerLit RIGHTBRACKET SEMICOLON
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "LEFTBRACKET",
            .isLeaf = 1,
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $3 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "RIGHTBRACKET",
            .isLeaf = 1,
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $5 = b;
        struct node *c = (struct node*)malloc(sizeof(struct node));
        *c = (struct node)
        {
            .label = "SEMICOLON",
            .isLeaf = 1,
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $6 = c;
        struct node *d = (struct node*)malloc(sizeof(struct node));
        *d = (struct node)
        {
            .label = "var_decl",
            .isLeaf = 0,
            .children = {[0] = $1, [1] = $2, [2] = $3, [3] = $4, [4] = $5, [5] = $6},
            .depth = max(($1)->depth, ($4)->depth) + 1,
            .lp = max(max(($1)->lp, ($4)->lp), ($1)->depth + ($4)->depth + 1)
        };
        $$ = d;
    }
    | type_spec identifier LEFTBRACKET integerLit RIGHTBRACKET COMMA var_decl
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "LEFTBRACKET",
            .isLeaf = 1,
            .children = {},
            .depth = 1,
            .lp = 1
        };
        $3 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "RIGHTBRACKET",
            .isLeaf = 1,
            .children = {},
            .depth = 1,
            .lp = 1
        };
        $5 = b;
        struct node *c = (struct node*)malloc(sizeof(struct node));
        *c = (struct node)
        {
            .label = "COMMA",
            .isLeaf = 1,
            .children = {},
            .depth = 1,
            .lp = 1
        };
        $6 = c;
        struct node *d = (struct node*)malloc(sizeof(struct node));
        *d = (struct node)
        {
            .label = "var_decl",
            .isLeaf = 0,
            .children = {[0] = $1, [1] = $2, [2] = $3, [3] = $4, [4] = $5, [5] = $6, [6] = $7},
            .depth = max(($1)->depth, ($7)->depth) + 1,
            .lp = max(max(($1)->lp, ($7)->lp), ($1)->depth + ($7)->depth + 1)
        };
        $$ = d;
    };

type_spec
    : extern_spec VOID 
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "VOID",
            .isLeaf = 1,
            .children = {},
            .depth = 1,
            .lp = 1
        };
        $2 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "type_spec",
            .isLeaf = 0,
            .children = {[0] = $1, [1] = $2},
            .depth = 3,
            .lp = 4
        };
        $$ = b;
    }
    | extern_spec INT
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "INT",
            .isLeaf = 1,
            .children = {},
            .depth = 1,
            .lp = 1
        };
        $2 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "type_spec",
            .isLeaf = 0,
            .children = {[0] = $1, [1] = $2},
            .depth = 3,
            .lp = 4
        };
        $$ = b;
    }
    | extern_spec FLOAT
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "FLOAT",
            .isLeaf = 1,
            .children = {},
            .depth = 1,
            .lp = 1
        };
        $2 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "type_spec",
            .isLeaf = 0,
            .children = {[0] = $1, [1] = $2},
            .depth = 3,
            .lp = 4
        };
        $$ = b;
    }
    | extern_spec VOID STAR
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "VOID",
            .isLeaf = 1,
            .children = {},
            .depth = 1,
            .lp = 1
        };
        $2 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "STAR",
            .isLeaf = 1,
            .children = {},
            .depth = 1,
            .lp = 1
        };
        $3 = b;
        struct node *c = (struct node*)malloc(sizeof(struct node));
        *c = (struct node)
        {
            .label = "type_spec",
            .isLeaf = 0,
            .children = {[0] = $1, [1] = $2, [2] = $3},
            .depth = 3,
            .lp = 4
        };
        $$ = c;
    }
    | extern_spec INT STAR
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "INT",
            .isLeaf = 1,
            .children = {},
            .depth = 1,
            .lp = 1
        };
        $2 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "STAR",
            .isLeaf = 1,
            .children = {},
            .depth = 1,
            .lp = 1
        };
        $3 = b;
        struct node *c = (struct node*)malloc(sizeof(struct node));
        *c = (struct node)
        {
            .label = "type_spec",
            .isLeaf = 0,
            .children = {[0] = $1, [1] = $2, [2] = $3},
            .depth = 3,
            .lp = 4
        };
        $$ = c;
    }
    | extern_spec FLOAT STAR
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "FLOAT",
            .isLeaf = 1,
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $2 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "STAR",
            .isLeaf = 1,
            .children = {},
            .depth = 1,
            .lp = 1
        };
        $3 = b;
        struct node *c = (struct node*)malloc(sizeof(struct node));
        *c = (struct node)
        {
            .label = "type_spec",
            .isLeaf = 0,
            .children = {[0] = $1, [1] = $2, [2] = $3},
            .depth = 3,
            .lp = 4
        };
        $$ = c;
    }
    | STRUCT identifier 
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "STRUCT",
            .isLeaf = 1,
            .children = {},
            .depth = 1,
            .lp = 1
        };
        $1 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "type_spec",
            .isLeaf = 0,
            .children = {[0] = $1, [1] = $2},
            .depth = 3,
            .lp = 4
        };
        $$ = b;
    }
    | STRUCT identifier STAR
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "STRUCT",
            .isLeaf = 1,
            .children = {},
            .depth = 1,
            .lp = 1
        };
        $1 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "STAR",
            .isLeaf = 1,
            .children = {},
            .depth = 1,
            .lp = 1
        };
        $3 = b;
        struct node *c = (struct node*)malloc(sizeof(struct node));
        *c = (struct node)
        {
            .label = "type_spec",
            .isLeaf = 0,
            .children = {[0] = $1, [1] = $2, [2] = $3},
            .depth = 3,
            .lp = 4
        };
        $$ = c;
    };

extern_spec
    : /*empty*/
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "epsilon",
            .isLeaf = 1,
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "extern_spec",
            .isLeaf = 0,
            .children = {[0] = a},
            .depth = 2,
            .lp = 2
        };
        $$ = b;
    }
    | EXTERN
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "EXTERN",
            .isLeaf = 1,
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $1 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "extern_spec",
            .isLeaf = 0,
            .children = {[0] = $1},
            .depth = 2,
            .lp = 2
        };
        $$ = b;
    };

func_decl
    : type_spec identifier { if(strcmp(mytext, "main") == 0) isMain = 1; } LEFTBRACE params RIGHTBRACE compound_stmt
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "LEFTBRACE",
            .isLeaf = 1, 
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $4 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "RIGHTBRACE",
            .isLeaf = 1, 
            .children = {},
            .depth = 1,
            .lp = 1
        };
        $6 = b;
        struct node *c = (struct node*)malloc(sizeof(struct node));
        *c = (struct node)
        {
            .label = "func_decl",
            .isLeaf = 0, 
            .children = {[0] = $1, [1] = $2, [2] = $4, [3] = $5, [4] = $6, [5] = $7},
            .depth = max(max(($1)->depth, ($5)->depth), ($7)->depth) + 1,
            .lp = max(max(max(max(max(($1)->lp, ($5)->lp), ($7)->lp), ($1)->depth + ($5)->depth + 1), ($5)->depth + ($7)->depth + 1), ($1)->depth + ($7)->depth + 1)
        };
        $$ = c;
        if(isMain == 1)
        {
            longestPathMain = ($$)->lp;
            isMain = 0;
        }
    };

params
    : /*empty*/
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "epsilon",
            .isLeaf = 1,
            .children = {},
            .depth = 1,
            .lp = 1
        };
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "params",
            .isLeaf = 0,
            .children = {[0] = a},
            .depth = 2,
            .lp = 2
        };
        $$ = b;
    }
    | param_list
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "params",
            .isLeaf = 0,
            .children = {[0] = $1},
            .depth = ($1)->depth + 1, 
            .lp = max(($1)->lp, ($1)->depth + 1)
        };
        $$ = a;
    };

param_list
    : param_list COMMA param
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "COMMA",
            .isLeaf = 1,
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $2 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "param_list",
            .isLeaf = 0,
            .children = {[0] = $1, [1] = $2, [2] = $3},
            .depth = max(($1)->depth, ($3)->depth) + 1, 
            .lp = max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1)
        };
        $$ = b;
    }
    | param
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "param_list",
            .isLeaf = 0,
            .children = {[0] = $1}, 
            .depth = ($1)->depth + 1, 
            .lp = max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
    };

param
    : type_spec identifier 
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "param",
            .isLeaf = 0,
            .children = {[0] = $1, [1] = $2}, 
            .depth = max(($1)->depth, ($2)->depth) + 1, 
            .lp = max(max(($1)->lp, ($2)->lp), ($1)->depth + ($2)->depth + 1)
        };
        $$ = a;
    }
    | type_spec identifier LEFTBRACKET RIGHTBRACKET
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "LEFTBRACKET",
            .isLeaf = 1,
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $3 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "RIGHTBRACKET",
            .isLeaf = 1,
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $4 = b;
        struct node *c = (struct node*)malloc(sizeof(struct node));
        *c = (struct node)
        {
            .label = "param",
            .isLeaf = 0,
            .children = {[0] = $1, [1] = $2, [2] = $3, [3] = $4}, 
            .depth = max(($1)->depth, ($2)->depth) + 1, 
            .lp = max(max(($1)->lp, ($2)->lp), ($1)->depth + ($2)->depth + 1)
        };
        $$ = c;
    };

stmt_list
    : stmt_list stmt
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "stmt_list", 
            .isLeaf = 0, 
            .children = {[0] = $1, [1] = $2},
            .depth = max(($1)->depth, ($2)->depth) + 1, 
            .lp = max(max(($1)->lp, ($2)->lp), ($1)->depth + ($2)->depth + 1)
        };
        $$ = a;
    }
    | stmt
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "stmt_list", 
            .isLeaf = 0, 
            .children = {[0] = $1},
            .depth = ($1)->depth + 1, 
            .lp = max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
    };

stmt
    : assign_stmt
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "stmt", 
            .isLeaf = 0, 
            .children = {[0] = $1},
            .depth = ($1)->depth + 1, 
            .lp = max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
    }
    | compound_stmt
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "stmt", 
            .isLeaf = 0, 
            .children = {[0] = $1},
            .depth = ($1)->depth + 1, 
            .lp = max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
    }
    | if_stmt
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "stmt", 
            .isLeaf = 0, 
            .children = {[0] = $1},
            .depth = ($1)->depth + 1, 
            .lp = max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
        if(($$)->lp > longestPathIf)
        {
            longestPathIf = ($$)->lp;
        }
    }
    | while_stmt
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "stmt", 
            .isLeaf = 0, 
            .children = {[0] = $1},
            .depth = ($1)->depth + 1, 
            .lp = max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
        if(($$)->lp > longestPathWhile)
        {
            longestPathWhile = ($$)->lp;
        }
    }
    | switch_stmt
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "stmt", 
            .isLeaf = 0, 
            .children = {[0] = $1},
            .depth = ($1)->depth + 1, 
            .lp = max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
        if(($$)->lp > longestPathSwitch)
        {
            longestPathSwitch = ($$)->lp;
        }
    }
    | return_stmt
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "stmt", 
            .isLeaf = 0, 
            .children = {[0] = $1},
            .depth = ($1)->depth + 1, 
            .lp = max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
    }
    | break_stmt
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "stmt", 
            .isLeaf = 0, 
            .children = {[0] = $1},
            .depth = ($1)->depth + 1, 
            .lp = max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
    }
    | continue_stmt
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "stmt", 
            .isLeaf = 0, 
            .children = {[0] = $1},
            .depth = ($1)->depth + 1, 
            .lp = max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
    }
    | dowhile_stmt
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "stmt", 
            .isLeaf = 0, 
            .children = {[0] = $1},
            .depth = ($1)->depth + 1, 
            .lp = max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
    }
    | print_stmt
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "stmt", 
            .isLeaf = 0, 
            .children = {[0] = $1},
            .depth = ($1)->depth + 1, 
            .lp = max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
    }
    | incr_stmt 
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "stmt", 
            .isLeaf = 0, 
            .children = {[0] = $1},
            .depth = ($1)->depth + 1, 
            .lp = max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
    }
    | decr_stmt
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "stmt", 
            .isLeaf = 0, 
            .children = {[0] = $1},
            .depth = ($1)->depth + 1, 
            .lp = max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
    };
    /*| expr_stmt
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "stmt", 
            .isLeaf = 0, 
            .children = {[0] = $1},
            .depth = ($1)->depth + 1, 
            .lp = max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
    };

/*
expr_stmt
    : expr SEMICOLON
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "SEMICOLON",
            .isLeaf = 1, 
            .children = {}, 
            .depth = 1, 
            .lp = 1
        };
        $1 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "expr_stmt",
            .isLeaf = 0, 
            .children = {[0] = $1, [1] = $2},
            .depth = max(($1)->depth, ($2)->depth) + 1,
            .lp = max(($1)->depth + 2, ($1)->lp)
        };
        $$ = b;
    };
*/
while_stmt
    : WHILE LEFTBRACE expr RIGHTBRACE stmt
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "WHILE",
            .isLeaf = 1, 
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $1 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "LEFTBRACE",
            .isLeaf = 1, 
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $2 = b;
        struct node *c = (struct node*)malloc(sizeof(struct node));
        *c = (struct node)
        {
            .label = "RIGHTBRACE",
            .isLeaf = 1, 
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $4 = c;
        struct node *d = (struct node*)malloc(sizeof(struct node));
        *d = (struct node)
        {
            .label = "while_stmt",
            .isLeaf = 0,
            .children = {[0] = $1, [1] = $2, [2] = $3, [3] = $4, [4] = $5},
            .depth = max(($3)->depth, ($5)->depth) + 1,
            .lp = max(max(($3)->lp, ($5)->lp), ($3)->depth + ($5)->depth + 1)
        };
        $$ = d;
    };

dowhile_stmt
    : DO stmt WHILE LEFTBRACE expr RIGHTBRACE SEMICOLON
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "DO",
            .isLeaf = 1,
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $1 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "WHILE",
            .isLeaf = 1,
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $3 = b;
        struct node *c = (struct node*)malloc(sizeof(struct node));
        *c = (struct node)
        {
            .label = "LEFTBRACE",
            .isLeaf = 1,
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $4 = c;
        struct node *d = (struct node*)malloc(sizeof(struct node));
        *d = (struct node)
        {
            .label = "RIGHTBRACE",
            .isLeaf = 1,
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $6 = d;
        struct node *e = (struct node*)malloc(sizeof(struct node));
        *e = (struct node)
        {
            .label = "SEMICOLON",
            .isLeaf = 1,
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $7 = e;
        struct node *f = (struct node*)malloc(sizeof(struct node));
        *f = (struct node)
        {
            .label = "dowhile_stmt",
            .isLeaf = 0,
            .children = {[0] = $1, [1] = $2, [2] = $3, [3] = $4, [4] = $5, [5] = $6, [6] = $7},
            .depth = max(($2)->depth, ($5)->depth) + 1, 
            .lp = max(max(($2)->lp, ($5)->lp), ($2)->depth + ($5)->depth + 1)
        };
        $$ = f;
    };

print_stmt
    : PRINTF LEFTBRACE format_specifier COMMA identifier RIGHTBRACE SEMICOLON
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "PRINTF",
            .isLeaf = 1,
            .children = {},
            .depth = 1,
            .lp = 1
        };
        $1 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "LEFTBRACE",
            .isLeaf = 1,
            .children = {},
            .depth = 1,
            .lp = 1
        };
        $2 = b;
        struct node *c = (struct node*)malloc(sizeof(struct node));
        *c = (struct node)
        {
            .label = "COMMA",
            .isLeaf = 1,
            .children = {},
            .depth = 1,
            .lp = 1
        };
        $4 = c;
        struct node *d = (struct node*)malloc(sizeof(struct node));
        *d = (struct node)
        {
            .label = "RIGHTBRACE",
            .isLeaf = 1,
            .children = {},
            .depth = 1,
            .lp = 1
        };
        $6 = d;
        struct node *e = (struct node*)malloc(sizeof(struct node));
        *e = (struct node)
        {
            .label = "SEMICOLON",
            .isLeaf = 1,
            .children = {},
            .depth = 1,
            .lp = 1
        };
        $7 = e;
        struct node *f = (struct node*)malloc(sizeof(struct node));
        *f = (struct node)
        {
            .label = "print_stmt",
            .isLeaf = 0,
            .children = {[0] = $1, [1] = $2, [2] = $3, [3] = $4, [4] = $5, [5] = $6, [6] = $7},
            .depth = max(($3)->depth, ($5)->depth) + 1,
            .lp = max(max(($3)->lp, ($5)->lp), ($3)->depth + ($5)->depth + 1)
        };
        $$ = f;
    };

format_specifier
    : FRMT_SPEC
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "FRMT_SPEC",
            .isLeaf = 1,
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $1 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "format_specifier",
            .isLeaf = 0,
            .children = {[0] = $1},
            .depth = 2, 
            .lp = 2
        };
        $$ = b;
    };

compound_stmt  
    : LEFTCURLY local_decls stmt_list RIGHTCURLY
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "LEFTCURLY",
            .isLeaf = 1,
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $1 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "RIGHTCURLY",
            .isLeaf = 1,
            .children = {},
            .depth = 1,
            .lp = 1
        };
        $4 = b;
        struct node *c = (struct node*)malloc(sizeof(struct node));
        *c = (struct node)
        {
            .label = "compound_stmt",
            .isLeaf = 0,
            .children = {[0] = $1, [1] = $2, [2] = $3, [3] = $4},
            .depth = max(($2)->depth, ($3)->depth) + 1, 
            .lp = max(max(($2)->lp, ($3)->lp), ($2)->depth + ($3)->depth + 1)
        };
        $$ = c;
    };

local_decls
    : /*empty*/
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "epsilon",
            .isLeaf = 1, 
            .children = {},
            .depth = 1,
            .lp = 1
        };
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "local_decls",
            .isLeaf = 0, 
            .children = {[0] = a},
            .depth = 2,
            .lp = 2
        };
        $$ = b;
    }
    | local_decls local_decl
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "local_decls",
            .isLeaf = 0,
            .children = {[0] = $1, [1] = $2},
            .depth = max(($1)->depth, ($2)->depth) + 1, 
            .lp = max(max(($1)->lp, ($2)->lp), ($1)->depth + ($2)->depth + 1)
        };
        $$ = a;
    };

local_decl
    : type_spec identifier SEMICOLON
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "SEMICOLON",
            .isLeaf = 1,
            .children = {},
            .depth = 1,
            .lp = 1
        };
        $3 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "local_decl",
            .isLeaf = 0,
            .children = {[0] = $1, [1] = $2, [2] = $3},
            .depth = max(($1)->depth, ($2)->depth) + 1,
            .lp = max(max(($1)->lp, ($2)->lp), ($1)->depth + ($2)->depth + 1)
        };
        $$ = b;
    }
    | type_spec identifier LEFTBRACKET expr RIGHTBRACKET SEMICOLON
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "LEFTBRACKET",
            .isLeaf = 1,
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $3 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "RIGHTBRACKET",
            .isLeaf = 1,
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $5 = b;
        struct node *c = (struct node*)malloc(sizeof(struct node));
        *c = (struct node)
        {
            .label = "SEMICOLON",
            .isLeaf = 1,
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $6 = c;
        struct node *d = (struct node*)malloc(sizeof(struct node));
        *d = (struct node)
        {
            .label = "local_decl",
            .isLeaf = 0,
            .children = {[0] = $1, [1] = $2, [2] = $3, [3] = $4, [4] = $5, [5] = $6},
            .depth = max(($1)->depth, ($4)->depth) + 1, 
            .lp = max(max(($1)->lp, ($4)->lp), ($1)->depth + ($4)->depth + 1)
        };
        $$ = d;
    };

if_stmt
    : IF LEFTBRACE expr RIGHTBRACE stmt %prec IFX
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "IF",
            .isLeaf = 1,
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $1 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "LEFTBRACE",
            .isLeaf = 1,
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $2 = b;
        struct node *c = (struct node*)malloc(sizeof(struct node));
        *c = (struct node)
        {
            .label = "RIGHTBRACE",
            .isLeaf = 1,
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $4 = c;
        struct node *d = (struct node*)malloc(sizeof(struct node));
        *d = (struct node)
        {
            .label = "if_stmt",
            .isLeaf = 0,
            .children = {[0] = $1, [1] = $2, [2] = $3, [3] = $4, [4] = $5},
            .depth = max(($3)->depth, ($5)->depth) + 1, 
            .lp = max(max(($3)->lp, ($5)->lp), ($3)->depth + ($5)->depth + 1)
        };
        $$ = d;
    }
    | IF LEFTBRACE expr RIGHTBRACE stmt ELSE stmt
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "IF",
            .isLeaf = 1,
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $1 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "LEFTBRACE",
            .isLeaf = 1,
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $2 = b;
        struct node *c = (struct node*)malloc(sizeof(struct node));
        *c = (struct node)
        {
            .label = "RIGHTBRACE",
            .isLeaf = 1,
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $4 = c;
        struct node *d = (struct node*)malloc(sizeof(struct node));
        *d = (struct node)
        {
            .label = "ELSE",
            .isLeaf = 1,
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $6 = d;
        struct node *e = (struct node*)malloc(sizeof(struct node));
        *e = (struct node)
        {
            .label = "if_stmt",
            .isLeaf = 0,
            .children = {[0] = $1, [1] = $2, [2] = $3, [3] = $4, [4] = $5, [5] = $6, [6] = $7},
            .depth = max(max(($3)->depth, ($5)->depth), ($7)->depth) + 1,
            .lp = max(max(max(max(max(($3)->lp, ($5)->lp), ($7)->lp), ($3)->depth + ($5)->depth + 1), ($5)->depth + ($7)->depth + 1), ($3)->depth + ($7)->depth + 1)
        };
        $$ = e;
    };

return_stmt
    : RETURN SEMICOLON
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "RETURN",
            .isLeaf = 1,
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $1 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "SEMICOLON",
            .isLeaf = 1,
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $2 = b;
        struct node *c = (struct node*)malloc(sizeof(struct node));
        *c = (struct node)
        {
            .label = "return_stmt",
            .isLeaf = 0,
            .children = {[0] = $1, [1] = $2},
            .depth = 2, 
            .lp = 3
        };
        $$ = c;
    }
    | RETURN expr SEMICOLON
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "RETURN",
            .isLeaf = 1,
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $1 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "SEMICOLON",
            .isLeaf = 1,
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $3 = b;
        struct node *c = (struct node*)malloc(sizeof(struct node));
        *c = (struct node)
        {
            .label = "return_stmt",
            .isLeaf = 0,
            .children = {[0] = $1, [1] = $2, [2] = $3},
            .depth =($2)->depth + 1, 
            .lp = max(($2)->depth + 2, ($2)->lp)
        };
        $$ = c;
    };

break_stmt
    : BREAK SEMICOLON
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "BREAK",
            .isLeaf = 1,
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $1 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "SEMICOLON",
            .isLeaf = 1,
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $2 = b;
        struct node *c = (struct node*)malloc(sizeof(struct node));
        *c = (struct node)
        {
            .label = "break_stmt",
            .isLeaf = 0,
            .children = {[0] = $1, [1] = $2},
            .depth = 2, 
            .lp = 3
        };
        $$ = c;
    };

continue_stmt
    : CONTINUE SEMICOLON
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "CONTINUE",
            .isLeaf = 1,
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $1 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "SEMICOLON",
            .isLeaf = 1,
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $2 = b;
        struct node *c = (struct node*)malloc(sizeof(struct node));
        *c = (struct node)
        {
            .label = "continue_stmt",
            .isLeaf = 0,
            .children = {[0] = $1, [1] = $2},
            .depth = 2, 
            .lp = 3
        };
        $$ = c;
    };

switch_stmt
    : SWITCH LEFTBRACE expr RIGHTBRACE LEFTCURLY compound_case default_case RIGHTCURLY
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "SWITCH",
            .isLeaf = 1,
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $1 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "LEFTBRACE",
            .isLeaf = 1,
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $2 = b;
        struct node *c = (struct node*)malloc(sizeof(struct node));
        *c = (struct node)
        {
            .label = "RIGHTBRACE",
            .isLeaf = 1,
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $4 = c;
        struct node *d = (struct node*)malloc(sizeof(struct node));
        *d = (struct node)
        {
            .label = "LEFTCURLY",
            .isLeaf = 1,
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        $5 = d;
        struct node *e = (struct node*)malloc(sizeof(struct node));
        *e = (struct node)
        {
            .label = "RIGHTCURLY",
            .isLeaf = 1, 
            .children = {}, 
            .depth  = 1, 
            .lp = 1
        };
        $8 = e;
        struct node *f = (struct node*)malloc(sizeof(struct node));
        *f = (struct node)
        {
            .label = "switch_stmt",
            .isLeaf = 0,
            .children = {[0] = $1, [1] = $2, [2] = $3, [3] = $4, [4] = $5, [5] = $6, [6] = $7, [7] = $8},
            .depth = max(max(($3)->depth, ($6)->depth), ($7)->depth) + 1, 
            .lp = max(max(max(max(max(($3)->lp, ($6)->lp), ($7)->lp), ($3)->depth + ($6)->depth + 1), ($6)->depth + ($7)->depth + 1), ($3)->depth + ($7)->depth + 1)
        };
        $$ = f;
    };

compound_case 
    : single_case compound_case
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "compound_case",
            .isLeaf = 0,
            .children = {[0] = $1, [1] = $2},
            .depth = max(($1)->depth, ($2)->depth) + 1,
            .lp = max(max(($1)->lp, ($2)->lp), ($1)->depth + ($2)->depth + 1)
        };
        $$ = a;
    }
    | single_case
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "single_case",
            .isLeaf = 0,
            .children = {[0] = $1},
            .depth = ($1)->depth + 1,
            .lp = max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
    };

single_case
    : CASE integerLit COLON stmt_list
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "CASE",
            .isLeaf = 1,
            .children = {},
            .depth = 1,
            .lp = 1
        };
        $1 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "COLON",
            .isLeaf = 1,
            .children = {},
            .depth = 1,
            .lp = 1
        };
        $3 = b;
        struct node *c = (struct node*)malloc(sizeof(struct node));
        *c = (struct node)
        {
            .label = "single_case",
            .isLeaf = 0,
            .children = {[0] = $1, [1] = $2, [2] = $3, [3] = $4},
            .depth = max(($2)->depth, ($4)->depth) + 1,
            .lp = max(max(($2)->lp, ($4)->lp), ($2)->depth + ($4)->depth + 1)
        };
        $$ = c;
    };

default_case
    : DEFAULT COLON stmt_list
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "DEFAULT",
            .isLeaf = 1,
            .children = {},
            .depth = 1,
            .lp = 1
        };
        $1 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "COLON",
            .isLeaf = 1,
            .children = {},
            .depth = 1,
            .lp = 1
        };
        $2 = b;
        struct node *c = (struct node*)malloc(sizeof(struct node));
        *c = (struct node)
        {
            .label = "default_case",
            .isLeaf = 0,
            .children = {[0] = $1, [1] = $2, [2] = $3},
            .depth = ($3)->depth + 1,
            .lp = max(($3)->depth + 2, ($3)->lp)
        };
        $$ = c;
    };

assign_stmt
    : identifier EQUAL expr SEMICOLON
    {
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "SEMICOLON",
            .isLeaf = 1,
            .children = {},
            .depth = 1,
            .lp = 1
        };
        $4 = b;
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "EQUAL",
            .isLeaf = 0,
            .children = {[0] = $1, [1] = $3},
            .depth = max(($1)->depth, ($3)->depth) + 1,
            .lp = max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1)
        };
        $2 = a;
        struct node *c = (struct node*)malloc(sizeof(struct node));
        *c = (struct node)
        {
            .label = "assign_stmt", 
            .isLeaf = 0, 
            .children = {[0] = $2, [1] = $4},
            .depth = max(($2)->depth, ($4)->depth) + 1,
            .lp = max(($2)->lp, ($2)->depth + 2)
        };
        $$ = c;
    }
    | identifier LEFTBRACKET expr RIGHTBRACKET EQUAL expr SEMICOLON
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "LEFTBRACKET",
            .isLeaf = 1,
            .children = {},
            .depth = 1,
            .lp = 1
        };
        $2 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "RIGHTBRACKET",
            .isLeaf = 1,
            .children = {},
            .depth = 1,
            .lp = 1
        };
        $4 = b;
        struct node *c = (struct node*)malloc(sizeof(struct node));
        *c = (struct node)
        {
            .label = "SEMICOLON",
            .isLeaf = 1,
            .children = {},
            .depth = 1,
            .lp = 1
        };
        $7 = c;
        struct node *e = (struct node*)malloc(sizeof(struct node));
        *e = (struct node)
        {
            .label = "EQUAL", 
            .isLeaf = 0, 
            .children = {[0] = $1, [1] = $2, [2] = $3, [3] = $4, [4] = $6},
            .depth = max(max(($6)->depth, ($3)->depth), ($1)->depth) + 1,
            .lp = max(max(max(max(max(($6)->lp, ($3)->lp), ($1)->lp), ($3)->depth + ($6)->depth + 1), ($1)->depth + ($3)->depth + 1), ($1)->depth + ($6)->depth + 1)
        };
        $5 = e;
        struct node *d = (struct node*)malloc(sizeof(struct node));
        *d = (struct node)
        {
            .label = "assign_stmt",
            .isLeaf = 0,
            .children = {[0] = $5, [1] = $7},
            .depth = max(($5)->depth, ($7)->depth) + 1, 
            .lp = max(max(($5)->lp, ($7)->lp), ($5)->depth + ($7)->depth + 1)
        };
        $$= d;
    }
    | identifier STRUCTPOINT identifier EQUAL expr SEMICOLON
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "STRUCTPOINT",
            .isLeaf = 1, 
            .children = {},
            .depth = 1,
            .lp = 1
        };
        $2 = a;
        struct node *c = (struct node*)malloc(sizeof(struct node));
        *c = (struct node)
        {
            .label = "SEMICOLON", 
            .isLeaf = 1, 
            .children = {}, 
            .depth = 1, 
            .lp = 1
        };
        $6 = c;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "EQUAL",
            .isLeaf = 0,
            .children = {[0] = $1, [1] = $2, [2] = $3, [3] = $5},
            .depth = max(($1)->depth, ($5)->depth) + 1,
            .lp = max(max(($1)->lp, ($5)->lp), ($1)->depth + ($5)->depth + 1)
        };
        $4 = b;
        struct node *d = (struct node*)malloc(sizeof(struct node));
        *d = (struct node)
        {
            .label = "assign_stmt",
            .isLeaf = 0, 
            .children = {[0] = $4, [1] = $6}, 
            .depth = max(($4)->depth, ($6)->depth) + 1, 
            .lp = max(max(($4)->lp, ($6)->lp), ($4)->depth + ($6)->depth + 1)
        };
        $$ = d;
    }
    | identifier STRUCTNORM identifier EQUAL expr SEMICOLON
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "STRUCTNORM",
            .isLeaf = 1, 
            .children = {},
            .depth = 1,
            .lp = 1
        };
        $2 = a;
        struct node *c = (struct node*)malloc(sizeof(struct node));
        *c = (struct node)
        {
            .label = "SEMICOLON", 
            .isLeaf = 1, 
            .children = {}, 
            .depth = 1, 
            .lp = 1
        };
        $6 = c;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "EQUAL",
            .isLeaf = 0,
            .children = {[0] = $1, [1] = $2, [2] = $3, [3] = $5},
            .depth = max(($1)->depth, ($5)->depth) + 1,
            .lp = max(max(($1)->lp, ($5)->lp), ($1)->depth + ($5)->depth + 1)
        };
        $4 = b;
        struct node *d = (struct node*)malloc(sizeof(struct node));
        *d = (struct node)
        {
            .label = "assign_stmt",
            .isLeaf = 0, 
            .children = {[0] = $4, [1] = $6}, 
            .depth = max(($4)->depth, ($6)->depth) + 1, 
            .lp = max(max(($4)->lp, ($6)->lp), ($4)->depth + ($6)->depth + 1)
        };
        $$ = d;
    };

incr_stmt
    : identifier INCR SEMICOLON
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "SEMICOLON", 
            .isLeaf = 1, 
            .children = {}, 
            .depth = 1, 
            .lp = 1
        };
        $3 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "INCR", 
            .isLeaf = 1, 
            .children = {}, 
            .depth = 1,
            .lp = 1
        };
        $2 = b;
        struct node *c = (struct node*)malloc(sizeof(struct node));
        *c = (struct node)
        {
            .label = "incr_stmt", 
            .isLeaf = 0, 
            .children = {[0] = $1, [1] = $2, [2] = $3}, 
            .depth = max(($1)->depth, ($2)->depth) + 1,
            .lp = max(max(($1)->depth + 1 + ($2)->depth, ($2)->lp), ($1)->lp)
        };
        $$ = c;
    };

decr_stmt
    : identifier DECR SEMICOLON
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "SEMICOLON", 
            .isLeaf = 1, 
            .children = {}, 
            .depth = 1, 
            .lp = 1
        };
        $3 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "DECR", 
            .isLeaf = 0, 
            .children = {}, 
            .depth = 1,
            .lp = 1 
        };
        $2 = b;
        struct node *c = (struct node*)malloc(sizeof(struct node));
        *c = (struct node)
        {
            .label = "decr_stmt", 
            .isLeaf = 0, 
            .children = {[0] = $1, [1] = $2, [2] = $3}, 
            .depth = max(($1)->depth, ($2)->depth) + 1,
            .lp = max(max(($1)->depth + 1 + ($2)->depth, ($1)->lp), ($2)->lp)
        };
        $$ = c;
    };

expr
    : Pexpr LT Pexpr
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "LT", 
            .isLeaf = 0, 
            .children = {[0] = $1, [1] = $3}, 
            .depth = max(($1)->depth, ($3)->depth) + 1,
            .lp = max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1)
        };
        $2 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "expr", 
            .isLeaf = 0, 
            .children = {[0] = $2}, 
            .depth = ($2)->depth + 1, 
            .lp = max(($2)->depth + 1, ($2)->lp)
        };
        $$ = b;
    }
    | Pexpr GT Pexpr
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "GT", 
            .isLeaf = 0, 
            .children = {[0] = $1, [1] = $3}, 
            .depth = max(($1)->depth, ($3)->depth) + 1,
            .lp = max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1)
        };
        $2 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "expr", 
            .isLeaf = 0, 
            .children = {[0] = $2}, 
            .depth = ($2)->depth + 1, 
            .lp = max(($2)->depth + 1, ($2)->lp)
        };
        $$ = b;
    }
    | Pexpr LEQ Pexpr
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "LEQ", 
            .isLeaf = 0, 
            .children = {[0] = $1, [1] = $3}, 
            .depth = max(($1)->depth, ($3)->depth) + 1,
            .lp = max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1)
        };
        $2 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "expr", 
            .isLeaf = 0, 
            .children = {[0] = $2}, 
            .depth = ($2)->depth + 1, 
            .lp = max(($2)->depth + 1, ($2)->lp)
        };
        $$ = b;
    }
    | Pexpr GEQ Pexpr
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "GEQ", 
            .isLeaf = 0, 
            .children = {[0] = $1, [1] = $3}, 
            .depth = max(($1)->depth, ($3)->depth) + 1,
            .lp = max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1)
        };
        $2 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "expr", 
            .isLeaf = 0, 
            .children = {[0] = $2}, 
            .depth = ($2)->depth + 1, 
            .lp = max(($2)->depth + 1, ($2)->lp)
        };
        $$ = b;
    }
    | Pexpr LOGOR Pexpr
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "LOGOR", 
            .isLeaf = 0, 
            .children = {[0] = $1, [1] = $3}, 
            .depth = max(($1)->depth, ($3)->depth) + 1,
            .lp = max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1)
        };
        $2 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "expr", 
            .isLeaf = 0, 
            .children = {[0] = $2}, 
            .depth = ($2)->depth + 1, 
            .lp = max(($2)->depth + 1, ($2)->lp)
        };
        $$ = b;
    }
    | Pexpr LOGAND Pexpr
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "LOGAND", 
            .isLeaf = 0, 
            .children = {[0] = $1, [1] = $3}, 
            .depth = max(($1)->depth, ($3)->depth) + 1,
            .lp = max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1)
        };
        $2 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "expr", 
            .isLeaf = 0, 
            .children = {[0] = $2}, 
            .depth = ($2)->depth + 1, 
            .lp = max(($2)->depth + 1, ($2)->lp)
        };
        $$ = b;
    }
    | SIZEOF LEFTBRACE Pexpr RIGHTBRACE
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "SIZEOF", 
            .isLeaf = 1, 
            .children = {}, 
            .depth = 1,
            .lp = 1
        };
        $1 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "LEFTBRACE", 
            .isLeaf = 1, 
            .children = {}, 
            .depth = 1,
            .lp = 1
        };
        $2 = b;
        struct node *c = (struct node*)malloc(sizeof(struct node));
        *c = (struct node)
        {
            .label = "RIGHTBRACE", 
            .isLeaf = 1, 
            .children = {}, 
            .depth = 1,
            .lp = 1
        };
        $4 = c;
        struct node *d = (struct node*)malloc(sizeof(struct node));
        *d = (struct node)
        {
            .label = "expr", 
            .isLeaf = 0, 
            .children = {[0] = $1, [1] = $2, [2] = $3, [3] = $4},
            .depth = ($3)->depth + 1,
            .lp = max(($3)->depth + 2, ($3)->lp)
        };
        $$ = d;
    }
    | Pexpr STRUCTPOINT Pexpr
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "STRUCTPOINT", 
            .isLeaf = 0, 
            .children = {[0] = $1, [1] = $3}, 
            .depth = max(($1)->depth, ($3)->depth) + 1,
            .lp = max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1)
        };
        $2 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "expr", 
            .isLeaf = 0, 
            .children = {[0] = $2}, 
            .depth = ($2)->depth + 1, 
            .lp = max(($2)->depth + 1, ($2)->lp)
        };
        $$ = b;
    }
    | Pexpr ISEQ Pexpr
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "ISEQ", 
            .isLeaf = 0, 
            .children = {[0] = $1, [1] = $3}, 
            .depth = max(($1)->depth, ($3)->depth) + 1,
            .lp = max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1)
        };
        $2 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "expr", 
            .isLeaf = 0, 
            .children = {[0] = $2}, 
            .depth = ($2)->depth + 1, 
            .lp = max(($2)->depth + 1, ($2)->lp)
        };
        $$ = b;
    }
    | Pexpr NEQ Pexpr
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "NEQ", 
            .isLeaf = 0, 
            .children = {[0] = $1, [1] = $3}, 
            .depth = max(($1)->depth, ($3)->depth) + 1,
            .lp = max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1)
        };
        $2 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "expr", 
            .isLeaf = 0, 
            .children = {[0] = $2}, 
            .depth = ($2)->depth + 1, 
            .lp = max(($2)->depth + 1, ($2)->lp)
        };
        $$ = b;
    }
    | Pexpr THREEWAY Pexpr
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "THREEWAY", 
            .isLeaf = 0, 
            .children = {[0] = $1, [1] = $3}, 
            .depth = max(($1)->depth, ($3)->depth) + 1,
            .lp = max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1)
        };
        $2 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "expr", 
            .isLeaf = 0, 
            .children = {[0] = $2}, 
            .depth = ($2)->depth + 1, 
            .lp = max(($2)->depth + 1, ($2)->lp)
        };
        $$ = b;
    }
    | Pexpr PLUS Pexpr
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "PLUS", 
            .isLeaf = 0, 
            .children = {[0] = $1, [1] = $3}, 
            .depth = max(($1)->depth, ($3)->depth) + 1,
            .lp = max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1)
        };
        $2 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "expr", 
            .isLeaf = 0, 
            .children = {[0] = $2}, 
            .depth = ($2)->depth + 1, 
            .lp = max(($2)->depth + 1, ($2)->lp)
        };
        $$ = b;
    }
    | Pexpr MINUS Pexpr
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "MINUS", 
            .isLeaf = 0, 
            .children = {[0] = $1, [1] = $3}, 
            .depth = max(($1)->depth, ($3)->depth) + 1,
            .lp = max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1)
        };
        $2 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "expr", 
            .isLeaf = 0, 
            .children = {[0] = $2}, 
            .depth = ($2)->depth + 1, 
            .lp = max(($2)->depth + 1, ($2)->lp)
        };
        $$ = b;
    }
    | Pexpr STAR Pexpr
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "STAR", 
            .isLeaf = 0, 
            .children = {[0] = $1, [1] = $3}, 
            .depth = max(($1)->depth, ($3)->depth) + 1,
            .lp = max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1)
        };
        $2 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "expr", 
            .isLeaf = 0, 
            .children = {[0] = $2}, 
            .depth = ($2)->depth + 1, 
            .lp = max(($2)->depth + 1, ($2)->lp)
        };
        $$ = b;
    }
    | Pexpr DIV Pexpr
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "DIV", 
            .isLeaf = 0, 
            .children = {[0] = $1, [1] = $3}, 
            .depth = max(($1)->depth, ($3)->depth) + 1,
            .lp = max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1)
        };
        $2 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "expr", 
            .isLeaf = 0, 
            .children = {[0] = $2}, 
            .depth = ($2)->depth + 1, 
            .lp = max(($2)->depth + 1, ($2)->lp)
        };
        $$ = b;
    }
    | Pexpr MOD Pexpr
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "MOD", 
            .isLeaf = 0, 
            .children = {[0] = $1, [1] = $3}, 
            .depth = max(($1)->depth, ($3)->depth) + 1,
            .lp = max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1)
        };
        $2 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "expr", 
            .isLeaf = 0, 
            .children = {[0] = $2}, 
            .depth = ($2)->depth + 1, 
            .lp = max(($2)->depth + 1, ($2)->lp)
        };
        $$ = b;
    }
    | NOT Pexpr
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "NOT", 
            .isLeaf = 1, 
            .children = {}, 
            .depth = 1, 
            .lp = 1
        };
        $1 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "expr", 
            .isLeaf = 0, 
            .children = {[0] = $1, [1] = $2}, 
            .depth = ($2)->depth + 1,
            .lp = max(($2)->depth + 2, ($2)->lp)
        };
        $$ = b;
    }
    | MINUS Pexpr
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "MINUS", 
            .isLeaf = 1, 
            .children = {}, 
            .depth = 1, 
            .lp = 1
        };
        $1 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "expr", 
            .isLeaf = 0, 
            .children = {[0] = $1, [1] = $2}, 
            .depth = ($2)->depth + 1,
            .lp = max(($2)->depth + 2, ($2)->lp)
        };
        $$ = b;
    }
    | PLUS Pexpr
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "PLUS", 
            .isLeaf = 1, 
            .children = {}, 
            .depth = 1, 
            .lp = 1
        };
        $1 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "expr", 
            .isLeaf = 0, 
            .children = {[0] = $1, [1] = $2}, 
            .depth = ($2)->depth + 1,
            .lp = max(($2)->depth + 2, ($2)->lp)
        };
        $$ = b;
    }
    | STAR Pexpr
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "STAR", 
            .isLeaf = 1, 
            .children = {}, 
            .depth = 1, 
            .lp = 1
        };
        $1 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "expr", 
            .isLeaf = 0, 
            .children = {[0] = $1, [1] = $2}, 
            .depth = ($2)->depth + 1,
            .lp = max(($2)->depth + 2, ($2)->lp)
        };
        $$ = b;
    }
    | AND Pexpr
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "AND", 
            .isLeaf = 1, 
            .children = {}, 
            .depth = 1, 
            .lp = 1
        };
        $1 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "expr", 
            .isLeaf = 0, 
            .children = {[0] = $1, [1] = $2}, 
            .depth = ($2)->depth + 1,
            .lp = max(($2)->depth + 2, ($2)->lp)
        };
        $$ = b;
    }
    | Pexpr
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "expr", 
            .isLeaf = 0, 
            .children = {[0] = $1}, 
            .depth = ($1)->depth + 1, 
            .lp = max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
    }
    | identifier LEFTBRACE args RIGHTBRACE
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "LEFTBRACE", 
            .isLeaf = 1, 
            .children = {}, 
            .depth = 1, 
            .lp = 1
        };
        $2 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "RIGHTBRACE", 
            .isLeaf = 1, 
            .children = {}, 
            .depth = 1, 
            .lp = 1
        };
        $4 = b;
        struct node *c = (struct node*)malloc(sizeof(struct node));
        *c = (struct node)
        {
            .label = "expr", 
            .isLeaf = 0, 
            .children = {[0] = $1, [1] = $2, [2] = $3, [3] = $4}, 
            .depth = max(($1)->depth, ($3)->depth) + 1,
            .lp = max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1)
        };
        $$ = c;
    }
    | identifier LEFTBRACKET expr RIGHTBRACKET
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "LEFTBRACKET", 
            .isLeaf = 1, 
            .children = {}, 
            .depth = 1, 
            .lp = 1
        };
        $2 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "RIGHTBRACKET", 
            .isLeaf = 1, 
            .children = {}, 
            .depth = 1, 
            .lp = 1
        };
        $4 = b;
        struct node *c = (struct node*)malloc(sizeof(struct node));
        *c = (struct node)
        {
            .label = "expr", 
            .isLeaf = 0, 
            .children = {[0] = $1, [1] = $2, [2] = $3, [3] = $4}, 
            .depth = max(($1)->depth, ($3)->depth) + 1,
            .lp = max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1)
        };
        $$ = c;
    };

Pexpr
    : integerLit 
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "Pexpr", 
            .isLeaf = 0, 
            .children = {[0] = $1}, 
            .depth = ($1)->depth + 1, 
            .lp = max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
    }
    | floatLit
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "Pexpr", 
            .isLeaf = 0, 
            .children = {[0] = $1}, 
            .depth = ($1)->depth + 1, 
            .lp = max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
    }
    | identifier 
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "Pexpr", 
            .isLeaf = 0, 
            .children = {[0] = $1}, 
            .depth = ($1)->depth + 1, 
            .lp = max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
    }
    | LEFTBRACE expr RIGHTBRACE
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "LEFTBRACE", 
            .isLeaf = 1, 
            .children = {}, 
            .depth = 1, 
            .lp = 1
        };
        $1 = a;
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "RIGHTBRACE", 
            .isLeaf = 1, 
            .children = {}, 
            .depth = 1, 
            .lp = 1
        };
        $3 = b;
        struct node *c = (struct node*)malloc(sizeof(struct node));
        *c = (struct node)
        {
            .label = "Pexpr", 
            .isLeaf = 0, 
            .children = {[0] = $1, [1] = $2, [2] = $3}, 
            .depth = ($2)->depth + 1, 
            .lp = max(($2)->depth + 2, ($2)->lp)
        };
        $$ = c;
    };

arg_list
    : arg_list COMMA expr
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "COMMA", 
            .isLeaf = 1, 
            .children = {}, 
            .depth = 1, 
            .lp = 1
        };
        $2 = a;
        struct node *c = (struct node*)malloc(sizeof(struct node));
        *c = (struct node)
        {
            .label = "arg_list", 
            .isLeaf = 0, 
            .children = {[0] = $1, [1] = $2, [2] = $3}, 
            .depth = max(($1)->depth, ($3)->depth) + 1, 
            .lp = max(max(($1)->depth + 1 + ($3)->depth, ($1)->lp), ($3)->lp)
        };
        $$ = c;
    }
    | expr
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "arg_list", 
            .isLeaf = 0, 
            .children = {[0] = $1}, 
            .depth = ($1)->depth + 1, 
            .lp = max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
    };

args 
    : /*empty*/
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "epsilon",
            .isLeaf = 1,
            .children = {},
            .depth = 1, 
            .lp = 1
        };
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .label = "args",
            .isLeaf = 0,
            .children = {[0] = a},
            .depth = 2,
            .lp = 2
        };
        $$ = b;
    }
    | arg_list
    {
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "args", 
            .isLeaf = 0, 
            .children = {[0] = $1}, 
            .depth = ($1)->depth + 1, 
            .lp = max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
    };

integerLit
    : INTCONSTANT
    {
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .isLeaf = 1, 
            .children = {}, 
            .depth = 1, 
            .lp = 1
        };
        $1 = b;
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "integerLit",
            .isLeaf = 0, 
            .children = {[0] = $1}, 
            .depth = 2, 
            .lp = 2
        };
        strcpy(b->label, yytext);
        $$ = a;
    };

floatLit
    : FLOATCONSTANT
    {
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .isLeaf = 1, 
            .children = {}, 
            .depth = 1, 
            .lp = 1
        };
        $1 = b;
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "floatLit",
            .isLeaf = 0, 
            .children = {[0] = $1}, 
            .depth = 2, 
            .lp = 2
        };
        strcpy(b->label, yytext);
        $$ = a;
    };

identifier
    : ID 
    {
        struct node *b = (struct node*)malloc(sizeof(struct node));
        *b = (struct node)
        {
            .isLeaf = 1, 
            .children = {}, 
            .depth = 1, 
            .lp = 1
        };
        $1 = b;
        struct node *a = (struct node*)malloc(sizeof(struct node));
        *a = (struct node)
        {
            .label = "identifier",
            .isLeaf = 0, 
            .children = {[0] = $1}, 
            .depth = 2, 
            .lp = 2
        };
        strcpy(b->label, yytext);
        $$ = a;
    };
%%
void yyerror(char *s) 
{
    printf("syntax error\n");
    exit(0);
}

int main(int argc, char* argv[]) 
{
    int a = yyparse();
    if(a == 0)
    {
        printf("%lli\n", longestPath);
        printf("%lli\n", longestPathIf);
        printf("%lli\n", longestPathWhile);
        printf("%lli\n", longestPathSwitch);
        printf("%lli\n", longestPathMain);
    }
    else
    {
        printf("syntax error\n");
        exit(0);
    }
    return 0;
}