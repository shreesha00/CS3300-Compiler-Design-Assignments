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
	#include <bits/stdc++.h>
    using namespace std;
    typedef long long int ll;
    typedef struct record
    {
        string identifier;
        ll stackOffset;
        int type;
    } record;
    #define MAX 10
    void yyerror(char*);
    int yylex(void);
	char mytext[10000];
	extern char *yytext;
    extern int commentError;
    struct node
    {
        string label; //label name
        bool isLeaf; //isLeaf = 1 -> Leaf node. Else isLeaf = 0;
        struct node *children[MAX]; //at max 10 children for each node
        ll depth; //depth represents the height of it's subtree.
        ll lp; //longest path in subtree
    };
    //int yydebug = 1;
    inline ll max(ll a, ll b)
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
    void printtree(struct node* n, ll level)
    {
        for(int i = 0; i < level ; i++)
        {
            cout<<"_";
        }
        cout<<n->label<<endl;
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
    struct node* ast = NULL;
    ll longestPath = 0;
    ll longestPathIf = 0;
    ll longestPathWhile = 0;
    ll longestPathSwitch = 0;
    ll longestPathMain = 0;
    bool isMain = 0;
    vector<string> asmCode;
    unordered_map<string, record*> symbolTable;   
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
        struct node *a = new node;
        *a = (struct node)
        {
            "program",
            0, 
            {[0] = $1},
            ($1)->depth + 1,
            max(($1)->lp, ($1)->depth + 1)
        };
        $$ = a;
        //printtree($$, 0);
        longestPath = $$->lp;
        ast = $$;
    };

decl_list
    : decl 
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "decl_list",
            0, 
            {[0] = $1},
            ($1)->depth + 1,
            max(($1)->lp, ($1)->depth + 1)
        };
        $$ = a;
    }
    | decl_list decl
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "decl_list",
            0, 
            {[0] = $1, [1] = $2},
            max(($1)->depth, ($2)->depth) + 1,
            max(max(($1)->lp, ($2)->lp), ($1)->depth + ($2)->depth + 1)
        };
        $$ = a;
    };

decl
    : var_decl
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "decl",
            0, 
            {[0] = $1},
            ($1)->depth + 1,
            max(($1)->lp, ($1)->depth + 1)
        };
        $$ = a;
    }
    | func_decl
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "decl",
            0, 
            {[0] = $1},
            ($1)->depth + 1,
            max(($1)->lp, ($1)->depth + 1)
        };
        $$ = a;
    }
    | struct_decl
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "decl",
            0, 
            {[0] = $1},
            ($1)->depth + 1,
            max(($1)->lp, ($1)->depth + 1)
        };
        $$ = a;
    };

struct_decl
    : STRUCT identifier LEFTCURLY local_decls RIGHTCURLY SEMICOLON
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "STRUCT",
            1, 
            {},
            1,
            1
        };
        $1 = a; 
        struct node *b = new node;
        *b = (struct node)
        {
            "LEFTCURLY",
            1, 
            {},
            1,
            1
        };
        $3 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "RIGHTCURLY",
            1, 
            {},
            1,
            1
        };
        $5 = c;
        struct node *d = new node;
        *d = (struct node)
        {
            "SEMICOLON",
            1, 
            {},
            1,
            1
        };
        $6 = d;
        struct node *e = new node;
        *e = (struct node)
        {
            "struct_decl",
            0, 
            {[0] = $1, [1] = $2, [2] = $3, [3] = $4, [4] = $5, [5] = $6},
            max(($2)->depth, ($4)->depth) + 1,
            max(max(($2)->lp, ($4)->lp), ($2)->depth + ($4)->depth + 1)
        };
        $$ = e;
    };

var_decl
    : type_spec identifier SEMICOLON
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "SEMICOLON",
            1, 
            {},
            1,
            1
        };
        $3 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "var_decl",
            0, 
            {[0] = $1, [1] = $2, [2] = $3},
            max(($1)->depth, ($2)->depth) + 1,
            max(max(($1)->lp, ($2)->lp), ($1)->depth + ($2)->depth + 1)
        };
        $$ = b;
    }
    | type_spec identifier COMMA var_decl
    {
        struct node *b = new node;
        *b = (struct node)
        {
            "COMMA",
            1, 
            {},
            1,
            1
        };
        $3 = b;
        struct node *a = new node;
        *a = (struct node)
        {
            "var_decl",
            0, 
            {[0] = $1, [1] = $2, [2] = $3, [3] = $4},
            max(($1)->depth, ($4)->depth) + 1,
            max(max(($1)->lp, ($4)->lp), ($1)->depth + ($4)->depth + 1)
        };
        $$ = a;
    }
    | type_spec identifier LEFTBRACKET integerLit RIGHTBRACKET SEMICOLON
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "LEFTBRACKET",
            1,
            {},
            1, 
            1
        };
        $3 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "RIGHTBRACKET",
            1,
            {},
            1, 
            1
        };
        $5 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "SEMICOLON",
            1,
            {},
            1, 
            1
        };
        $6 = c;
        struct node *d = new node;
        *d = (struct node)
        {
            "var_decl",
            0,
            {[0] = $1, [1] = $2, [2] = $3, [3] = $4, [4] = $5, [5] = $6},
            max(($1)->depth, ($4)->depth) + 1,
            max(max(($1)->lp, ($4)->lp), ($1)->depth + ($4)->depth + 1)
        };
        $$ = d;
    }
    | type_spec identifier LEFTBRACKET integerLit RIGHTBRACKET COMMA var_decl
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "LEFTBRACKET",
            1,
            {},
            1,
            1
        };
        $3 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "RIGHTBRACKET",
            1,
            {},
            1,
            1
        };
        $5 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "COMMA",
            1,
            {},
            1,
            1
        };
        $6 = c;
        struct node *d = new node;
        *d = (struct node)
        {
            "var_decl",
            0,
            {[0] = $1, [1] = $2, [2] = $3, [3] = $4, [4] = $5, [5] = $6, [6] = $7},
            max(($1)->depth, ($7)->depth) + 1,
            max(max(($1)->lp, ($7)->lp), ($1)->depth + ($7)->depth + 1)
        };
        $$ = d;
    };

type_spec
    : extern_spec VOID 
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "VOID",
            1,
            {},
            1,
            1
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "type_spec",
            0,
            {[0] = $1, [1] = $2},
            3,
            4
        };
        $$ = b;
    }
    | extern_spec INT
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "INT",
            1,
            {},
            1,
            1
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "type_spec",
            0,
            {[0] = $1, [1] = $2},
            3,
            4
        };
        $$ = b;
    }
    | extern_spec FLOAT
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "FLOAT",
            1,
            {},
            1,
            1
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "type_spec",
            0,
            {[0] = $1, [1] = $2},
            3,
            4
        };
        $$ = b;
    }
    | extern_spec VOID STAR
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "VOID",
            1,
            {},
            1,
            1
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "STAR",
            1,
            {},
            1,
            1
        };
        $3 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "type_spec",
            0,
            {[0] = $1, [1] = $2, [2] = $3},
            3,
            4
        };
        $$ = c;
    }
    | extern_spec INT STAR
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "INT",
            1,
            {},
            1,
            1
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "STAR",
            1,
            {},
            1,
            1
        };
        $3 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "type_spec",
            0,
            {[0] = $1, [1] = $2, [2] = $3},
            3,
            4
        };
        $$ = c;
    }
    | extern_spec FLOAT STAR
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "FLOAT",
            1,
            {},
            1, 
            1
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "STAR",
            1,
            {},
            1,
            1
        };
        $3 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "type_spec",
            0,
            {[0] = $1, [1] = $2, [2] = $3},
            3,
            4
        };
        $$ = c;
    }
    | STRUCT identifier 
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "STRUCT",
            1,
            {},
            1,
            1
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "type_spec",
            0,
            {[0] = $1, [1] = $2},
            3,
            4
        };
        $$ = b;
    }
    | STRUCT identifier STAR
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "STRUCT",
            1,
            {},
            1,
            1
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "STAR",
            1,
            {},
            1,
            1
        };
        $3 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "type_spec",
            0,
            {[0] = $1, [1] = $2, [2] = $3},
            3,
            4
        };
        $$ = c;
    };

extern_spec
    : /*empty*/
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "epsilon",
            1,
            {},
            1, 
            1
        };
        struct node *b = new node;
        *b = (struct node)
        {
            "extern_spec",
            0,
            {[0] = a},
            2,
            2
        };
        $$ = b;
    }
    | EXTERN
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "EXTERN",
            1,
            {},
            1, 
            1
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "extern_spec",
            0,
            {[0] = $1},
            2,
            2
        };
        $$ = b;
    };

func_decl
    : type_spec identifier { if(strcmp(mytext, "main") == 0) isMain = 1; } LEFTBRACE params RIGHTBRACE compound_stmt
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "LEFTBRACE",
            1, 
            {},
            1, 
            1
        };
        $4 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "RIGHTBRACE",
            1, 
            {},
            1,
            1
        };
        $6 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "func_decl",
            0, 
            {[0] = $1, [1] = $2, [2] = $4, [3] = $5, [4] = $6, [5] = $7},
            max(max(($1)->depth, ($5)->depth), ($7)->depth) + 1,
            max(max(max(max(max(($1)->lp, ($5)->lp), ($7)->lp), ($1)->depth + ($5)->depth + 1), ($5)->depth + ($7)->depth + 1), ($1)->depth + ($7)->depth + 1)
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
        struct node *a = new node;
        *a = (struct node)
        {
            "epsilon",
            1,
            {},
            1,
            1
        };
        struct node *b = new node;
        *b = (struct node)
        {
            "params",
            0,
            {[0] = a},
            2,
            2
        };
        $$ = b;
    }
    | param_list
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "params",
            0,
            {[0] = $1},
            ($1)->depth + 1, 
            max(($1)->lp, ($1)->depth + 1)
        };
        $$ = a;
    };

param_list
    : param_list COMMA param
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "COMMA",
            1,
            {},
            1, 
            1
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "param_list",
            0,
            {[0] = $1, [1] = $2, [2] = $3},
            max(($1)->depth, ($3)->depth) + 1, 
            max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1)
        };
        $$ = b;
    }
    | param
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "param_list",
            0,
            {[0] = $1}, 
            ($1)->depth + 1, 
            max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
    };

param
    : type_spec identifier 
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "param",
            0,
            {[0] = $1, [1] = $2}, 
            max(($1)->depth, ($2)->depth) + 1, 
            max(max(($1)->lp, ($2)->lp), ($1)->depth + ($2)->depth + 1)
        };
        $$ = a;
    }
    | type_spec identifier LEFTBRACKET RIGHTBRACKET
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "LEFTBRACKET",
            1,
            {},
            1, 
            1
        };
        $3 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "RIGHTBRACKET",
            1,
            {},
            1, 
            1
        };
        $4 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "param",
            0,
            {[0] = $1, [1] = $2, [2] = $3, [3] = $4}, 
            max(($1)->depth, ($2)->depth) + 1, 
            max(max(($1)->lp, ($2)->lp), ($1)->depth + ($2)->depth + 1)
        };
        $$ = c;
    };

stmt_list
    : stmt_list stmt
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "stmt_list", 
            0, 
            {[0] = $1, [1] = $2},
            max(($1)->depth, ($2)->depth) + 1, 
            max(max(($1)->lp, ($2)->lp), ($1)->depth + ($2)->depth + 1)
        };
        $$ = a;
    }
    | stmt
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "stmt_list", 
            0, 
            {[0] = $1},
            ($1)->depth + 1, 
            max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
    };

stmt
    : assign_stmt
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "stmt", 
            0, 
            {[0] = $1},
            ($1)->depth + 1, 
            max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
    }
    | compound_stmt
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "stmt", 
            0, 
            {[0] = $1},
            ($1)->depth + 1, 
            max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
    }
    | if_stmt
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "stmt", 
            0, 
            {[0] = $1},
            ($1)->depth + 1, 
            max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
        if(($$)->lp > longestPathIf)
        {
            longestPathIf = ($$)->lp;
        }
    }
    | while_stmt
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "stmt", 
            0, 
            {[0] = $1},
            ($1)->depth + 1, 
            max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
        if(($$)->lp > longestPathWhile)
        {
            longestPathWhile = ($$)->lp;
        }
    }
    | switch_stmt
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "stmt", 
            0, 
            {[0] = $1},
            ($1)->depth + 1, 
            max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
        if(($$)->lp > longestPathSwitch)
        {
            longestPathSwitch = ($$)->lp;
        }
    }
    | return_stmt
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "stmt", 
            0, 
            {[0] = $1},
            ($1)->depth + 1, 
            max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
    }
    | break_stmt
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "stmt", 
            0, 
            {[0] = $1},
            ($1)->depth + 1, 
            max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
    }
    | continue_stmt
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "stmt", 
            0, 
            {[0] = $1},
            ($1)->depth + 1, 
            max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
    }
    | dowhile_stmt
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "stmt", 
            0, 
            {[0] = $1},
            ($1)->depth + 1, 
            max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
    }
    | print_stmt
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "stmt", 
            0, 
            {[0] = $1},
            ($1)->depth + 1, 
            max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
    }
    | incr_stmt 
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "stmt", 
            0, 
            {[0] = $1},
            ($1)->depth + 1, 
            max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
    }
    | decr_stmt
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "stmt", 
            0, 
            {[0] = $1},
            ($1)->depth + 1, 
            max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
    };
    /*| expr_stmt
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "stmt", 
            0, 
            {[0] = $1},
            ($1)->depth + 1, 
            max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
    };

/*
expr_stmt
    : expr SEMICOLON
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "SEMICOLON",
            1, 
            {}, 
            1, 
            1
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr_stmt",
            0, 
            {[0] = $1, [1] = $2},
            max(($1)->depth, ($2)->depth) + 1,
            max(($1)->depth + 2, ($1)->lp)
        };
        $$ = b;
    };
*/
while_stmt
    : WHILE LEFTBRACE expr RIGHTBRACE stmt
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "WHILE",
            1, 
            {},
            1, 
            1
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "LEFTBRACE",
            1, 
            {},
            1, 
            1
        };
        $2 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "RIGHTBRACE",
            1, 
            {},
            1, 
            1
        };
        $4 = c;
        struct node *d = new node;
        *d = (struct node)
        {
            "while_stmt",
            0,
            {[0] = $1, [1] = $2, [2] = $3, [3] = $4, [4] = $5},
            max(($3)->depth, ($5)->depth) + 1,
            max(max(($3)->lp, ($5)->lp), ($3)->depth + ($5)->depth + 1)
        };
        $$ = d;
    };

dowhile_stmt
    : DO stmt WHILE LEFTBRACE expr RIGHTBRACE SEMICOLON
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "DO",
            1,
            {},
            1, 
            1
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "WHILE",
            1,
            {},
            1, 
            1
        };
        $3 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "LEFTBRACE",
            1,
            {},
            1, 
            1
        };
        $4 = c;
        struct node *d = new node;
        *d = (struct node)
        {
            "RIGHTBRACE",
            1,
            {},
            1, 
            1
        };
        $6 = d;
        struct node *e = new node;
        *e = (struct node)
        {
            "SEMICOLON",
            1,
            {},
            1, 
            1
        };
        $7 = e;
        struct node *f = new node;
        *f = (struct node)
        {
            "dowhile_stmt",
            0,
            {[0] = $1, [1] = $2, [2] = $3, [3] = $4, [4] = $5, [5] = $6, [6] = $7},
            max(($2)->depth, ($5)->depth) + 1, 
            max(max(($2)->lp, ($5)->lp), ($2)->depth + ($5)->depth + 1)
        };
        $$ = f;
    };

print_stmt
    : PRINTF LEFTBRACE format_specifier COMMA identifier RIGHTBRACE SEMICOLON
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "PRINTF",
            1,
            {},
            1,
            1
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "LEFTBRACE",
            1,
            {},
            1,
            1
        };
        $2 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "COMMA",
            1,
            {},
            1,
            1
        };
        $4 = c;
        struct node *d = new node;
        *d = (struct node)
        {
            "RIGHTBRACE",
            1,
            {},
            1,
            1
        };
        $6 = d;
        struct node *e = new node;
        *e = (struct node)
        {
            "SEMICOLON",
            1,
            {},
            1,
            1
        };
        $7 = e;
        struct node *f = new node;
        *f = (struct node)
        {
            "print_stmt",
            0,
            {[0] = $1, [1] = $2, [2] = $3, [3] = $4, [4] = $5, [5] = $6, [6] = $7},
            max(($3)->depth, ($5)->depth) + 1,
            max(max(($3)->lp, ($5)->lp), ($3)->depth + ($5)->depth + 1)
        };
        $$ = f;
    };

format_specifier
    : FRMT_SPEC
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "FRMT_SPEC",
            1,
            {},
            1, 
            1
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "format_specifier",
            0,
            {[0] = $1},
            2, 
            2
        };
        $$ = b;
    };

compound_stmt  
    : LEFTCURLY local_decls stmt_list RIGHTCURLY
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "LEFTCURLY",
            1,
            {},
            1, 
            1
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "RIGHTCURLY",
            1,
            {},
            1,
            1
        };
        $4 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "compound_stmt",
            0,
            {[0] = $1, [1] = $2, [2] = $3, [3] = $4},
            max(($2)->depth, ($3)->depth) + 1, 
            max(max(($2)->lp, ($3)->lp), ($2)->depth + ($3)->depth + 1)
        };
        $$ = c;
    };

local_decls
    : /*empty*/
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "epsilon",
            1, 
            {},
            1,
            1
        };
        struct node *b = new node;
        *b = (struct node)
        {
            "local_decls",
            0, 
            {[0] = a},
            2,
            2
        };
        $$ = b;
    }
    | local_decls local_decl
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "local_decls",
            0,
            {[0] = $1, [1] = $2},
            max(($1)->depth, ($2)->depth) + 1, 
            max(max(($1)->lp, ($2)->lp), ($1)->depth + ($2)->depth + 1)
        };
        $$ = a;
    };

local_decl
    : type_spec identifier SEMICOLON
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "SEMICOLON",
            1,
            {},
            1,
            1
        };
        $3 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "local_decl",
            0,
            {[0] = $1, [1] = $2, [2] = $3},
            max(($1)->depth, ($2)->depth) + 1,
            max(max(($1)->lp, ($2)->lp), ($1)->depth + ($2)->depth + 1)
        };
        $$ = b;
    }
    | type_spec identifier LEFTBRACKET expr RIGHTBRACKET SEMICOLON
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "LEFTBRACKET",
            1,
            {},
            1, 
            1
        };
        $3 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "RIGHTBRACKET",
            1,
            {},
            1, 
            1
        };
        $5 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "SEMICOLON",
            1,
            {},
            1, 
            1
        };
        $6 = c;
        struct node *d = new node;
        *d = (struct node)
        {
            "local_decl",
            0,
            {[0] = $1, [1] = $2, [2] = $3, [3] = $4, [4] = $5, [5] = $6},
            max(($1)->depth, ($4)->depth) + 1, 
            max(max(($1)->lp, ($4)->lp), ($1)->depth + ($4)->depth + 1)
        };
        $$ = d;
    };

if_stmt
    : IF LEFTBRACE expr RIGHTBRACE stmt %prec IFX
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "IF",
            1,
            {},
            1, 
            1
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "LEFTBRACE",
            1,
            {},
            1, 
            1
        };
        $2 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "RIGHTBRACE",
            1,
            {},
            1, 
            1
        };
        $4 = c;
        struct node *d = new node;
        *d = (struct node)
        {
            "if_stmt",
            0,
            {[0] = $1, [1] = $2, [2] = $3, [3] = $4, [4] = $5},
            max(($3)->depth, ($5)->depth) + 1, 
            max(max(($3)->lp, ($5)->lp), ($3)->depth + ($5)->depth + 1)
        };
        $$ = d;
    }
    | IF LEFTBRACE expr RIGHTBRACE stmt ELSE stmt
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "IF",
            1,
            {},
            1, 
            1
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "LEFTBRACE",
            1,
            {},
            1, 
            1
        };
        $2 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "RIGHTBRACE",
            1,
            {},
            1, 
            1
        };
        $4 = c;
        struct node *d = new node;
        *d = (struct node)
        {
            "ELSE",
            1,
            {},
            1, 
            1
        };
        $6 = d;
        struct node *e = new node;
        *e = (struct node)
        {
            "if_stmt",
            0,
            {[0] = $1, [1] = $2, [2] = $3, [3] = $4, [4] = $5, [5] = $6, [6] = $7},
            max(max(($3)->depth, ($5)->depth), ($7)->depth) + 1,
            max(max(max(max(max(($3)->lp, ($5)->lp), ($7)->lp), ($3)->depth + ($5)->depth + 1), ($5)->depth + ($7)->depth + 1), ($3)->depth + ($7)->depth + 1)
        };
        $$ = e;
    };

return_stmt
    : RETURN SEMICOLON
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "RETURN",
            1,
            {},
            1, 
            1
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "SEMICOLON",
            1,
            {},
            1, 
            1
        };
        $2 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "return_stmt",
            0,
            {[0] = $1, [1] = $2},
            2, 
            3
        };
        $$ = c;
    }
    | RETURN expr SEMICOLON
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "RETURN",
            1,
            {},
            1, 
            1
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "SEMICOLON",
            1,
            {},
            1, 
            1
        };
        $3 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "return_stmt",
            0,
            {[0] = $1, [1] = $2, [2] = $3},
            .depth =($2)->depth + 1, 
            max(($2)->depth + 2, ($2)->lp)
        };
        $$ = c;
    };

break_stmt
    : BREAK SEMICOLON
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "BREAK",
            1,
            {},
            1, 
            1
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "SEMICOLON",
            1,
            {},
            1, 
            1
        };
        $2 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "break_stmt",
            0,
            {[0] = $1, [1] = $2},
            2, 
            3
        };
        $$ = c;
    };

continue_stmt
    : CONTINUE SEMICOLON
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "CONTINUE",
            1,
            {},
            1, 
            1
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "SEMICOLON",
            1,
            {},
            1, 
            1
        };
        $2 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "continue_stmt",
            0,
            {[0] = $1, [1] = $2},
            2, 
            3
        };
        $$ = c;
    };

switch_stmt
    : SWITCH LEFTBRACE expr RIGHTBRACE LEFTCURLY compound_case default_case RIGHTCURLY
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "SWITCH",
            1,
            {},
            1, 
            1
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "LEFTBRACE",
            1,
            {},
            1, 
            1
        };
        $2 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "RIGHTBRACE",
            1,
            {},
            1, 
            1
        };
        $4 = c;
        struct node *d = new node;
        *d = (struct node)
        {
            "LEFTCURLY",
            1,
            {},
            1, 
            1
        };
        $5 = d;
        struct node *e = new node;
        *e = (struct node)
        {
            "RIGHTCURLY",
            1, 
            {}, 
            .depth  = 1, 
            1
        };
        $8 = e;
        struct node *f = new node;
        *f = (struct node)
        {
            "switch_stmt",
            0,
            {[0] = $1, [1] = $2, [2] = $3, [3] = $4, [4] = $5, [5] = $6, [6] = $7, [7] = $8},
            max(max(($3)->depth, ($6)->depth), ($7)->depth) + 1, 
            max(max(max(max(max(($3)->lp, ($6)->lp), ($7)->lp), ($3)->depth + ($6)->depth + 1), ($6)->depth + ($7)->depth + 1), ($3)->depth + ($7)->depth + 1)
        };
        $$ = f;
    };

compound_case 
    : single_case compound_case
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "compound_case",
            0,
            {[0] = $1, [1] = $2},
            max(($1)->depth, ($2)->depth) + 1,
            max(max(($1)->lp, ($2)->lp), ($1)->depth + ($2)->depth + 1)
        };
        $$ = a;
    }
    | single_case
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "single_case",
            0,
            {[0] = $1},
            ($1)->depth + 1,
            max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
    };

single_case
    : CASE integerLit COLON stmt_list
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "CASE",
            1,
            {},
            1,
            1
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "COLON",
            1,
            {},
            1,
            1
        };
        $3 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "single_case",
            0,
            {[0] = $1, [1] = $2, [2] = $3, [3] = $4},
            max(($2)->depth, ($4)->depth) + 1,
            max(max(($2)->lp, ($4)->lp), ($2)->depth + ($4)->depth + 1)
        };
        $$ = c;
    };

default_case
    : DEFAULT COLON stmt_list
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "DEFAULT",
            1,
            {},
            1,
            1
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "COLON",
            1,
            {},
            1,
            1
        };
        $2 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "default_case",
            0,
            {[0] = $1, [1] = $2, [2] = $3},
            ($3)->depth + 1,
            max(($3)->depth + 2, ($3)->lp)
        };
        $$ = c;
    };

assign_stmt
    : identifier EQUAL expr SEMICOLON
    {
        struct node *b = new node;
        *b = (struct node)
        {
            "SEMICOLON",
            1,
            {},
            1,
            1
        };
        $4 = b;
        struct node *a = new node;
        *a = (struct node)
        {
            "EQUAL",
            0,
            {[0] = $1, [1] = $3},
            max(($1)->depth, ($3)->depth) + 1,
            max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1)
        };
        $2 = a;
        struct node *c = new node;
        *c = (struct node)
        {
            "assign_stmt", 
            0, 
            {[0] = $2, [1] = $4},
            max(($2)->depth, ($4)->depth) + 1,
            max(($2)->lp, ($2)->depth + 2)
        };
        $$ = c;
    }
    | identifier LEFTBRACKET expr RIGHTBRACKET EQUAL expr SEMICOLON
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "LEFTBRACKET",
            1,
            {},
            1,
            1
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "RIGHTBRACKET",
            1,
            {},
            1,
            1
        };
        $4 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "SEMICOLON",
            1,
            {},
            1,
            1
        };
        $7 = c;
        struct node *e = new node;
        *e = (struct node)
        {
            "EQUAL", 
            0, 
            {[0] = $1, [1] = $2, [2] = $3, [3] = $4, [4] = $6},
            max(max(($6)->depth, ($3)->depth), ($1)->depth) + 1,
            max(max(max(max(max(($6)->lp, ($3)->lp), ($1)->lp), ($3)->depth + ($6)->depth + 1), ($1)->depth + ($3)->depth + 1), ($1)->depth + ($6)->depth + 1)
        };
        $5 = e;
        struct node *d = new node;
        *d = (struct node)
        {
            "assign_stmt",
            0,
            {[0] = $5, [1] = $7},
            max(($5)->depth, ($7)->depth) + 1, 
            max(max(($5)->lp, ($7)->lp), ($5)->depth + ($7)->depth + 1)
        };
        $$= d;
    }
    | identifier STRUCTPOINT identifier EQUAL expr SEMICOLON
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "STRUCTPOINT",
            1, 
            {},
            1,
            1
        };
        $2 = a;
        struct node *c = new node;
        *c = (struct node)
        {
            "SEMICOLON", 
            1, 
            {}, 
            1, 
            1
        };
        $6 = c;
        struct node *b = new node;
        *b = (struct node)
        {
            "EQUAL",
            0,
            {[0] = $1, [1] = $2, [2] = $3, [3] = $5},
            max(($1)->depth, ($5)->depth) + 1,
            max(max(($1)->lp, ($5)->lp), ($1)->depth + ($5)->depth + 1)
        };
        $4 = b;
        struct node *d = new node;
        *d = (struct node)
        {
            "assign_stmt",
            0, 
            {[0] = $4, [1] = $6}, 
            max(($4)->depth, ($6)->depth) + 1, 
            max(max(($4)->lp, ($6)->lp), ($4)->depth + ($6)->depth + 1)
        };
        $$ = d;
    }
    | identifier STRUCTNORM identifier EQUAL expr SEMICOLON
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "STRUCTNORM",
            1, 
            {},
            1,
            1
        };
        $2 = a;
        struct node *c = new node;
        *c = (struct node)
        {
            "SEMICOLON", 
            1, 
            {}, 
            1, 
            1
        };
        $6 = c;
        struct node *b = new node;
        *b = (struct node)
        {
            "EQUAL",
            0,
            {[0] = $1, [1] = $2, [2] = $3, [3] = $5},
            max(($1)->depth, ($5)->depth) + 1,
            max(max(($1)->lp, ($5)->lp), ($1)->depth + ($5)->depth + 1)
        };
        $4 = b;
        struct node *d = new node;
        *d = (struct node)
        {
            "assign_stmt",
            0, 
            {[0] = $4, [1] = $6}, 
            max(($4)->depth, ($6)->depth) + 1, 
            max(max(($4)->lp, ($6)->lp), ($4)->depth + ($6)->depth + 1)
        };
        $$ = d;
    };

incr_stmt
    : identifier INCR SEMICOLON
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "SEMICOLON", 
            1, 
            {}, 
            1, 
            1
        };
        $3 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "INCR", 
            1, 
            {}, 
            1,
            1
        };
        $2 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "incr_stmt", 
            0, 
            {[0] = $1, [1] = $2, [2] = $3}, 
            max(($1)->depth, ($2)->depth) + 1,
            max(max(($1)->depth + 1 + ($2)->depth, ($2)->lp), ($1)->lp)
        };
        $$ = c;
    };

decr_stmt
    : identifier DECR SEMICOLON
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "SEMICOLON", 
            1, 
            {}, 
            1, 
            1
        };
        $3 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "DECR", 
            0, 
            {}, 
            1,
            1 
        };
        $2 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "decr_stmt", 
            0, 
            {[0] = $1, [1] = $2, [2] = $3}, 
            max(($1)->depth, ($2)->depth) + 1,
            max(max(($1)->depth + 1 + ($2)->depth, ($1)->lp), ($2)->lp)
        };
        $$ = c;
    };

expr
    : Pexpr LT Pexpr
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "LT", 
            0, 
            {[0] = $1, [1] = $3}, 
            max(($1)->depth, ($3)->depth) + 1,
            max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1)
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr", 
            0, 
            {[0] = $2}, 
            ($2)->depth + 1, 
            max(($2)->depth + 1, ($2)->lp)
        };
        $$ = b;
    }
    | Pexpr GT Pexpr
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "GT", 
            0, 
            {[0] = $1, [1] = $3}, 
            max(($1)->depth, ($3)->depth) + 1,
            max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1)
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr", 
            0, 
            {[0] = $2}, 
            ($2)->depth + 1, 
            max(($2)->depth + 1, ($2)->lp)
        };
        $$ = b;
    }
    | Pexpr LEQ Pexpr
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "LEQ", 
            0, 
            {[0] = $1, [1] = $3}, 
            max(($1)->depth, ($3)->depth) + 1,
            max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1)
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr", 
            0, 
            {[0] = $2}, 
            ($2)->depth + 1, 
            max(($2)->depth + 1, ($2)->lp)
        };
        $$ = b;
    }
    | Pexpr GEQ Pexpr
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "GEQ", 
            0, 
            {[0] = $1, [1] = $3}, 
            max(($1)->depth, ($3)->depth) + 1,
            max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1)
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr", 
            0, 
            {[0] = $2}, 
            ($2)->depth + 1, 
            max(($2)->depth + 1, ($2)->lp)
        };
        $$ = b;
    }
    | Pexpr LOGOR Pexpr
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "LOGOR", 
            0, 
            {[0] = $1, [1] = $3}, 
            max(($1)->depth, ($3)->depth) + 1,
            max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1)
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr", 
            0, 
            {[0] = $2}, 
            ($2)->depth + 1, 
            max(($2)->depth + 1, ($2)->lp)
        };
        $$ = b;
    }
    | Pexpr LOGAND Pexpr
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "LOGAND", 
            0, 
            {[0] = $1, [1] = $3}, 
            max(($1)->depth, ($3)->depth) + 1,
            max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1)
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr", 
            0, 
            {[0] = $2}, 
            ($2)->depth + 1, 
            max(($2)->depth + 1, ($2)->lp)
        };
        $$ = b;
    }
    | SIZEOF LEFTBRACE Pexpr RIGHTBRACE
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "SIZEOF", 
            1, 
            {}, 
            1,
            1
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "LEFTBRACE", 
            1, 
            {}, 
            1,
            1
        };
        $2 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "RIGHTBRACE", 
            1, 
            {}, 
            1,
            1
        };
        $4 = c;
        struct node *d = new node;
        *d = (struct node)
        {
            "expr", 
            0, 
            {[0] = $1, [1] = $2, [2] = $3, [3] = $4},
            ($3)->depth + 1,
            max(($3)->depth + 2, ($3)->lp)
        };
        $$ = d;
    }
    | Pexpr STRUCTPOINT Pexpr
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "STRUCTPOINT", 
            0, 
            {[0] = $1, [1] = $3}, 
            max(($1)->depth, ($3)->depth) + 1,
            max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1)
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr", 
            0, 
            {[0] = $2}, 
            ($2)->depth + 1, 
            max(($2)->depth + 1, ($2)->lp)
        };
        $$ = b;
    }
    | Pexpr ISEQ Pexpr
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "ISEQ", 
            0, 
            {[0] = $1, [1] = $3}, 
            max(($1)->depth, ($3)->depth) + 1,
            max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1)
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr", 
            0, 
            {[0] = $2}, 
            ($2)->depth + 1, 
            max(($2)->depth + 1, ($2)->lp)
        };
        $$ = b;
    }
    | Pexpr NEQ Pexpr
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "NEQ", 
            0, 
            {[0] = $1, [1] = $3}, 
            max(($1)->depth, ($3)->depth) + 1,
            max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1)
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr", 
            0, 
            {[0] = $2}, 
            ($2)->depth + 1, 
            max(($2)->depth + 1, ($2)->lp)
        };
        $$ = b;
    }
    | Pexpr THREEWAY Pexpr
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "THREEWAY", 
            0, 
            {[0] = $1, [1] = $3}, 
            max(($1)->depth, ($3)->depth) + 1,
            max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1)
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr", 
            0, 
            {[0] = $2}, 
            ($2)->depth + 1, 
            max(($2)->depth + 1, ($2)->lp)
        };
        $$ = b;
    }
    | Pexpr PLUS Pexpr
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "PLUS", 
            0, 
            {[0] = $1, [1] = $3}, 
            max(($1)->depth, ($3)->depth) + 1,
            max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1)
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr", 
            0, 
            {[0] = $2}, 
            ($2)->depth + 1, 
            max(($2)->depth + 1, ($2)->lp)
        };
        $$ = b;
    }
    | Pexpr MINUS Pexpr
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "MINUS", 
            0, 
            {[0] = $1, [1] = $3}, 
            max(($1)->depth, ($3)->depth) + 1,
            max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1)
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr", 
            0, 
            {[0] = $2}, 
            ($2)->depth + 1, 
            max(($2)->depth + 1, ($2)->lp)
        };
        $$ = b;
    }
    | Pexpr STAR Pexpr
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "STAR", 
            0, 
            {[0] = $1, [1] = $3}, 
            max(($1)->depth, ($3)->depth) + 1,
            max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1)
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr", 
            0, 
            {[0] = $2}, 
            ($2)->depth + 1, 
            max(($2)->depth + 1, ($2)->lp)
        };
        $$ = b;
    }
    | Pexpr DIV Pexpr
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "DIV", 
            0, 
            {[0] = $1, [1] = $3}, 
            max(($1)->depth, ($3)->depth) + 1,
            max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1)
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr", 
            0, 
            {[0] = $2}, 
            ($2)->depth + 1, 
            max(($2)->depth + 1, ($2)->lp)
        };
        $$ = b;
    }
    | Pexpr MOD Pexpr
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "MOD", 
            0, 
            {[0] = $1, [1] = $3}, 
            max(($1)->depth, ($3)->depth) + 1,
            max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1)
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr", 
            0, 
            {[0] = $2}, 
            ($2)->depth + 1, 
            max(($2)->depth + 1, ($2)->lp)
        };
        $$ = b;
    }
    | NOT Pexpr
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "NOT", 
            1, 
            {}, 
            1, 
            1
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr", 
            0, 
            {[0] = $1, [1] = $2}, 
            ($2)->depth + 1,
            max(($2)->depth + 2, ($2)->lp)
        };
        $$ = b;
    }
    | MINUS Pexpr
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "MINUS", 
            1, 
            {}, 
            1, 
            1
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr", 
            0, 
            {[0] = $1, [1] = $2}, 
            ($2)->depth + 1,
            max(($2)->depth + 2, ($2)->lp)
        };
        $$ = b;
    }
    | PLUS Pexpr
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "PLUS", 
            1, 
            {}, 
            1, 
            1
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr", 
            0, 
            {[0] = $1, [1] = $2}, 
            ($2)->depth + 1,
            max(($2)->depth + 2, ($2)->lp)
        };
        $$ = b;
    }
    | STAR Pexpr
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "STAR", 
            1, 
            {}, 
            1, 
            1
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr", 
            0, 
            {[0] = $1, [1] = $2}, 
            ($2)->depth + 1,
            max(($2)->depth + 2, ($2)->lp)
        };
        $$ = b;
    }
    | AND Pexpr
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "AND", 
            1, 
            {}, 
            1, 
            1
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr", 
            0, 
            {[0] = $1, [1] = $2}, 
            ($2)->depth + 1,
            max(($2)->depth + 2, ($2)->lp)
        };
        $$ = b;
    }
    | Pexpr
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "expr", 
            0, 
            {[0] = $1}, 
            ($1)->depth + 1, 
            max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
    }
    | identifier LEFTBRACE args RIGHTBRACE
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "LEFTBRACE", 
            1, 
            {}, 
            1, 
            1
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "RIGHTBRACE", 
            1, 
            {}, 
            1, 
            1
        };
        $4 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "expr", 
            0, 
            {[0] = $1, [1] = $2, [2] = $3, [3] = $4}, 
            max(($1)->depth, ($3)->depth) + 1,
            max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1)
        };
        $$ = c;
    }
    | identifier LEFTBRACKET expr RIGHTBRACKET
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "LEFTBRACKET", 
            1, 
            {}, 
            1, 
            1
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "RIGHTBRACKET", 
            1, 
            {}, 
            1, 
            1
        };
        $4 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "expr", 
            0, 
            {[0] = $1, [1] = $2, [2] = $3, [3] = $4}, 
            max(($1)->depth, ($3)->depth) + 1,
            max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1)
        };
        $$ = c;
    };

Pexpr
    : integerLit 
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "Pexpr", 
            0, 
            {[0] = $1}, 
            ($1)->depth + 1, 
            max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
    }
    | floatLit
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "Pexpr", 
            0, 
            {[0] = $1}, 
            ($1)->depth + 1, 
            max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
    }
    | identifier 
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "Pexpr", 
            0, 
            {[0] = $1}, 
            ($1)->depth + 1, 
            max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
    }
    | LEFTBRACE expr RIGHTBRACE
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "LEFTBRACE", 
            1, 
            {}, 
            1, 
            1
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "RIGHTBRACE", 
            1, 
            {}, 
            1, 
            1
        };
        $3 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "Pexpr", 
            0, 
            {[0] = $1, [1] = $2, [2] = $3}, 
            ($2)->depth + 1, 
            max(($2)->depth + 2, ($2)->lp)
        };
        $$ = c;
    };

arg_list
    : arg_list COMMA expr
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "COMMA", 
            1, 
            {}, 
            1, 
            1
        };
        $2 = a;
        struct node *c = new node;
        *c = (struct node)
        {
            "arg_list", 
            0, 
            {[0] = $1, [1] = $2, [2] = $3}, 
            max(($1)->depth, ($3)->depth) + 1, 
            max(max(($1)->depth + 1 + ($3)->depth, ($1)->lp), ($3)->lp)
        };
        $$ = c;
    }
    | expr
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "arg_list", 
            0, 
            {[0] = $1}, 
            ($1)->depth + 1, 
            max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
    };

args 
    : /*empty*/
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "epsilon",
            1,
            {},
            1, 
            1
        };
        struct node *b = new node;
        *b = (struct node)
        {
            "args",
            0,
            {[0] = a},
            2,
            2
        };
        $$ = b;
    }
    | arg_list
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "args", 
            0, 
            {[0] = $1}, 
            ($1)->depth + 1, 
            max(($1)->depth + 1, ($1)->lp)
        };
        $$ = a;
    };

integerLit
    : INTCONSTANT
    {
        struct node *b = new node;
        *b = (struct node)
        {
            "",
            1, 
            {}, 
            1, 
            1
        };
        $1 = b;
        struct node *a = new node;
        *a = (struct node)
        {
            "integerLit",
            0, 
            {[0] = $1}, 
            2, 
            2
        };
        b->label = yytext;
        $$ = a;
    };

floatLit
    : FLOATCONSTANT
    {
        struct node *b = new node;
        *b = (struct node)
        {
            "",
            1, 
            {}, 
            1, 
            1
        };
        $1 = b;
        struct node *a = new node;
        *a = (struct node)
        {
            "floatLit",
            0, 
            {[0] = $1}, 
            2, 
            2
        };
        b->label = yytext;
        $$ = a;
    };

identifier
    : ID 
    {
        struct node *b = new node;
        *b = (struct node)
        {
            "",
            1, 
            {}, 
            1, 
            1
        };
        $1 = b;
        struct node *a = new node;
        *a = (struct node)
        {
            "identifier",
            0, 
            {[0] = $1}, 
            2, 
            2
        };
        b->label = yytext;
        $$ = a;
    };
%%
void yyerror(char *s) 
{
    printf("syntax error\n");
    exit(0);
}

void initializeRecord(string identifier, int type)
{
    record* newRecord = new record;
    *newRecord = (record)
    { 
        identifier, 
        -1,
        type
    };
    symbolTable[identifier] = newRecord;
}
/*
ll exprEvaluator(struct node* n)
{
    if(n->label == "Pexpr")
    {
        if(n->children[0]->label == "integerLit")
        {
            return stoi(n->children[0]->children[0]->label);
        }
        else if(n->children[0]->label == "identifier")
        {
            string id = n->children[0]->children[0]->label;
            ll temp = symbolTable[id]->value;
            return temp;
        }
        else if(n->children[0]->label == "LEFTBRACE")
        {
            return exprEvaluator(n->children[1]);
        }

    }
    else if(n->label == "expr")
    {
        if(n->children[0]->label == "Pexpr" && n->children[1] == NULL)
        {
            return exprEvaluator(n->children[0]);
        }
        else if(n->children[1] != NULL && n->children[1]->label == "Pexpr")
        {
            ll temp1 = exprEvaluator(n->children[1]);
            if(n->children[0]->label == "NOT")
            {
                return !temp1;
            }
            else if(n->children[0]->label == "MINUS")
            {
                return -temp1;
            }
            else if(n->children[0]->label == "PLUS")
            {
                return temp1;
            }
        }
        else if(n->children[0]->label == "SIZEOF")
        {
            return 4;
        }
        else 
        {
            ll temp1 = exprEvaluator(n->children[0]->children[0]);
            ll temp2 = exprEvaluator(n->children[0]->children[1]);
            if(n->children[0]->label == "LT")
            {
                return temp1 < temp2;
            }
            else if(n->children[0]->label == "GT")
            {
                return temp1 > temp2;
            }
            else if(n->children[0]->label == "LEQ")
            {
                return temp1 <= temp2;
            }
            else if(n->children[0]->label == "GEQ")
            {
                return temp1 >= temp2;
            }
            else if(n->children[0]->label == "LOGOR")
            {
                return temp1 || temp2;
            }
            else if(n->children[0]->label == "LOGAND")
            {
                return temp1 && temp2;
            }
            else if(n->children[0]->label == "ISEQ")
            {
                return temp1 == temp2;
            }
            else if(n->children[0]->label == "NEQ")
            {
                return temp1 != temp2;
            }
            else if(n->children[0]->label == "THREEWAY")
            {
                if(temp1 < temp2)
                {
                    return -1;
                } 
                else if(temp1 == temp2)
                {
                    return 0;
                }
                else if(temp1 > temp2)
                {
                    return 1;
                }
            }
            else if(n->children[0]->label == "PLUS")
            {
                return temp1 + temp2;
            }
            else if(n->children[0]->label == "MINUS")
            {
                return temp1 - temp2;
            }
            else if(n->children[0]->label == "STAR")
            {
                return temp1 * temp2;
            }
            else if(n->children[0]->label == "DIV")
            {
                return temp1 / temp2;
            }
            else if(n->children[0]->label == "MOD")
            {
                return temp1 % temp2;
            }
        }
    }
    return -1;
}
*/

string labelGenerator(string labelType)
{
    static int counter;
    counter++;
    return labelType + "." + to_string(counter);
}

void fillArg(struct node* n, ll num)
{
    record* newRecord = new record;
    newRecord->identifier = n->children[1]->children[0]->label;
    newRecord->stackOffset = num * 8;
    if(n->children[2] == NULL) //int
    {
        newRecord->type = 0;
    }
    else //arrays
    {
        newRecord->type = 1;
    }
    symbolTable[newRecord->identifier] = newRecord;
}


void getnargs(struct node* n, ll num)
{
    if(n->children[1] == NULL)
    {
        fillArg(n->children[0], num);
    }
    else
    {
        fillArg(n->children[2], num);
        getnargs(n->children[0], num + 1);
    }
} 

void getnargsHelper(struct node* n) //passed with n->label == "params"
{
    if(n->children[0]->label == "epsilon")
    {
        return;
    }
    else if(n->children[0]->label == "param_list")
    {
        getnargs(n->children[0], 2);
    }
}

void codeGenerator(struct node*);

void pushArgs(struct node* n)
{
    if(n->children[1] == NULL)
    {
        codeGenerator(n->children[0]);
    }
    else
    {
        pushArgs(n->children[0]);
        codeGenerator(n->children[2]);
    }
}

void pushArgsHelper(struct node* n)
{
    if(n->children[0]->label == "epsilon")
    {
        return;
    }
    else if(n->children[0]->label == "arg_list")
    {
        pushArgs(n->children[0]);
    }
}

void popArgs(struct node* n)
{
    if(n->children[1] == NULL)
    {
        asmCode.push_back("popq %rcx");
    }
    else
    {
        popArgs(n->children[0]);
        asmCode.push_back("popq %rcx");
    }
}

void popArgsHelper(struct node* n)
{
    if(n->children[0]->label == "epsilon")
    {
        return;
    }
    else if(n->children[0]->label == "arg_list")
    {
        popArgs(n->children[0]);
    }
}

void codeGenerator(struct node* n)
{
    static ll currentStackOffset;
    static bool inMain = 0;
    static bool returnStmt = 0;
    static string currentFunctionName  = "noName";
    if(n->label == "func_decl")
    {
        symbolTable.clear();
        returnStmt = 0;
        currentStackOffset = -24;
        currentFunctionName = n->children[1]->children[0]->label;
        if(n->children[1]->children[0]->label == "main")
        {
            inMain = 1;
            asmCode.push_back(".globl main");
            asmCode.push_back(".type main, @function");
            asmCode.push_back("main:");
            asmCode.push_back(".LFB0:");
            asmCode.push_back(".cfi_startproc");
            asmCode.push_back("endbr64");
            asmCode.push_back("pushq %rbp");
            asmCode.push_back(".cfi_def_cfa_offset 16");
            asmCode.push_back(".cfi_offset 6, -16");
            asmCode.push_back("movq %rsp, %rbp");
            asmCode.push_back(".cfi_def_cfa_register 6");
            asmCode.push_back("subq	$16, %rsp"); //stack alignment
            codeGenerator(n->children[5]);
            if(returnStmt != 1)
            {
                asmCode.push_back("movl $0, %eax"); //default return value, used if return does not exist in the body of the function
            }
            asmCode.push_back(".leave_main_label:");
            asmCode.push_back("leave");
            asmCode.push_back(".cfi_def_cfa 7, 8");
            asmCode.push_back("ret");
            asmCode.push_back(".cfi_endproc");
            inMain = 0;
        }
        else
        {
            asmCode.push_back(".globl " + n->children[1]->children[0]->label);
            asmCode.push_back(".type " + n->children[1]->children[0]->label + ", @function");
            asmCode.push_back(n->children[1]->children[0]->label + ":");
            asmCode.push_back("pushq %rbp");
            asmCode.push_back("movq %rsp, %rbp");
            asmCode.push_back("subq	$16, %rsp");
            getnargsHelper(n->children[3]);
            codeGenerator(n->children[5]);
            if(returnStmt != 1)
            {
                asmCode.push_back("movl $0, %eax");
            }
            asmCode.push_back(".leave_" + n->children[1]->children[0]->label + "_label:");
            asmCode.push_back("leave");
            asmCode.push_back("ret");
        }
    }
    else if(n->label == "compound_stmt")
    {
        codeGenerator(n->children[1]);
        codeGenerator(n->children[2]);
    }
    else if(n->label == "local_decl")
    {
        if(n->children[2]->label == "SEMICOLON")
        {
            initializeRecord(n->children[1]->children[0]->label, 0);
            asmCode.push_back("pushq $0");
            symbolTable[n->children[1]->children[0]->label]->stackOffset = currentStackOffset;
            currentStackOffset -= 8;
        }
        else 
        {
            initializeRecord(n->children[1]->children[0]->label, 1);
            codeGenerator(n->children[3]);
            asmCode.push_back("popq %rdi");
            asmCode.push_back("imull $4, %edi");
            asmCode.push_back("pushq %rdi");
            asmCode.push_back("call malloc@PLT");
            asmCode.push_back("pushq $0");
            asmCode.push_back("movq %rax, (%rsp)");
            symbolTable[n->children[1]->children[0]->label]->stackOffset = currentStackOffset - 8;
            currentStackOffset -= 16;
        }
    }
    else if(n->label == "assign_stmt")
    {
        int i;
        for(i = 0; i < MAX && n->children[0]->children[i] != NULL ; i++);
        if(i == 2)
        {
            codeGenerator(n->children[0]->children[1]);
            asmCode.push_back("popq %rax");
            string id = n->children[0]->children[0]->children[0]->label;
            ll stkOffset = symbolTable[id]->stackOffset;
            asmCode.push_back("movl %eax, " + to_string((int)stkOffset) + "(%rbp)");
        }
        else if(i == 5)
        {
            codeGenerator(n->children[0]->children[2]);
            codeGenerator(n->children[0]->children[4]);
            asmCode.push_back("popq %rdx");
            asmCode.push_back("popq %rcx");
            string id = n->children[0]->children[0]->children[0]->label;
            ll stkOffset = symbolTable[id]->stackOffset;
            asmCode.push_back("movq " + to_string((int)stkOffset) + "(%rbp), %rax");
            asmCode.push_back("imull $4, %ecx");
            asmCode.push_back("addq %rcx, %rax");
            asmCode.push_back("movl %edx, (%rax)");
        }
    }
    else if(n->label == "if_stmt")
    {
        if(n->children[5] == NULL)  //if statement
        {
            codeGenerator(n->children[2]);
            asmCode.push_back("popq %rax");
            string newLabel = labelGenerator("if_stmt");
            asmCode.push_back("movl $0, %ecx");
            asmCode.push_back("movl %eax, %edx");
            asmCode.push_back("cmpl %ecx, %edx");
            asmCode.push_back("jle " + newLabel);
            codeGenerator(n->children[4]);
            asmCode.push_back("" + newLabel + ":");
        } 
        else
        {
            codeGenerator(n->children[2]);
            asmCode.push_back("popq %rax");
            string newLabel = labelGenerator("else_stmt");
            string endLabel = labelGenerator("end");
            asmCode.push_back("movl $0, %ecx");
            asmCode.push_back("movl %eax, %edx");
            asmCode.push_back("cmpl %ecx, %edx");
            asmCode.push_back("jle " + newLabel);
            codeGenerator(n->children[4]);
            asmCode.push_back("jmp " + endLabel);
            asmCode.push_back("" + newLabel + ":");
            codeGenerator(n->children[6]);
            asmCode.push_back("" + endLabel + ":");
        }
    }
    else if(n->label == "while_stmt")
    {
        string begLabel = labelGenerator("while_stmt_begin");
        string endLabel = labelGenerator("while_stmt_end");
        asmCode.push_back("" + begLabel + ":");
        codeGenerator(n->children[2]);
        asmCode.push_back("popq %rax");
        asmCode.push_back("movl $0, %ecx");
        asmCode.push_back("movl %eax, %edx");
        asmCode.push_back("cmpl %ecx, %edx");
        asmCode.push_back("jle " + endLabel);
        codeGenerator(n->children[4]);
        asmCode.push_back("jmp " + begLabel);
        asmCode.push_back("" + endLabel + ":");
    }
    else if(n->label == "incr_stmt")
    {
        string id = n->children[0]->children[0]->label;
        ll stkOffset = symbolTable[id]->stackOffset;
        asmCode.push_back("incl "+ to_string((int)stkOffset) + "(%rbp)");
    }
    else if(n->label == "decr_stmt")
    {
        string id = n->children[0]->children[0]->label;
        ll stkOffset = symbolTable[id]->stackOffset;
        asmCode.push_back("decl "+ to_string((int)stkOffset) + "(%rbp)");
    }
    else if(n->label == "return_stmt")
    {
        returnStmt = 1;
        if(n->children[1]->label == "SEMICOLON")
        {
            asmCode.push_back("movl $0, %eax");
        }
        else 
        {
            codeGenerator(n->children[1]);
            asmCode.push_back("popq %rax");
        }
        asmCode.push_back("jmp .leave_" + currentFunctionName + "_label");    

    }
    else if(n->label == "print_stmt")
    {
        string id = n->children[4]->children[0]->label; 
        ll stkOffset = symbolTable[id]->stackOffset;
        asmCode.push_back("movl " + to_string((int)stkOffset) + "(%rbp), %esi");
        asmCode.push_back("leaq .LC0(%rip), %rdi");
        asmCode.push_back("movl $0, %eax");
        asmCode.push_back("call printf@PLT");
    }
    else if(n->label == "Pexpr")
    {
        if(n->children[0]->label == "identifier")
        {
            string id = n->children[0]->children[0]->label;
            ll stkOffset = symbolTable[id]->stackOffset;
            asmCode.push_back("movq " + to_string((int)stkOffset) + "(%rbp), %rax"); //allowing 64 bit in case of array addresses
            asmCode.push_back("pushq %rax");
        }
        else if(n->children[0]->label == "integerLit")
        {
            asmCode.push_back("movl $" + n->children[0]->children[0]->label + ", %eax");
            asmCode.push_back("pushq %rax");
        }
        else if(n->children[0]->label == "LEFTBRACE")
        {
            codeGenerator(n->children[1]);
        }
    }
    else if(n->label == "expr")
    {
        if(n->children[0]->label == "Pexpr" && n->children[1] == NULL)
        {
            codeGenerator(n->children[0]);
        }
        else if(n->children[1] != NULL && n->children[1]->label == "Pexpr")
        {
            codeGenerator(n->children[1]);
            if(n->children[0]->label == "NOT")
            {
                asmCode.push_back("popq %rax");
                asmCode.push_back("cmpl $0, %eax");
                asmCode.push_back("setle %al");
                asmCode.push_back("movzbl %al, %eax");
                asmCode.push_back("pushq %rax");
            }
            else if(n->children[0]->label == "MINUS")
            {
                asmCode.push_back("popq %rax");
                asmCode.push_back("negl %eax");
                asmCode.push_back("pushq %rax");
            }
            else if(n->children[0]->label == "PLUS")
            {
                //do nothing, value in stack is correct
            }
        }
        else if(n->children[0]->label == "SIZEOF")
        {
            a:
            if(n->children[2]->children[0]->label == "identifier")
            {
                string id = n->children[2]->children[0]->children[0]->label;
                if(symbolTable[id]->type == 1) //array identifier
                {
                    ll stkOffset = symbolTable[id]->stackOffset;
                    stkOffset += 8; //pointer to the size of the corresponding array, maintained on the stack
                    asmCode.push_back("movl " + to_string((int)stkOffset) + "(%rbp), %eax");
                    asmCode.push_back("pushq %rax");
                    return;
                }
            }
            else if(n->children[2]->children[0]->label == "LEFTBRACKET")
            {
                goto a;
            }
            asmCode.push_back("pushq $4");
        }
        else if(n->children[0]->label == "identifier" && n->children[1]->label == "LEFTBRACKET")
        {
            codeGenerator(n->children[2]);
            asmCode.push_back("popq %rcx");
            string id = n->children[0]->children[0]->label;
            ll stkOffset = symbolTable[id]->stackOffset;
            asmCode.push_back("movq " + to_string((int)stkOffset) + "(%rbp), %rax");
            asmCode.push_back("imull $4, %ecx");
            asmCode.push_back("addq %rcx, %rax");
            asmCode.push_back("movl (%rax), %eax");
            asmCode.push_back("pushq %rax");
        }
        else if(n->children[0]->label == "identifier" && n->children[1]->label == "LEFTBRACE")
        {
            pushArgsHelper(n->children[2]);
            asmCode.push_back("call " + n->children[0]->children[0]->label);
            popArgsHelper(n->children[2]);
            asmCode.push_back("pushq %rax");
        }
        else 
        {
            if(n->children[0]->label == "LOGOR" || n->children[0]->label == "LOGAND")
            {
                if(n->children[0]->label == "LOGOR")
                {
                    string set1 = labelGenerator("logor_set1");
                    string end = labelGenerator("logor_end");
                    codeGenerator(n->children[0]->children[0]);
                    asmCode.push_back("popq %rax");
                    asmCode.push_back("cmpl $0, %eax");
                    asmCode.push_back("jne " + set1);
                    codeGenerator(n->children[0]->children[1]);
                    asmCode.push_back("popq %rax");
                    asmCode.push_back("cmpl $0, %eax");
                    asmCode.push_back("jne " + set1);
                    asmCode.push_back("pushq $0");
                    asmCode.push_back("jmp " + end);
                    asmCode.push_back(set1 + ":");
                    asmCode.push_back("pushq $1");
                    asmCode.push_back(end + ":");
                }
                else
                {
                    string set0 = labelGenerator("logand_set0");
                    string end = labelGenerator("logand_end");
                    codeGenerator(n->children[0]->children[0]);
                    asmCode.push_back("popq %rax");
                    asmCode.push_back("cmpl $0, %eax");
                    asmCode.push_back("je " + set0);
                    codeGenerator(n->children[0]->children[1]);
                    asmCode.push_back("popq %rax");
                    asmCode.push_back("cmpl $0, %eax");
                    asmCode.push_back("je " + set0);
                    asmCode.push_back("pushq $1");
                    asmCode.push_back("jmp " + end);
                    asmCode.push_back(set0 + ":");
                    asmCode.push_back("pushq $0");
                    asmCode.push_back(end + ":");
                }
            }
            else 
            {
                codeGenerator(n->children[0]->children[0]);
                codeGenerator(n->children[0]->children[1]);
                asmCode.push_back("popq %rcx");
                asmCode.push_back("popq %rax");
                if(n->children[0]->label == "LT")
                {
                    asmCode.push_back("cmpl %ecx, %eax");
                    asmCode.push_back("setl %al");
                    asmCode.push_back("movzbl %al, %eax");
                }
                else if(n->children[0]->label == "GT")
                {
                    asmCode.push_back("cmpl %ecx, %eax");
                    asmCode.push_back("setg %al");
                    asmCode.push_back("movzbl %al, %eax");
                }
                else if(n->children[0]->label == "LEQ")
                {
                    asmCode.push_back("cmpl %ecx, %eax");
                    asmCode.push_back("setle %al");
                    asmCode.push_back("movzbl %al, %eax");
                }
                else if(n->children[0]->label == "GEQ")
                {
                    asmCode.push_back("cmpl %ecx, %eax");
                    asmCode.push_back("setge %al");
                    asmCode.push_back("movzbl %al, %eax");
                }
                else if(n->children[0]->label == "ISEQ")
                {
                    asmCode.push_back("cmpl %ecx, %eax");
                    asmCode.push_back("sete %al");
                    asmCode.push_back("movzbl %al, %eax");
                }
                else if(n->children[0]->label == "NEQ")
                {
                    asmCode.push_back("cmpl %ecx, %eax");
                    asmCode.push_back("setne %al");
                    asmCode.push_back("movzbl %al, %eax");
                }
                else if(n->children[0]->label == "THREEWAY")
                {
                    asmCode.push_back("cmpl %ecx, %eax");
                    string label1 = labelGenerator("thrway");
                    string label2 = labelGenerator("thrway");
                    string label3 = labelGenerator("thrway");
                    asmCode.push_back("jg " + label1);
                    asmCode.push_back("je " + label2);
                    asmCode.push_back("movl $-1, %eax");
                    asmCode.push_back("jmp " + label3);
                    asmCode.push_back(label2 + ":");
                    asmCode.push_back("movl $0, %eax");
                    asmCode.push_back("jmp " + label3);
                    asmCode.push_back(label1 + ":");
                    asmCode.push_back("movl $1, %eax");
                    asmCode.push_back(label3 + ":");
                }
                else if(n->children[0]->label == "PLUS")
                {
                    asmCode.push_back("addl %ecx, %eax");
                }
                else if(n->children[0]->label == "MINUS")
                {
                    asmCode.push_back("subl %ecx, %eax");
                }
                else if(n->children[0]->label == "STAR")
                {
                    asmCode.push_back("imull %ecx, %eax");
                }
                else if(n->children[0]->label == "DIV")
                {
                    asmCode.push_back("cltd");
                    asmCode.push_back("idivl %ecx");
                }
                else if(n->children[0]->label == "MOD")
                {
                    asmCode.push_back("cltd");
                    asmCode.push_back("idivl %ecx");
                    asmCode.push_back("movl %edx, %eax");
                }
                asmCode.push_back("pushq %rax");
            }
        }
    }
    else
    {
        for(int i = 0; i < MAX && n->children[i] != NULL; i++)
        {
            codeGenerator(n->children[i]);
        }
    }
    
}

int main(int argc, char* argv[]) 
{
    int a = yyparse();
    if(a == 0)
    {
        //printtree(ast, 0);
        asmCode.push_back(".text");
        asmCode.push_back(".section	.rodata");
        asmCode.push_back(".LC0:");
        asmCode.push_back(".string \"%d\\n\"");
        asmCode.push_back(".text");
        codeGenerator(ast);
        for(ll i = 0; i < asmCode.size(); i++)
        {
            cout<<asmCode[i]<<endl;
        }
    }
    else
    {
        cout<<"syntax error"<<endl;
        exit(0);
    }
    return 0;
}