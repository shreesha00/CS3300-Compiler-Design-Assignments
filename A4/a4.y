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
        bool used;
        bool valueKnown;
        int value;
        ll lineNumber;
        ll declarationNumber;
    } record;
    #define MAX 10
    void yyerror(char*);
    int yylex(void);
	char mytext[10000];
	extern char *yytext;
    extern int commentError;
    extern long long int lineNumber;
    struct node
    {
        string label; //label name
        bool isLeaf; //isLeaf = 1 -> Leaf node. Else isLeaf = 0;
        struct node *children[MAX]; //at max 10 children for each node
        ll depth; //depth represents the height of it's subtree.
        ll lp; //longest path in subtree
        ll lineNumber;
        string postfixExpr;
        set<string> identifiersInExpr;
        bool valueKnown;
        int value;
        int maximalConsFold;
        bool consFoldOccured;
        bool atLeastOneOperator;
        multiset<pair<string, ll> > exprSet;
    };
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
    struct node* ast = NULL;
    ll longestPath = 0;
    ll longestPathIf = 0;
    ll longestPathWhile = 0;
    ll longestPathSwitch = 0;
    ll longestPathMain = 0;
    bool isMain = 0;
    vector<string> asmCode;
    vector<string> summaryText;
    vector<pair<ll, ll> > consFold;
    set<string> unusedVars;
    unordered_map<string, record*> symbolTable;   
    unordered_map<string, record*> firstPassSymbolTable;   
    unordered_map<string, record*> ifSymbolTable;
    unordered_map<string, record*> elseSymbolTable;
    unordered_map<string, record*> savedSymbolTable;
    unordered_map<string, record*> afterSymbolTable;
    unordered_map<string, record*> savedFinalPassSymbolTable;
    unordered_map<string, set<string> > exprDataTable;
    unordered_map<string, bool> ifValueChanged;
    unordered_map<string, bool> elseValueChanged;
    /*The following are for code optimization purposes*/
    map<ll, set<pair<ll, pair<string, ll> > > > constantPropagation;
    map<pair<string, ll>, ll> cseIndex; // map string to integer index into cse.
    map<ll, multiset<ll> > cse;
    bool ifSimplified = false;
    bool ifSimplification = false;
    bool ifExists = false;
    map<ll, ll> strengthReduction;

    void printtree(struct node* n, ll level)
    {
        for(ll i = 0; i < level; i++)
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
%}

//Keyword Tokens
%token<n> BREAK CASE CONTINUE DEFAULT DO
%token<n> ELSE EXTERN FLOAT IF INT RETURN
%token<n> SIZEOF STRUCT SWITCH VOID WHILE

//Token for printf and format specifier
%token<n> PRINTF PRINT_FRMT_SPEC SCANF SCAN_FRMT_SPEC

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
%type<n> scan_stmt;
//%type<n> expr_stmt;
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
            max(($1)->lp, ($1)->depth + 1), 
            lineNumber
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
            max(($1)->lp, ($1)->depth + 1), 
            lineNumber
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
            max(max(($1)->lp, ($2)->lp), ($1)->depth + ($2)->depth + 1), 
            lineNumber
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
            max(($1)->lp, ($1)->depth + 1), 
            lineNumber
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
            max(($1)->lp, ($1)->depth + 1), 
            lineNumber
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
            max(($1)->lp, ($1)->depth + 1), 
            lineNumber
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
            1, 
            lineNumber
        };
        $1 = a; 
        struct node *b = new node;
        *b = (struct node)
        {
            "LEFTCURLY",
            1, 
            {},
            1,
            1, 
            lineNumber
        };
        $3 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "RIGHTCURLY",
            1, 
            {},
            1,
            1, 
            lineNumber
        };
        $5 = c;
        struct node *d = new node;
        *d = (struct node)
        {
            "SEMICOLON",
            1, 
            {},
            1,
            1, 
            lineNumber
        };
        $6 = d;
        struct node *e = new node;
        *e = (struct node)
        {
            "struct_decl",
            0, 
            {[0] = $1, [1] = $2, [2] = $3, [3] = $4, [4] = $5, [5] = $6},
            max(($2)->depth, ($4)->depth) + 1,
            max(max(($2)->lp, ($4)->lp), ($2)->depth + ($4)->depth + 1), 
            lineNumber
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
            1, 
            lineNumber
        };
        $3 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "var_decl",
            0, 
            {[0] = $1, [1] = $2, [2] = $3},
            max(($1)->depth, ($2)->depth) + 1,
            max(max(($1)->lp, ($2)->lp), ($1)->depth + ($2)->depth + 1), 
            lineNumber
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
            1, 
            lineNumber
        };
        $3 = b;
        struct node *a = new node;
        *a = (struct node)
        {
            "var_decl",
            0, 
            {[0] = $1, [1] = $2, [2] = $3, [3] = $4},
            max(($1)->depth, ($4)->depth) + 1,
            max(max(($1)->lp, ($4)->lp), ($1)->depth + ($4)->depth + 1), 
            lineNumber
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
            1, 
            lineNumber
        };
        $3 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "RIGHTBRACKET",
            1,
            {},
            1, 
            1, 
            lineNumber
        };
        $5 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "SEMICOLON",
            1,
            {},
            1, 
            1, 
            lineNumber
        };
        $6 = c;
        struct node *d = new node;
        *d = (struct node)
        {
            "var_decl",
            0,
            {[0] = $1, [1] = $2, [2] = $3, [3] = $4, [4] = $5, [5] = $6},
            max(($1)->depth, ($4)->depth) + 1,
            max(max(($1)->lp, ($4)->lp), ($1)->depth + ($4)->depth + 1), 
            lineNumber
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
            1, 
            lineNumber
        };
        $3 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "RIGHTBRACKET",
            1,
            {},
            1,
            1, 
            lineNumber
        };
        $5 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "COMMA",
            1,
            {},
            1,
            1, 
            lineNumber
        };
        $6 = c;
        struct node *d = new node;
        *d = (struct node)
        {
            "var_decl",
            0,
            {[0] = $1, [1] = $2, [2] = $3, [3] = $4, [4] = $5, [5] = $6, [6] = $7},
            max(($1)->depth, ($7)->depth) + 1,
            max(max(($1)->lp, ($7)->lp), ($1)->depth + ($7)->depth + 1), 
            lineNumber
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
            1, 
            lineNumber
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "type_spec",
            0,
            {[0] = $1, [1] = $2},
            3,
            4, 
            lineNumber
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
            1, 
            lineNumber
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "type_spec",
            0,
            {[0] = $1, [1] = $2},
            3,
            4, 
            lineNumber
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
            1, 
            lineNumber
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "type_spec",
            0,
            {[0] = $1, [1] = $2},
            3,
            4, 
            lineNumber
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
            1, 
            lineNumber
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "STAR",
            1,
            {},
            1,
            1, 
            lineNumber
        };
        $3 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "type_spec",
            0,
            {[0] = $1, [1] = $2, [2] = $3},
            3,
            4, 
            lineNumber
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
            1, 
            lineNumber
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "STAR",
            1,
            {},
            1,
            1, 
            lineNumber
        };
        $3 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "type_spec",
            0,
            {[0] = $1, [1] = $2, [2] = $3},
            3,
            4, 
            lineNumber
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
            1, 
            lineNumber
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "STAR",
            1,
            {},
            1,
            1, 
            lineNumber
        };
        $3 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "type_spec",
            0,
            {[0] = $1, [1] = $2, [2] = $3},
            3,
            4, 
            lineNumber
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
            1, 
            lineNumber
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "type_spec",
            0,
            {[0] = $1, [1] = $2},
            3,
            4, 
            lineNumber
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
            1, 
            lineNumber
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "STAR",
            1,
            {},
            1,
            1, 
            lineNumber
        };
        $3 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "type_spec",
            0,
            {[0] = $1, [1] = $2, [2] = $3},
            3,
            4, 
            lineNumber
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
            1, 
            lineNumber
        };
        struct node *b = new node;
        *b = (struct node)
        {
            "extern_spec",
            0,
            {[0] = a},
            2,
            2, 
            lineNumber
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
            1, 
            lineNumber
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "extern_spec",
            0,
            {[0] = $1},
            2,
            2, 
            lineNumber
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
            1, 
            lineNumber
        };
        $4 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "RIGHTBRACE",
            1, 
            {},
            1,
            1, 
            lineNumber
        };
        $6 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "func_decl",
            0, 
            {[0] = $1, [1] = $2, [2] = $4, [3] = $5, [4] = $6, [5] = $7},
            max(max(($1)->depth, ($5)->depth), ($7)->depth) + 1,
            max(max(max(max(max(($1)->lp, ($5)->lp), ($7)->lp), ($1)->depth + ($5)->depth + 1), ($5)->depth + ($7)->depth + 1), ($1)->depth + ($7)->depth + 1), 
            lineNumber
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
            1, 
            lineNumber
        };
        struct node *b = new node;
        *b = (struct node)
        {
            "params",
            0,
            {[0] = a},
            2,
            2, 
            lineNumber
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
            max(($1)->lp, ($1)->depth + 1), 
            lineNumber
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
            1, 
            lineNumber
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "param_list",
            0,
            {[0] = $1, [1] = $2, [2] = $3},
            max(($1)->depth, ($3)->depth) + 1, 
            max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1), 
            lineNumber
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
            max(($1)->depth + 1, ($1)->lp), 
            lineNumber
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
            max(max(($1)->lp, ($2)->lp), ($1)->depth + ($2)->depth + 1), 
            lineNumber
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
            1, 
            lineNumber
        };
        $3 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "RIGHTBRACKET",
            1,
            {},
            1, 
            1, 
            lineNumber
        };
        $4 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "param",
            0,
            {[0] = $1, [1] = $2, [2] = $3, [3] = $4}, 
            max(($1)->depth, ($2)->depth) + 1, 
            max(max(($1)->lp, ($2)->lp), ($1)->depth + ($2)->depth + 1), 
            lineNumber
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
            max(max(($1)->lp, ($2)->lp), ($1)->depth + ($2)->depth + 1), 
            lineNumber
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
            max(($1)->depth + 1, ($1)->lp), 
            lineNumber
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
            max(($1)->depth + 1, ($1)->lp), 
            lineNumber
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
            max(($1)->depth + 1, ($1)->lp), 
            lineNumber
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
            max(($1)->depth + 1, ($1)->lp), 
            lineNumber
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
            max(($1)->depth + 1, ($1)->lp), 
            lineNumber
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
            max(($1)->depth + 1, ($1)->lp), 
            lineNumber
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
            max(($1)->depth + 1, ($1)->lp), 
            lineNumber
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
            max(($1)->depth + 1, ($1)->lp), 
            lineNumber
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
            max(($1)->depth + 1, ($1)->lp), 
            lineNumber
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
            max(($1)->depth + 1, ($1)->lp), 
            lineNumber
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
            max(($1)->depth + 1, ($1)->lp), 
            lineNumber
        };
        $$ = a;
    }
    | scan_stmt
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "stmt", 
            0, 
            {[0] = $1},
            ($1)->depth + 1, 
            max(($1)->depth + 1, ($1)->lp), 
            lineNumber
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
            max(($1)->depth + 1, ($1)->lp), 
            lineNumber
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
            max(($1)->depth + 1, ($1)->lp), 
            lineNumber
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
            1, 
            lineNumber
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "LEFTBRACE",
            1, 
            {},
            1, 
            1, 
            lineNumber
        };
        $2 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "RIGHTBRACE",
            1, 
            {},
            1, 
            1, 
            lineNumber
        };
        $4 = c;
        struct node *d = new node;
        *d = (struct node)
        {
            "while_stmt",
            0,
            {[0] = $1, [1] = $2, [2] = $3, [3] = $4, [4] = $5},
            max(($3)->depth, ($5)->depth) + 1,
            max(max(($3)->lp, ($5)->lp), ($3)->depth + ($5)->depth + 1), 
            lineNumber
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
            1, 
            lineNumber
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "WHILE",
            1,
            {},
            1, 
            1, 
            lineNumber
        };
        $3 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "LEFTBRACE",
            1,
            {},
            1, 
            1, 
            lineNumber
        };
        $4 = c;
        struct node *d = new node;
        *d = (struct node)
        {
            "RIGHTBRACE",
            1,
            {},
            1, 
            1, 
            lineNumber
        };
        $6 = d;
        struct node *e = new node;
        *e = (struct node)
        {
            "SEMICOLON",
            1,
            {},
            1, 
            1, 
            lineNumber
        };
        $7 = e;
        struct node *f = new node;
        *f = (struct node)
        {
            "dowhile_stmt",
            0,
            {[0] = $1, [1] = $2, [2] = $3, [3] = $4, [4] = $5, [5] = $6, [6] = $7},
            max(($2)->depth, ($5)->depth) + 1, 
            max(max(($2)->lp, ($5)->lp), ($2)->depth + ($5)->depth + 1), 
            lineNumber
        };
        $$ = f;
    };

print_stmt
    : PRINTF LEFTBRACE PRINT_FRMT_SPEC COMMA identifier RIGHTBRACE SEMICOLON
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "PRINTF",
            1,
            {},
            1,
            1, 
            lineNumber
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "LEFTBRACE",
            1,
            {},
            1,
            1, 
            lineNumber
        };
        $2 = b;
        struct node *g = new node;
        *g = (struct node)
        {
            "PRINT_FRMT_SPEC",
            1,
            {},
            1,
            1, 
            lineNumber
        };
        $3 = g;
        struct node *c = new node;
        *c = (struct node)
        {
            "COMMA",
            1,
            {},
            1,
            1, 
            lineNumber
        };
        $4 = c;
        struct node *d = new node;
        *d = (struct node)
        {
            "RIGHTBRACE",
            1,
            {},
            1,
            1, 
            lineNumber
        };
        $6 = d;
        struct node *e = new node;
        *e = (struct node)
        {
            "SEMICOLON",
            1,
            {},
            1,
            1, 
            lineNumber
        };
        $7 = e;
        struct node *f = new node;
        *f = (struct node)
        {
            "print_stmt",
            0,
            {[0] = $1, [1] = $2, [2] = $3, [3] = $4, [4] = $5, [5] = $6, [6] = $7},
            max(($3)->depth, ($5)->depth) + 1,
            max(max(($3)->lp, ($5)->lp), ($3)->depth + ($5)->depth + 1), 
            lineNumber
        };
        $$ = f;
    };

scan_stmt
    : SCANF LEFTBRACE SCAN_FRMT_SPEC COMMA AND identifier RIGHTBRACE SEMICOLON
    {
        struct node *a = new node;
        *a = (struct node)
        {
            "SCANF",
            1,
            {},
            1,
            1, 
            lineNumber
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "LEFTBRACE",
            1,
            {},
            1,
            1, 
            lineNumber
        };
        $2 = b;
        struct node *g = new node;
        *g = (struct node)
        {
            "SCAN_FRMT_SPEC",
            1,
            {},
            1,
            1, 
            lineNumber
        };
        $3 = g;
        struct node *c = new node;
        *c = (struct node)
        {
            "COMMA",
            1,
            {},
            1,
            1, 
            lineNumber
        };
        $4 = c;
        struct node *h = new node;
        *h = (struct node)
        {
            "AND",
            1,
            {},
            1,
            1, 
            lineNumber
        };
        $5 = h;
        struct node *d = new node;
        *d = (struct node)
        {
            "RIGHTBRACE",
            1,
            {},
            1,
            1, 
            lineNumber
        };
        $7 = d;
        struct node *e = new node;
        *e = (struct node)
        {
            "SEMICOLON",
            1,
            {},
            1,
            1, 
            lineNumber
        };
        $8 = e;
        struct node *f = new node;
        *f = (struct node)
        {
            "scan_stmt",
            0,
            {[0] = $1, [1] = $2, [2] = $3, [3] = $4, [4] = $5, [5] = $6, [6] = $7, [7] = $8},
            max(($3)->depth, ($5)->depth) + 1,
            max(max(($3)->lp, ($5)->lp), ($3)->depth + ($5)->depth + 1), 
            lineNumber
        };
        $$ = f;
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
            1, 
            lineNumber
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "RIGHTCURLY",
            1,
            {},
            1,
            1, 
            lineNumber
        };
        $4 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "compound_stmt",
            0,
            {[0] = $1, [1] = $2, [2] = $3, [3] = $4},
            max(($2)->depth, ($3)->depth) + 1, 
            max(max(($2)->lp, ($3)->lp), ($2)->depth + ($3)->depth + 1), 
            lineNumber
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
            1, 
            lineNumber
        };
        struct node *b = new node;
        *b = (struct node)
        {
            "local_decls",
            0, 
            {[0] = a},
            2,
            2, 
            lineNumber
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
            max(max(($1)->lp, ($2)->lp), ($1)->depth + ($2)->depth + 1), 
            lineNumber
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
            1, 
            lineNumber
        };
        $3 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "local_decl",
            0,
            {[0] = $1, [1] = $2, [2] = $3},
            max(($1)->depth, ($2)->depth) + 1,
            max(max(($1)->lp, ($2)->lp), ($1)->depth + ($2)->depth + 1), 
            lineNumber
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
            1, 
            lineNumber
        };
        $3 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "RIGHTBRACKET",
            1,
            {},
            1, 
            1, 
            lineNumber
        };
        $5 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "SEMICOLON",
            1,
            {},
            1, 
            1, 
            lineNumber
        };
        $6 = c;
        struct node *d = new node;
        *d = (struct node)
        {
            "local_decl",
            0,
            {[0] = $1, [1] = $2, [2] = $3, [3] = $4, [4] = $5, [5] = $6},
            max(($1)->depth, ($4)->depth) + 1, 
            max(max(($1)->lp, ($4)->lp), ($1)->depth + ($4)->depth + 1), 
            lineNumber
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
            1, 
            lineNumber
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "LEFTBRACE",
            1,
            {},
            1, 
            1, 
            lineNumber
        };
        $2 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "RIGHTBRACE",
            1,
            {},
            1, 
            1, 
            lineNumber
        };
        $4 = c;
        struct node *d = new node;
        *d = (struct node)
        {
            "if_stmt",
            0,
            {[0] = $1, [1] = $2, [2] = $3, [3] = $4, [4] = $5},
            max(($3)->depth, ($5)->depth) + 1, 
            max(max(($3)->lp, ($5)->lp), ($3)->depth + ($5)->depth + 1), 
            lineNumber
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
            1, 
            lineNumber
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "LEFTBRACE",
            1,
            {},
            1, 
            1, 
            lineNumber
        };
        $2 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "RIGHTBRACE",
            1,
            {},
            1, 
            1, 
            lineNumber
        };
        $4 = c;
        struct node *d = new node;
        *d = (struct node)
        {
            "ELSE",
            1,
            {},
            1, 
            1, 
            lineNumber
        };
        $6 = d;
        struct node *e = new node;
        *e = (struct node)
        {
            "if_stmt",
            0,
            {[0] = $1, [1] = $2, [2] = $3, [3] = $4, [4] = $5, [5] = $6, [6] = $7},
            max(max(($3)->depth, ($5)->depth), ($7)->depth) + 1,
            max(max(max(max(max(($3)->lp, ($5)->lp), ($7)->lp), ($3)->depth + ($5)->depth + 1), ($5)->depth + ($7)->depth + 1), ($3)->depth + ($7)->depth + 1), 
            lineNumber
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
            1, 
            lineNumber
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "SEMICOLON",
            1,
            {},
            1, 
            1, 
            lineNumber
        };
        $2 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "return_stmt",
            0,
            {[0] = $1, [1] = $2},
            2, 
            3, 
            lineNumber
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
            1, 
            lineNumber
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "SEMICOLON",
            1,
            {},
            1, 
            1, 
            lineNumber
        };
        $3 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "return_stmt",
            0,
            {[0] = $1, [1] = $2, [2] = $3},
            ($2)->depth + 1, 
            max(($2)->depth + 2, ($2)->lp), 
            lineNumber
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
            1, 
            lineNumber
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "SEMICOLON",
            1,
            {},
            1, 
            1, 
            lineNumber
        };
        $2 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "break_stmt",
            0,
            {[0] = $1, [1] = $2},
            2, 
            3, 
            lineNumber
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
            1, 
            lineNumber
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "SEMICOLON",
            1,
            {},
            1, 
            1, 
            lineNumber
        };
        $2 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "continue_stmt",
            0,
            {[0] = $1, [1] = $2},
            2, 
            3, 
            lineNumber
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
            1, 
            lineNumber
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "LEFTBRACE",
            1,
            {},
            1, 
            1, 
            lineNumber
        };
        $2 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "RIGHTBRACE",
            1,
            {},
            1, 
            1, 
            lineNumber
        };
        $4 = c;
        struct node *d = new node;
        *d = (struct node)
        {
            "LEFTCURLY",
            1,
            {},
            1, 
            1, 
            lineNumber
        };
        $5 = d;
        struct node *e = new node;
        *e = (struct node)
        {
            "RIGHTCURLY",
            1, 
            {}, 
            .depth  = 1, 
            1, 
            lineNumber
        };
        $8 = e;
        struct node *f = new node;
        *f = (struct node)
        {
            "switch_stmt",
            0,
            {[0] = $1, [1] = $2, [2] = $3, [3] = $4, [4] = $5, [5] = $6, [6] = $7, [7] = $8},
            max(max(($3)->depth, ($6)->depth), ($7)->depth) + 1, 
            max(max(max(max(max(($3)->lp, ($6)->lp), ($7)->lp), ($3)->depth + ($6)->depth + 1), ($6)->depth + ($7)->depth + 1), ($3)->depth + ($7)->depth + 1), 
            lineNumber
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
            max(max(($1)->lp, ($2)->lp), ($1)->depth + ($2)->depth + 1), 
            lineNumber
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
            max(($1)->depth + 1, ($1)->lp), 
            lineNumber
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
            1, 
            lineNumber
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "COLON",
            1,
            {},
            1,
            1, 
            lineNumber
        };
        $3 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "single_case",
            0,
            {[0] = $1, [1] = $2, [2] = $3, [3] = $4},
            max(($2)->depth, ($4)->depth) + 1,
            max(max(($2)->lp, ($4)->lp), ($2)->depth + ($4)->depth + 1), 
            lineNumber
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
            1, 
            lineNumber
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "COLON",
            1,
            {},
            1,
            1, 
            lineNumber
        };
        $2 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "default_case",
            0,
            {[0] = $1, [1] = $2, [2] = $3},
            ($3)->depth + 1,
            max(($3)->depth + 2, ($3)->lp), 
            lineNumber
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
            1, 
            lineNumber
        };
        $4 = b;
        struct node *a = new node;
        *a = (struct node)
        {
            "EQUAL",
            0,
            {[0] = $1, [1] = $3},
            max(($1)->depth, ($3)->depth) + 1,
            max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1), 
            lineNumber
        };
        $2 = a;
        struct node *c = new node;
        *c = (struct node)
        {
            "assign_stmt", 
            0, 
            {[0] = $2, [1] = $4},
            max(($2)->depth, ($4)->depth) + 1,
            max(($2)->lp, ($2)->depth + 2), 
            lineNumber
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
            1, 
            lineNumber
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "RIGHTBRACKET",
            1,
            {},
            1,
            1, 
            lineNumber
        };
        $4 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "SEMICOLON",
            1,
            {},
            1,
            1, 
            lineNumber
        };
        $7 = c;
        struct node *e = new node;
        *e = (struct node)
        {
            "EQUAL", 
            0, 
            {[0] = $1, [1] = $2, [2] = $3, [3] = $4, [4] = $6},
            max(max(($6)->depth, ($3)->depth), ($1)->depth) + 1,
            max(max(max(max(max(($6)->lp, ($3)->lp), ($1)->lp), ($3)->depth + ($6)->depth + 1), ($1)->depth + ($3)->depth + 1), ($1)->depth + ($6)->depth + 1), 
            lineNumber
        };
        $5 = e;
        struct node *d = new node;
        *d = (struct node)
        {
            "assign_stmt",
            0,
            {[0] = $5, [1] = $7},
            max(($5)->depth, ($7)->depth) + 1, 
            max(max(($5)->lp, ($7)->lp), ($5)->depth + ($7)->depth + 1), 
            lineNumber
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
            1, 
            lineNumber
        };
        $2 = a;
        struct node *c = new node;
        *c = (struct node)
        {
            "SEMICOLON", 
            1, 
            {}, 
            1, 
            1, 
            lineNumber
        };
        $6 = c;
        struct node *b = new node;
        *b = (struct node)
        {
            "EQUAL",
            0,
            {[0] = $1, [1] = $2, [2] = $3, [3] = $5},
            max(($1)->depth, ($5)->depth) + 1,
            max(max(($1)->lp, ($5)->lp), ($1)->depth + ($5)->depth + 1), 
            lineNumber
        };
        $4 = b;
        struct node *d = new node;
        *d = (struct node)
        {
            "assign_stmt",
            0, 
            {[0] = $4, [1] = $6}, 
            max(($4)->depth, ($6)->depth) + 1, 
            max(max(($4)->lp, ($6)->lp), ($4)->depth + ($6)->depth + 1), 
            lineNumber
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
            1, 
            lineNumber
        };
        $2 = a;
        struct node *c = new node;
        *c = (struct node)
        {
            "SEMICOLON", 
            1, 
            {}, 
            1, 
            1, 
            lineNumber
        };
        $6 = c;
        struct node *b = new node;
        *b = (struct node)
        {
            "EQUAL",
            0,
            {[0] = $1, [1] = $2, [2] = $3, [3] = $5},
            max(($1)->depth, ($5)->depth) + 1,
            max(max(($1)->lp, ($5)->lp), ($1)->depth + ($5)->depth + 1), 
            lineNumber
        };
        $4 = b;
        struct node *d = new node;
        *d = (struct node)
        {
            "assign_stmt",
            0, 
            {[0] = $4, [1] = $6}, 
            max(($4)->depth, ($6)->depth) + 1, 
            max(max(($4)->lp, ($6)->lp), ($4)->depth + ($6)->depth + 1), 
            lineNumber
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
            1, 
            lineNumber
        };
        $3 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "INCR", 
            1, 
            {}, 
            1,
            1, 
            lineNumber
        };
        $2 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "incr_stmt", 
            0, 
            {[0] = $1, [1] = $2, [2] = $3}, 
            max(($1)->depth, ($2)->depth) + 1,
            max(max(($1)->depth + 1 + ($2)->depth, ($2)->lp), ($1)->lp), 
            lineNumber
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
            1, 
            lineNumber
        };
        $3 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "DECR", 
            0, 
            {}, 
            1,
            1, 
            lineNumber
        };
        $2 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "decr_stmt", 
            0, 
            {[0] = $1, [1] = $2, [2] = $3}, 
            max(($1)->depth, ($2)->depth) + 1,
            max(max(($1)->depth + 1 + ($2)->depth, ($1)->lp), ($2)->lp), 
            lineNumber
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
            max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1), 
            lineNumber,
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr", 
            0, 
            {[0] = $2}, 
            ($2)->depth + 1, 
            max(($2)->depth + 1, ($2)->lp), 
            lineNumber
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
            max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1), 
            lineNumber
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr", 
            0, 
            {[0] = $2}, 
            ($2)->depth + 1, 
            max(($2)->depth + 1, ($2)->lp), 
            lineNumber
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
            max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1), 
            lineNumber
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr", 
            0, 
            {[0] = $2}, 
            ($2)->depth + 1, 
            max(($2)->depth + 1, ($2)->lp), 
            lineNumber
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
            max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1), 
            lineNumber
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr", 
            0, 
            {[0] = $2}, 
            ($2)->depth + 1, 
            max(($2)->depth + 1, ($2)->lp), 
            lineNumber
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
            max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1), 
            lineNumber
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr", 
            0, 
            {[0] = $2}, 
            ($2)->depth + 1, 
            max(($2)->depth + 1, ($2)->lp), 
            lineNumber
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
            max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1), 
            lineNumber
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr", 
            0, 
            {[0] = $2}, 
            ($2)->depth + 1, 
            max(($2)->depth + 1, ($2)->lp), 
            lineNumber
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
            1, 
            lineNumber
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "LEFTBRACE", 
            1, 
            {}, 
            1,
            1, 
            lineNumber
        };
        $2 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "RIGHTBRACE", 
            1, 
            {}, 
            1,
            1, 
            lineNumber
        };
        $4 = c;
        struct node *d = new node;
        *d = (struct node)
        {
            "expr", 
            0, 
            {[0] = $1, [1] = $2, [2] = $3, [3] = $4},
            ($3)->depth + 1,
            max(($3)->depth + 2, ($3)->lp), 
            lineNumber
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
            max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1), 
            lineNumber
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr", 
            0, 
            {[0] = $2}, 
            ($2)->depth + 1, 
            max(($2)->depth + 1, ($2)->lp), 
            lineNumber
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
            max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1), 
            lineNumber
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr", 
            0, 
            {[0] = $2}, 
            ($2)->depth + 1, 
            max(($2)->depth + 1, ($2)->lp), 
            lineNumber
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
            max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1), 
            lineNumber
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr", 
            0, 
            {[0] = $2}, 
            ($2)->depth + 1, 
            max(($2)->depth + 1, ($2)->lp), 
            lineNumber
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
            max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1), 
            lineNumber
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr", 
            0, 
            {[0] = $2}, 
            ($2)->depth + 1, 
            max(($2)->depth + 1, ($2)->lp), 
            lineNumber
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
            max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1), 
            lineNumber
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr", 
            0, 
            {[0] = $2}, 
            ($2)->depth + 1, 
            max(($2)->depth + 1, ($2)->lp), 
            lineNumber
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
            max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1), 
            lineNumber
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr", 
            0, 
            {[0] = $2}, 
            ($2)->depth + 1, 
            max(($2)->depth + 1, ($2)->lp), 
            lineNumber
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
            max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1), 
            lineNumber
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr", 
            0, 
            {[0] = $2}, 
            ($2)->depth + 1, 
            max(($2)->depth + 1, ($2)->lp), 
            lineNumber
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
            max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1), 
            lineNumber
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr", 
            0, 
            {[0] = $2}, 
            ($2)->depth + 1, 
            max(($2)->depth + 1, ($2)->lp), 
            lineNumber
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
            max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1), 
            lineNumber
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr", 
            0, 
            {[0] = $2}, 
            ($2)->depth + 1, 
            max(($2)->depth + 1, ($2)->lp), 
            lineNumber
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
            1, 
            lineNumber
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr", 
            0, 
            {[0] = $1, [1] = $2}, 
            ($2)->depth + 1,
            max(($2)->depth + 2, ($2)->lp), 
            lineNumber
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
            1, 
            lineNumber
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr", 
            0, 
            {[0] = $1, [1] = $2}, 
            ($2)->depth + 1,
            max(($2)->depth + 2, ($2)->lp), 
            lineNumber
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
            1, 
            lineNumber
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr", 
            0, 
            {[0] = $1, [1] = $2}, 
            ($2)->depth + 1,
            max(($2)->depth + 2, ($2)->lp), 
            lineNumber
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
            1, 
            lineNumber
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr", 
            0, 
            {[0] = $1, [1] = $2}, 
            ($2)->depth + 1,
            max(($2)->depth + 2, ($2)->lp), 
            lineNumber
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
            1, 
            lineNumber
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "expr", 
            0, 
            {[0] = $1, [1] = $2}, 
            ($2)->depth + 1,
            max(($2)->depth + 2, ($2)->lp), 
            lineNumber
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
            max(($1)->depth + 1, ($1)->lp), 
            lineNumber
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
            1, 
            lineNumber
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "RIGHTBRACE", 
            1, 
            {}, 
            1, 
            1, 
            lineNumber
        };
        $4 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "expr", 
            0, 
            {[0] = $1, [1] = $2, [2] = $3, [3] = $4}, 
            max(($1)->depth, ($3)->depth) + 1,
            max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1), 
            lineNumber
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
            1, 
            lineNumber
        };
        $2 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "RIGHTBRACKET", 
            1, 
            {}, 
            1, 
            1, 
            lineNumber
        };
        $4 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "expr", 
            0, 
            {[0] = $1, [1] = $2, [2] = $3, [3] = $4}, 
            max(($1)->depth, ($3)->depth) + 1,
            max(max(($1)->lp, ($3)->lp), ($1)->depth + ($3)->depth + 1), 
            lineNumber
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
            max(($1)->depth + 1, ($1)->lp), 
            lineNumber
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
            max(($1)->depth + 1, ($1)->lp), 
            lineNumber
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
            max(($1)->depth + 1, ($1)->lp), 
            lineNumber
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
            1, 
            lineNumber
        };
        $1 = a;
        struct node *b = new node;
        *b = (struct node)
        {
            "RIGHTBRACE", 
            1, 
            {}, 
            1, 
            1, 
            lineNumber
        };
        $3 = b;
        struct node *c = new node;
        *c = (struct node)
        {
            "Pexpr", 
            0, 
            {[0] = $1, [1] = $2, [2] = $3}, 
            ($2)->depth + 1, 
            max(($2)->depth + 2, ($2)->lp), 
            lineNumber
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
            1, 
            lineNumber
        };
        $2 = a;
        struct node *c = new node;
        *c = (struct node)
        {
            "arg_list", 
            0, 
            {[0] = $1, [1] = $2, [2] = $3}, 
            max(($1)->depth, ($3)->depth) + 1, 
            max(max(($1)->depth + 1 + ($3)->depth, ($1)->lp), ($3)->lp), 
            lineNumber
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
            max(($1)->depth + 1, ($1)->lp), 
            lineNumber
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
            1, 
            lineNumber
        };
        struct node *b = new node;
        *b = (struct node)
        {
            "args",
            0,
            {[0] = a},
            2,
            2, 
            lineNumber
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
            max(($1)->depth + 1, ($1)->lp), 
            lineNumber
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
            1, 
            lineNumber
        };
        $1 = b;
        struct node *a = new node;
        *a = (struct node)
        {
            "integerLit",
            0, 
            {[0] = $1}, 
            2, 
            2, 
            lineNumber
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
            1, 
            lineNumber
        };
        $1 = b;
        struct node *a = new node;
        *a = (struct node)
        {
            "floatLit",
            0, 
            {[0] = $1}, 
            2, 
            2, 
            lineNumber
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
            1, 
            lineNumber
        };
        $1 = b;
        struct node *a = new node;
        *a = (struct node)
        {
            "identifier",
            0, 
            {[0] = $1}, 
            2, 
            2, 
            lineNumber
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

void initializeRecord(string identifier)
{
    record* newRecord = new record;
    *newRecord = (record)
    { 
        identifier, 
        -1,
        0,
        0, 
        -1
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

void initializeRecordFirstPass(string identifier)
{
    record* newRecord = new record;
    *newRecord = (record)
    { 
        identifier, // name
        -1, // stackOffset
        0, // used
        0, // valueKnown
        -1, // value
    };
    firstPassSymbolTable[identifier] = newRecord;
}

/*
First pass of the optimization is done over the AST. 
Does constant propagation, constant folding at the AST level. 
*/

void copysymtab(unordered_map<string, record*>& old_map, unordered_map<string, record*>& new_map)
{
    new_map.clear();
    for(auto it : old_map)
    {
        record *r = new record;
        *r = *(it.second);
        new_map[it.first] = r;
    }
}

void firstPass(struct node* n)
{
    static int declarationNumber;
    static bool inIf = 0;
    if(n->label == "local_decl")
    {
        if(n->children[2]->label == "SEMICOLON")
        {
            string id = n->children[1]->children[0]->label;
            initializeRecordFirstPass(id);
            firstPassSymbolTable[id]->declarationNumber = declarationNumber; // extra bookkeeping for summary-text output
            declarationNumber++;
        }
    }
    else if(n->label == "assign_stmt")
    {
        set<pair<ll, pair<string, ll> > > v;
        constantPropagation[n->lineNumber] = v;
        string id = n->children[0]->children[0]->children[0]->label;
        firstPassSymbolTable[id]->used = 1; 
        firstPass(n->children[0]->children[1]);
        n->children[0]->children[0]->valueKnown = n->children[0]->children[1]->valueKnown;
        n->children[0]->children[0]->value = n->children[0]->children[1]->value;
        firstPassSymbolTable[id]->value = n->children[0]->children[0]->value;
        firstPassSymbolTable[id]->valueKnown = n->children[0]->children[0]->valueKnown;
        if(n->children[0]->children[1]->consFoldOccured == 1)
        {
            pair<ll, ll> p;
            p.first = n->lineNumber;
            p.second = n->children[0]->children[1]->maximalConsFold;
            consFold.push_back(p);
        }
    }
    else if(n->label == "scan_stmt")
    {
        string id = n->children[5]->children[0]->label;
        firstPassSymbolTable[id]->used = 1;
        firstPassSymbolTable[id]->valueKnown = 0;
    }
    else if(n->label == "print_stmt")
    {
        string id = n->children[4]->children[0]->label;
        n->children[4]->children[0]->valueKnown = firstPassSymbolTable[id]->valueKnown;
        n->children[4]->children[0]->value = firstPassSymbolTable[id]->value;
        if(firstPassSymbolTable[id]->valueKnown == 1)
        {
            pair<string, ll> p;
            p.first = id;
            p.second = n->children[4]->children[0]->value;
            pair<ll, pair<string, ll> > q;
            q.first = firstPassSymbolTable[id]->declarationNumber;
            q.second = p;
            constantPropagation[n->children[4]->children[0]->lineNumber].insert(q);
        }
    }
    else if(n->label == "if_stmt")
    {
        ifExists = 1;
        inIf = 1;
        firstPass(n->children[2]);
        inIf = 0;
        if(n->children[2]->valueKnown != 1 && n->children[2]->consFoldOccured == 1)
        {
            pair<ll, ll> p;
            p.first = n->children[2]->lineNumber;
            p.second = n->children[2]->maximalConsFold;
            consFold.push_back(p);
        }
        n->valueKnown = n->children[2]->valueKnown;
        n->value = n->children[2]->value;
        if(n->valueKnown)
        {
            ifSimplified = 1;
            ifSimplification = (n->value == 0);
        }
        copysymtab(firstPassSymbolTable, savedSymbolTable); // contains info prior to if-else
        if(n->children[5] == NULL)
        {
            if(n->valueKnown != 1 || (n->valueKnown == 1 && n->value != 0))
            {
                firstPass(n->children[4]);
                copysymtab(firstPassSymbolTable, ifSymbolTable);
            }
            if(n->valueKnown != 1)
            {
                firstPassSymbolTable.clear();
                for(auto element1 : ifSymbolTable)
                {
                    record* newRecord = new record;
                    *newRecord = *element1.second;
                    firstPassSymbolTable[element1.first] = newRecord;
                    if(!(savedSymbolTable[element1.first]->valueKnown && element1.second->valueKnown && savedSymbolTable[element1.first]->value == element1.second->value))
                    {
                        firstPassSymbolTable[element1.first]->valueKnown = 0;
                        firstPassSymbolTable[element1.first]->value = 0;
                    }
                }
                copysymtab(firstPassSymbolTable, afterSymbolTable);
            }
        }
        else
        {
            if(n->valueKnown != 1 || (n->valueKnown == 1 && n->value != 0))
            {
                copysymtab(savedSymbolTable, firstPassSymbolTable);
                firstPass(n->children[4]);
                copysymtab(firstPassSymbolTable, ifSymbolTable);
            }
            if(n->valueKnown != 1 || (n->valueKnown == 1 && n->value == 0))
            {
                copysymtab(savedSymbolTable, firstPassSymbolTable);
                firstPass(n->children[6]);
                copysymtab(firstPassSymbolTable, elseSymbolTable);
            }
            if(n->valueKnown != 1)
            {
                firstPassSymbolTable.clear();
                for(auto element1 : ifSymbolTable)
                {
                    record* newRecord = new record;
                    *newRecord = *element1.second;
                    firstPassSymbolTable[element1.first] = newRecord;
                    if(!(elseSymbolTable[element1.first]->valueKnown && element1.second->valueKnown && elseSymbolTable[element1.first]->value == element1.second->value))
                    {
                        firstPassSymbolTable[element1.first]->valueKnown = 0;
                        firstPassSymbolTable[element1.first]->value = 0;
                    }
                    firstPassSymbolTable[element1.first]->used = element1.second->used || elseSymbolTable[element1.first]->used;
                }
                copysymtab(firstPassSymbolTable, afterSymbolTable);
            }
        }
    }
    else if(n->label == "Pexpr")
    {
        if(n->children[0]->label == "identifier")
        {
            string id = n->children[0]->children[0]->label;
            n->children[0]->children[0]->valueKnown = firstPassSymbolTable[id]->valueKnown;
            n->children[0]->children[0]->value = firstPassSymbolTable[id]->value;
            n->valueKnown = n->children[0]->children[0]->valueKnown;
            n->value = n->children[0]->children[0]->value;
            if(n->valueKnown == 1 && !(inIf))
            {
                pair<string, ll> p;
                p.first = id;
                p.second = n->children[0]->children[0]->value;
                pair<ll, pair<string, ll> > q;
                q.first = firstPassSymbolTable[id]->declarationNumber;
                q.second = p;
                constantPropagation[n->children[0]->children[0]->lineNumber].insert(q);
            }
        }
        else if(n->children[0]->label == "integerLit")
        {
            n->valueKnown = 1;
            n->value = stoi(n->children[0]->children[0]->label);
        }
        else if(n->children[0]->label == "LEFTBRACE")
        {
            firstPass(n->children[1]);
            n->value = n->children[1]->value;
            n->valueKnown = n->children[1]->valueKnown;
            n->consFoldOccured = n->children[1]->consFoldOccured;
            n->maximalConsFold = n->children[1]->maximalConsFold;
        }
    }
    else if(n->label == "expr")
    {
        if(n->children[0]->label == "Pexpr" && n->children[1] == NULL)
        {
            firstPass(n->children[0]);
            n->value = n->children[0]->value;
            n->valueKnown = n->children[0]->valueKnown;
            n->consFoldOccured = n->children[0]->consFoldOccured;
            n->maximalConsFold = n->children[0]->maximalConsFold;
        }
        else if(n->children[1] != NULL && n->children[1]->label == "Pexpr")
        {
            firstPass(n->children[1]);
            if(n->children[1]->valueKnown == 1)
            {
                n->valueKnown = 1;
                if(n->children[0]->label == "NOT")
                {
                    n->value = !(n->children[1]->value);
                }
                else if(n->children[0]->label == "MINUS")
                {
                    n->value = -(n->children[1]->value);
                }
                else if(n->children[0]->label == "PLUS")
                {
                    n->value = n->children[1]->value;
                }
            }
            if(n->valueKnown)
            {
                n->consFoldOccured = 1;
                n->maximalConsFold = n->value;
            }
            else
            {
                n->consFoldOccured = n->children[1]->consFoldOccured;
                n->maximalConsFold = n->children[1]->maximalConsFold;
            }
        }
        else 
        {
            if(n->children[0]->label == "LOGOR" || n->children[0]->label == "LOGAND")
            {
                firstPass(n->children[0]->children[0]);
                firstPass(n->children[0]->children[1]);
                bool leftValueKnown = n->children[0]->children[0]->valueKnown;
                bool rightValueKnown = n->children[0]->children[1]->valueKnown;
                if(n->children[0]->label == "LOGOR")
                {
                    if(leftValueKnown)
                    {
                        int leftValue = n->children[0]->children[0]->value;
                        if(leftValue != 0)
                        {
                            n->valueKnown = 1;
                            n->value = 1;
                        }
                        else 
                        {
                            if(rightValueKnown)
                            {
                                int rightValue = n->children[0]->children[1]->value;
                                if(rightValue != 0)
                                {
                                    n->valueKnown = 1;
                                    n->value = 1;
                                }
                                else 
                                {
                                    n->valueKnown = 1;
                                    n->value = 0;
                                }
                            }
                        }
                    }
                    else
                    {
                        if(rightValueKnown)
                        {
                            int rightValue = n->children[0]->children[1]->value;
                            if(rightValue != 0)
                            {
                                n->valueKnown = 1;
                                n->value = 1;
                            }
                        }
                    }
                }
                else
                {
                    if(leftValueKnown)
                    {
                        int leftValue = n->children[0]->children[0]->value;
                        if(leftValue == 0)
                        {
                            n->valueKnown = 1;
                            n->value = 0;
                        }
                        else 
                        {
                            if(rightValueKnown)
                            {
                                int rightValue = n->children[0]->children[1]->value;
                                if(rightValue == 0)
                                {
                                    n->valueKnown = 1;
                                    n->value = 0;
                                }
                                else 
                                {
                                    n->valueKnown = 1;
                                    n->value = 1;
                                }
                            }
                        }
                    }
                    else
                    {
                        if(rightValueKnown)
                        {
                            int rightValue = n->children[0]->children[1]->value;
                            if(rightValue == 0)
                            {
                                n->valueKnown = 1;
                                n->value = 0;
                            }
                        }
                    }
                }
                if(n->valueKnown)
                {
                    n->consFoldOccured = 1;
                    n->maximalConsFold = n->value;
                }
                else
                {
                    n->consFoldOccured = n->children[0]->children[1]->consFoldOccured || n->children[0]->children[0]->consFoldOccured;
                    if(n->children[0]->children[0]->consFoldOccured)
                    {
                        if(n->children[0]->children[1]->consFoldOccured)
                        {
                            n->maximalConsFold = max(n->children[0]->children[0]->maximalConsFold, n->children[0]->children[1]->maximalConsFold);
                        }
                        else 
                        {
                            n->maximalConsFold = n->children[0]->children[0]->maximalConsFold;
                        }
                    }
                    else
                    {
                        if(n->children[0]->children[1]->consFoldOccured)
                        {
                            n->maximalConsFold = n->children[0]->children[1]->maximalConsFold;
                        }
                    }
                }
            }
            else 
            {
                firstPass(n->children[0]->children[0]);
                firstPass(n->children[0]->children[1]);
                bool leftValueKnown = n->children[0]->children[0]->valueKnown;
                bool rightValueKnown = n->children[0]->children[1]->valueKnown;
                if(leftValueKnown == 1 && rightValueKnown == 1)
                {
                    int leftValue = n->children[0]->children[0]->value;
                    int rightValue = n->children[0]->children[1]->value;
                    n->valueKnown = 1;
                    if(n->children[0]->label == "LT")
                    {
                        n->value = leftValue < rightValue;
                    }
                    else if(n->children[0]->label == "GT")
                    {
                        n->value = leftValue > rightValue;
                    }
                    else if(n->children[0]->label == "LEQ")
                    {
                        n->value = leftValue <= rightValue;
                    }
                    else if(n->children[0]->label == "GEQ")
                    {
                        n->value = leftValue >= rightValue;
                    }
                    else if(n->children[0]->label == "ISEQ")
                    {
                        n->value = leftValue == rightValue;
                    }
                    else if(n->children[0]->label == "NEQ")
                    {
                        n->value = leftValue != rightValue;
                    }
                    else if(n->children[0]->label == "THREEWAY")
                    {
                        if(leftValue < rightValue)
                        {
                            n->value = -1;
                        }
                        else if(leftValue > rightValue)
                        {
                            n->value = 1;
                        }
                        else 
                        {
                            n->value = 0;
                        }
                    }
                    else if(n->children[0]->label == "PLUS")
                    {
                        n->value = leftValue + rightValue;
                    }
                    else if(n->children[0]->label == "MINUS")
                    {
                        n->value = leftValue - rightValue;
                    }
                    else if(n->children[0]->label == "STAR")
                    {
                        n->value = leftValue * rightValue;
                    }
                    else if(n->children[0]->label == "DIV")
                    {
                        n->value = leftValue / rightValue;
                    }
                    else if(n->children[0]->label == "MOD")
                    {
                        n->value = leftValue % rightValue;
                    }
                }
                if(n->valueKnown)
                {
                    n->consFoldOccured = 1;
                    n->maximalConsFold = n->value;
                }
                else
                {
                    n->consFoldOccured = n->children[0]->children[1]->consFoldOccured || n->children[0]->children[0]->consFoldOccured;
                    if(n->children[0]->children[0]->consFoldOccured)
                    {
                        if(n->children[0]->children[1]->consFoldOccured)
                        {
                            n->maximalConsFold = max(n->children[0]->children[0]->maximalConsFold, n->children[0]->children[1]->maximalConsFold);
                        }
                        else 
                        {
                            n->maximalConsFold = n->children[0]->children[0]->maximalConsFold;
                        }
                    }
                    else
                    {
                        if(n->children[0]->children[1]->consFoldOccured)
                        {
                            n->maximalConsFold = n->children[0]->children[1]->maximalConsFold;
                        }
                    }
                }
            }
        }
    }
    else
    {
        for(int i = 0; i < MAX && n->children[i] != NULL; i++)
        {
            firstPass(n->children[i]);
        }
    }
}

/*
Second Pass -> generates the postfix expressions for every Pexpr and expr node
*/
void secondPass(struct node* n)
{
    if(n->valueKnown == 1)
    {
        n->postfixExpr = "[" + to_string(n->value) + "]";
        n->atLeastOneOperator = 0;
        if(n->label == "if_stmt")
        {
            if(ifSimplified == 1)
            {
                if(n->children[5] == NULL)
                {
                    if(!(ifSimplification == 1))
                    {
                        secondPass(n->children[4]);
                    }
                }
                else
                {
                    if(!(ifSimplification == 1))
                    {
                        secondPass(n->children[4]);
                    }
                    else
                    {
                        secondPass(n->children[6]);
                    }
                }
            }
        }
        return;
    }
    if(n->label == "Pexpr")
    {
        if(n->children[0]->label == "identifier")
        {
            string id = n->children[0]->children[0]->label;
            n->postfixExpr = id;
            n->atLeastOneOperator = 0;
            n->identifiersInExpr.insert(id);
            exprDataTable[n->postfixExpr] = n->identifiersInExpr;
        }
        else if(n->children[0]->label == "LEFTBRACE")
        {
            secondPass(n->children[1]);
            n->postfixExpr = n->children[1]->postfixExpr;
            n->atLeastOneOperator = n->children[1]->atLeastOneOperator;
            n->identifiersInExpr = n->children[1]->identifiersInExpr;
            exprDataTable[n->postfixExpr] = n->identifiersInExpr;
            n->exprSet = n->children[1]->exprSet;
        }
    }
    else if(n->label == "expr")
    {
        if(n->children[0]->label == "Pexpr" && n->children[1] == NULL)
        {
            secondPass(n->children[0]);
            n->postfixExpr = n->children[0]->postfixExpr;
            n->atLeastOneOperator = n->children[0]->atLeastOneOperator;
            n->identifiersInExpr = n->children[0]->identifiersInExpr;
            exprDataTable[n->postfixExpr] = n->identifiersInExpr;
            n->exprSet = n->children[0]->exprSet;
        }
        else if(n->children[1] != NULL && n->children[1]->label == "Pexpr")
        {
            secondPass(n->children[1]);
            n->atLeastOneOperator = 1;
            if(n->children[0]->label == "NOT")
            {
                n->postfixExpr = n->children[1]->postfixExpr + "!";
            }
            else if(n->children[0]->label == "MINUS")
            {
                n->postfixExpr = n->children[1]->postfixExpr + "~";
            }
            else if(n->children[0]->label == "PLUS")
            {
                n->postfixExpr = n->children[1]->postfixExpr + "#";
            }
            n->identifiersInExpr = n->children[1]->identifiersInExpr;
            exprDataTable[n->postfixExpr] = n->identifiersInExpr;
            n->exprSet = n->children[1]->exprSet;
            n->exprSet.insert(pair<string, ll>(n->postfixExpr, n->lineNumber));
        }
        else 
        {
            secondPass(n->children[0]->children[0]);
            secondPass(n->children[0]->children[1]);
            string leftpfExpr = n->children[0]->children[0]->postfixExpr;
            string rightpfExpr = n->children[0]->children[1]->postfixExpr;
            n->atLeastOneOperator = 1;
            if(n->children[0]->label == "LT")
            {
                n->postfixExpr = leftpfExpr + rightpfExpr + "<";
            }
            else if(n->children[0]->label == "LOGAND")
            {
                n->postfixExpr = leftpfExpr + rightpfExpr + "&&";
            }
            else if(n->children[0]->label == "LOGOR")
            {
                n->postfixExpr = leftpfExpr + rightpfExpr + "||";
            }
            else if(n->children[0]->label == "GT")
            {
                n->postfixExpr = leftpfExpr + rightpfExpr + ">";
            }
            else if(n->children[0]->label == "LEQ")
            {
                n->postfixExpr = leftpfExpr + rightpfExpr + "<=";
            }
            else if(n->children[0]->label == "GEQ")
            {
                n->postfixExpr = leftpfExpr + rightpfExpr + ">=";
            }
            else if(n->children[0]->label == "ISEQ")
            {
                n->postfixExpr = leftpfExpr + rightpfExpr + "==";
            }
            else if(n->children[0]->label == "NEQ")
            {
                n->postfixExpr = leftpfExpr + rightpfExpr + "!=";
            }
            else if(n->children[0]->label == "THREEWAY")
            {
                n->postfixExpr = leftpfExpr + rightpfExpr + "<=>";
            }
            else if(n->children[0]->label == "PLUS")
            {
                n->postfixExpr = leftpfExpr + rightpfExpr + "+";
            }
            else if(n->children[0]->label == "MINUS")
            {
                n->postfixExpr = leftpfExpr + rightpfExpr + "-";
            }
            else if(n->children[0]->label == "STAR")
            {
                n->postfixExpr = leftpfExpr + rightpfExpr + "*";
            }
            else if(n->children[0]->label == "DIV")
            {
                n->postfixExpr = leftpfExpr + rightpfExpr + "/";
            }
            else if(n->children[0]->label == "MOD")
            {
                n->postfixExpr = leftpfExpr + rightpfExpr + "%";
            }        
            n->identifiersInExpr = n->children[0]->children[0]->identifiersInExpr;
            for(auto ele : n->children[0]->children[1]->identifiersInExpr)
            {
                n->identifiersInExpr.insert(ele);
            }
            exprDataTable[n->postfixExpr] = n->identifiersInExpr;
            n->exprSet = n->children[0]->children[0]->exprSet;
            for(auto ele : n->children[0]->children[1]->exprSet)
            {
                n->exprSet.insert(ele);
            }
            n->exprSet.insert(pair<string, ll>(n->postfixExpr, n->lineNumber));
        }
    }
    else
    {
        for(int i = 0; i < MAX && n->children[i] != NULL; i++)
        {
            secondPass(n->children[i]);
        }
    }
}

/*Third Pass, generates the CSE line numbers*/
void thirdPass(struct node* n)
{
    static unordered_map<string, ll> activeExpressions;
    static unordered_map<string, ll> ifActiveExpressions;
    static unordered_map<string, ll> elseActiveExpressions;
    static unordered_map<string, ll> savedActiveExpressions;
    static bool inIf;
    static bool inElse;
    static int count;
    if(n->label == "assign_stmt")
    {
        if(inIf == 1)
        {
            ifValueChanged[n->children[0]->children[0]->children[0]->label] = 1;
        }
        if(inElse == 1)
        {
            elseValueChanged[n->children[0]->children[0]->children[0]->label] = 1;
        }
        if(n->children[0]->children[1]->valueKnown != 1)
        {
            for(auto ele : n->children[0]->children[1]->exprSet)
            {
                auto it = activeExpressions.find(ele.first);
                if(it != activeExpressions.end())
                {
                    pair<string, ll> p(ele.first, (*it).second);
                    if(cseIndex.find(p) != cseIndex.end())
                    {
                        cse[cseIndex[p]].insert(ele.second);
                    }
                    else
                    {
                        count++;
                        multiset<ll> newSet;
                        newSet.insert((*it).second);
                        newSet.insert(ele.second);
                        cseIndex[p] = count;
                        cse[count] = newSet;
                    }
                }
                else
                {
                    activeExpressions.insert(ele);
                }
            }
        }
        string id = n->children[0]->children[0]->children[0]->label;
        vector<string> temp;
        for(auto ele : activeExpressions)
        {
            if(exprDataTable[ele.first].find(id) != exprDataTable[ele.first].end())
            {
                temp.push_back(ele.first);
            }
        }
        for(auto expr : temp)
        {
            activeExpressions.erase(expr);
        }
    }
    else if(n->label == "scan_stmt")
    {
        if(inIf == 1)
        {
            ifValueChanged[n->children[5]->children[0]->label] = 1;
        }
        if(inElse == 1)
        {
            elseValueChanged[n->children[5]->children[0]->label] = 1;
        }
        string id = n->children[5]->children[0]->label;
        vector<string> temp;
        for(auto ele : activeExpressions)
        {
            if(exprDataTable[ele.first].find(id) != exprDataTable[ele.first].end())
            {
                temp.push_back(ele.first);
            }
        }
        for(auto expr : temp)
        {
            activeExpressions.erase(expr);
        }
    }
    else if(n->label == "if_stmt")
    {
        if(ifSimplified != 1)
        {
            for(auto ele : n->children[2]->exprSet)
            {
                auto it = activeExpressions.find(ele.first);
                if(it != activeExpressions.end())
                {
                    pair<string, ll> p(ele.first, (*it).second);
                    if(cseIndex.find(p) != cseIndex.end())
                    {
                        cse[cseIndex[p]].insert(ele.second);
                    }
                    else
                    {
                        count++;
                        multiset<ll> newSet;
                        newSet.insert((*it).second);
                        newSet.insert(ele.second);
                        cseIndex[p] = count;
                        cse[count] = newSet;
                    }
                }
                else
                {
                    activeExpressions.insert(ele);
                }
            }
            if(n->children[5] == NULL)
            {
                savedActiveExpressions = activeExpressions;
                for(auto ele : savedSymbolTable)
                {
                    ifValueChanged[ele.first] = false;
                }
                inIf = 1;
                thirdPass(n->children[4]);
                inIf = 0;
                ifActiveExpressions = activeExpressions;
                activeExpressions.clear();
                for(auto activeExprPair : ifActiveExpressions)
                {
                    auto it = savedActiveExpressions.find(activeExprPair.first);
                    if(it != savedActiveExpressions.end())
                    {
                        for(auto ele : exprDataTable[(*it).first])
                        {
                            if((afterSymbolTable[ele]->valueKnown == 1) && (savedSymbolTable[ele]->valueKnown == 1) && (afterSymbolTable[ele]->value == savedSymbolTable[ele]->value))
                            {
                                continue;
                            }
                            else if(ifValueChanged[ele] != 1)
                            {
                                continue;
                            }
                            else
                            {
                                goto end1;
                            }
                        }
                        activeExpressions.insert(activeExprPair);
                    }
                    end1: 
                    ;
                }
            }
            else
            {
                savedActiveExpressions = activeExpressions;
                for(auto ele : savedSymbolTable)
                {
                    ifValueChanged[ele.first] = false;
                    elseValueChanged[ele.first] = false;
                }
                inIf = 1;
                thirdPass(n->children[4]);
                inIf = 0;
                ifActiveExpressions = activeExpressions;
                activeExpressions = savedActiveExpressions;
                inElse = 1;
                thirdPass(n->children[6]);
                inElse = 0;
                elseActiveExpressions = activeExpressions;
                activeExpressions.clear();
                for(auto activeExprPair : ifActiveExpressions)
                {
                    auto it = elseActiveExpressions.find(activeExprPair.first);
                    if(it != elseActiveExpressions.end())
                    {
                        for(auto ele : exprDataTable[(*it).first])
                        {
                            if((afterSymbolTable[ele]->valueKnown == 1) && (savedSymbolTable[ele]->valueKnown == 1) && (afterSymbolTable[ele]->value == savedSymbolTable[ele]->value))
                            {
                                continue;
                            }
                            else if(ifValueChanged[ele] != 1 && elseValueChanged[ele] != 1)
                            {
                                continue;
                            }
                            else
                            {
                                goto end2;
                            }
                        }
                        activeExpressions.insert(activeExprPair);
                    }
                    end2: 
                    ;
                }
                for(auto ele : activeExpressions)
                {
                    if(savedActiveExpressions.find(ele.first) == savedActiveExpressions.end())
                    {
                        auto ifIt = ifActiveExpressions.find(ele.first);
                        auto elseIt = elseActiveExpressions.find(ele.first);
                        pair<string, ll> p(ele.first, (*ifIt).second);
                        count++;
                        multiset<ll> newSet;
                        newSet.insert((*ifIt).second);
                        newSet.insert((*elseIt).second);
                        cseIndex[p] = count;
                        cse[count] = newSet;
                    }
                }
            }
        }
        else
        {
            if(n->children[5] == NULL)
            {
                if(!(ifSimplification == 1))
                {
                    thirdPass(n->children[4]);
                }
            }
            else
            {
                if(!(ifSimplification == 1))
                {
                    thirdPass(n->children[4]);
                }
                else
                {
                    thirdPass(n->children[6]);
                }
            }
        }
    }
    else
    {
        for(int i = 0; i < MAX && n->children[i] != NULL; i++)
        {
            thirdPass(n->children[i]);
        }
    }
}

void fourthPass(struct node* n)
{
    if(n->label == "STAR")
    {
        bool leftValueKnown = n->children[0]->valueKnown;
        bool rightValueKnown = n->children[1]->valueKnown;
        if((leftValueKnown && !(rightValueKnown)) || (rightValueKnown && !(leftValueKnown)))
        {
            if(leftValueKnown)
            {
                ll leftValue = n->children[0]->value;
                if(ceil(log2(leftValue)) == floor(log2(leftValue)))
                {
                    ll p = log2(leftValue);
                    if(strengthReduction.find(n->lineNumber) != strengthReduction.end())
                    {
                        strengthReduction[n->lineNumber] = max(strengthReduction[n->lineNumber], p);
                    }
                    else
                    {
                        strengthReduction[n->lineNumber] = p;
                    }
                }
            }
            else
            {
                ll rightValue = n->children[1]->value;
                if(ceil(log2(rightValue)) == floor(log2(rightValue)))
                {
                    ll p = log2(rightValue);
                    if(strengthReduction.find(n->lineNumber) != strengthReduction.end())
                    {
                        strengthReduction[n->lineNumber] = max(strengthReduction[n->lineNumber], p);
                    }
                    else
                    {
                        strengthReduction[n->lineNumber] = p;
                    }
                }
            }
        }
    }
    else
    {
        for(int i = 0; i < MAX && n->children[i] != NULL; i++)
        {
            fourthPass(n->children[i]);
        }
    }
}

void codeGenerator(struct node* n)
{
    static ll currentStackOffset;
    static bool inMain = 0;
    static bool returnStmt = 0;
    static unordered_map<string, ll> activeExpressions;
    static unordered_map<string, ll> ifActiveExpressions;
    static unordered_map<string, ll> elseActiveExpressions;
    static unordered_map<string, ll> savedActiveExpressions;
    static ll extraStackOffset = -8000;
    if(n->label == "func_decl")
    {
        symbolTable.clear();
        returnStmt = 0;
        currentStackOffset = -24;
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
            initializeRecord(n->children[1]->children[0]->label);
            asmCode.push_back("pushq $0");
            symbolTable[n->children[1]->children[0]->label]->stackOffset = currentStackOffset;
            currentStackOffset -= 8;
        }
    }
    else if(n->label == "assign_stmt")
    {
        int i;
        for(i = 0; i < MAX && n->children[0]->children[i] != NULL ; i++);
        if(i == 2)
        {
            string id = n->children[0]->children[0]->children[0]->label;
            if(n->children[0]->children[1]->valueKnown == 1)
            {
                ll stkOffset = symbolTable[id]->stackOffset;
                asmCode.push_back("movl $" + to_string(n->children[0]->children[1]->value) + ", " + to_string((int)stkOffset) + "(%rbp)");
                symbolTable[id]->valueKnown = 1;
                symbolTable[id]->value = n->children[0]->children[1]->value;
            }
            else
            {
                codeGenerator(n->children[0]->children[1]);
                asmCode.push_back("popq %rax");
                ll stkOffset = symbolTable[id]->stackOffset;
                asmCode.push_back("movl %eax, " + to_string((int)stkOffset) + "(%rbp)");
                symbolTable[id]->valueKnown = 0;
            }
            vector<string> temp;
            for(auto ele : activeExpressions)
            {
                if(exprDataTable[ele.first].find(id) != exprDataTable[ele.first].end())
                {
                    temp.push_back(ele.first);
                }
            }
            for(auto expr : temp)
            {
                activeExpressions.erase(expr);
            }
        }
    }
    else if(n->label == "if_stmt")
    {
        for(auto ele : savedSymbolTable)
        {
            savedSymbolTable[ele.first]->stackOffset = symbolTable[ele.first]->stackOffset;
        }
        for(auto ele : afterSymbolTable)
        {
            afterSymbolTable[ele.first]->stackOffset = symbolTable[ele.first]->stackOffset;
        }
        if(ifSimplified != 1)
        {
            savedActiveExpressions = activeExpressions;
            if(n->children[5] == NULL)  //if statement
            {
                codeGenerator(n->children[2]);
                asmCode.push_back("popq %rax");
                string newLabel = labelGenerator("if_stmt");
                asmCode.push_back("movl $0, %ecx");
                asmCode.push_back("movl %eax, %edx");
                asmCode.push_back("cmpl %ecx, %edx");
                asmCode.push_back("je " + newLabel);
                copysymtab(savedSymbolTable, symbolTable);
                codeGenerator(n->children[4]);
                ifActiveExpressions = activeExpressions;
                asmCode.push_back("" + newLabel + ":");
                activeExpressions.clear();
                for(auto activeExprPair : ifActiveExpressions)
                {
                    auto it = savedActiveExpressions.find(activeExprPair.first);
                    if(it != savedActiveExpressions.end())
                    {
                        for(auto ele : exprDataTable[(*it).first])
                        {
                            if((afterSymbolTable[ele]->valueKnown == 1) && (savedSymbolTable[ele]->valueKnown == 1) && (afterSymbolTable[ele]->value == savedSymbolTable[ele]->value))
                            {
                                continue;
                            }
                            else if(ifValueChanged[ele] != 1)
                            {
                                continue;
                            }
                            else
                            {
                                goto end3;
                            }
                        }
                        activeExpressions.insert(activeExprPair);
                    }
                    end3: 
                    ;
                }
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
                asmCode.push_back("je " + newLabel);
                copysymtab(savedSymbolTable, symbolTable);
                codeGenerator(n->children[4]);
                ifActiveExpressions = activeExpressions;
                activeExpressions = savedActiveExpressions;
                asmCode.push_back("jmp " + endLabel);
                asmCode.push_back("" + newLabel + ":");
                copysymtab(savedSymbolTable, symbolTable);
                codeGenerator(n->children[6]);
                elseActiveExpressions = activeExpressions;
                asmCode.push_back("" + endLabel + ":");
                activeExpressions.clear();
                for(auto activeExprPair : ifActiveExpressions)
                {
                    auto it = elseActiveExpressions.find(activeExprPair.first);
                    if(it != elseActiveExpressions.end())
                    {
                        for(auto ele : exprDataTable[(*it).first])
                        {
                            if((afterSymbolTable[ele]->valueKnown == 1) && (savedSymbolTable[ele]->valueKnown == 1) && (afterSymbolTable[ele]->value == savedSymbolTable[ele]->value))
                            {
                                continue;
                            }
                            else if(ifValueChanged[ele] != 1 && elseValueChanged[ele] != 1)
                            {
                                continue;
                            }
                            else
                            {
                                goto end4;
                            }
                        }
                        activeExpressions.insert(activeExprPair);
                    }
                    end4: 
                    ;
                }
            }
            copysymtab(afterSymbolTable, symbolTable);
        }
        else
        {
            if(n->children[5] == NULL)
            {
                if(!(ifSimplification == 1))
                {
                    codeGenerator(n->children[4]);
                }
            }
            else
            {
                if(!(ifSimplification == 1))
                {
                    codeGenerator(n->children[4]);
                }
                else
                {
                    codeGenerator(n->children[6]);
                }
            }
        }
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
        asmCode.push_back("jmp .leave_main_label");    

    }
    else if(n->label == "print_stmt")
    {
        string id = n->children[4]->children[0]->label; 
        if(symbolTable[id]->valueKnown != 1)
        {
            ll stkOffset = symbolTable[id]->stackOffset;
            asmCode.push_back("movl " + to_string((int)stkOffset) + "(%rbp), %esi");
        }
        else
        {
            asmCode.push_back("movl $" + to_string(symbolTable[id]->value) + ", %esi");
        }
        asmCode.push_back("leaq .LC0(%rip), %rdi");
        asmCode.push_back("movl $0, %eax");
        asmCode.push_back("call printf@PLT");
    }
    else if(n->label == "scan_stmt")
    {
        string id = n->children[5]->children[0]->label; 
        ll stkOffset = symbolTable[id]->stackOffset;
        asmCode.push_back("leaq " + to_string((int)stkOffset) + "(%rbp), %rax");
        asmCode.push_back("movq %rax, %rsi");
        asmCode.push_back("leaq .LC1(%rip), %rdi");
        if(((-currentStackOffset)/8)%2 == 0)
        {
            asmCode.push_back("pushq $0");
        }
        asmCode.push_back("movl $0, %eax");
        asmCode.push_back("call scanf@PLT");
        if(((-currentStackOffset)/8)%2 == 0)
        {
            asmCode.push_back("popq %rax");
        }
        vector<string> temp;
        for(auto ele : activeExpressions)
        {
            if(exprDataTable[ele.first].find(id) != exprDataTable[ele.first].end())
            {
                temp.push_back(ele.first);
            }
        }
        for(auto expr : temp)
        {
            activeExpressions.erase(expr);
        }
        symbolTable[id]->valueKnown = 0;
    }
    else if(n->label == "Pexpr")
    {
        if(n->valueKnown == 1)
        {
            asmCode.push_back("movl $" + to_string(n->value) + ", %eax");
            asmCode.push_back("pushq %rax");
            return;
        }
        else
        {
            if(n->children[0]->label == "identifier")
            {
                string id = n->children[0]->children[0]->label;
                ll stkOffset = symbolTable[id]->stackOffset;
                if(symbolTable[id]->valueKnown == 1)
                {
                    asmCode.push_back("movl $" + to_string((int)symbolTable[id]->value) + ", %eax");
                    asmCode.push_back("pushq %rax");
                }
                else
                {
                    asmCode.push_back("movq " + to_string((int)stkOffset) + "(%rbp), %rax"); //allowing 64 bit in case of array addresses
                    asmCode.push_back("pushq %rax");
                }
            }
            else if(n->children[0]->label == "LEFTBRACE")
            {
                codeGenerator(n->children[1]);
            }
        }
    }
    else if(n->label == "expr")
    {
        if(n->valueKnown == 1)
        {
            asmCode.push_back("movl $" + to_string(n->value) + ", %eax");
            asmCode.push_back("pushq %rax");
            return;
        }
        auto it = activeExpressions.find(n->postfixExpr);
        if(it != activeExpressions.end())
        {
            ll stkOffset = (*it).second;
            asmCode.push_back("movl " + to_string((int)stkOffset) + "(%rbp), %eax");
            asmCode.push_back("pushq %rax");
            return;
        }
        else
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
                        bool leftValueKnown = n->children[0]->children[0]->valueKnown;
                        bool rightValueKnown = n->children[0]->children[1]->valueKnown;
                        if((leftValueKnown && !(rightValueKnown)) || (rightValueKnown && !(leftValueKnown)))
                        {
                            if(leftValueKnown)
                            {
                                ll leftValue = n->children[0]->children[0]->value;
                                if(ceil(log2(leftValue)) == floor(log2(leftValue)))
                                {
                                    ll p = log2(leftValue);
                                    asmCode.push_back("sall $" + to_string(p) + ", %ecx");
                                    asmCode.push_back("movl %ecx, %eax");
                                }
                                else
                                {
                                    asmCode.push_back("imull %ecx, %eax");
                                }
                            }
                            else
                            {
                                ll rightValue = n->children[0]->children[1]->value;
                                if(ceil(log2(rightValue)) == floor(log2(rightValue)))
                                {
                                    ll p = log2(rightValue);
                                    asmCode.push_back("sall $" + to_string(p) + ", %eax");
                                }
                                else
                                {
                                    asmCode.push_back("imull %ecx, %eax");
                                }
                            }
                        }
                        else
                        {
                            asmCode.push_back("imull %ecx, %eax");
                        }
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
            if(n->atLeastOneOperator == 1)
            {
                asmCode.push_back("popq %rax");
                asmCode.push_back("pushq %rax");
                ll stkOffset = extraStackOffset;
                asmCode.push_back("movl %eax, " + to_string((int)extraStackOffset) + "(%rbp)");
                pair<string, ll> p(n->postfixExpr, stkOffset);
                activeExpressions.insert(p);
                extraStackOffset -= 8;   
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

inline bool comparatorUnUsedVars(pair<ll, string> p1, pair<ll, string> p2)
{
    return p1.first < p2.first;
}

void unusedVarsFormatting()
{
    vector<pair<ll, string> > unusedVarsHelper;
    for(auto x : firstPassSymbolTable)
    {
        if(x.second->used != 1)
        {
            pair<ll, string> p;
            p.second = x.first;
            p.first = x.second->declarationNumber;
            unusedVarsHelper.push_back(p);
        }
    }
    sort(unusedVarsHelper.begin(), unusedVarsHelper.end(), comparatorUnUsedVars);
    summaryText.push_back("unused-vars\n");
    for(auto x : unusedVarsHelper)
    {
        unusedVars.insert(x.second);
        summaryText.push_back(x.second + "\n");
    }
    summaryText.push_back("\n");
}

void strengthReductionFomatting()
{
    summaryText.push_back("strength-reduction\n");
    for(auto ele : strengthReduction)
    {
        summaryText.push_back(to_string(ele.first) + " " + to_string(ele.second) + "\n");
    }
    summaryText.push_back("\n");
    return;
}

void constantFoldingFormatting()
{
    summaryText.push_back("constant-folding\n");
    for(auto x : consFold)
    {
        summaryText.push_back(to_string(x.first) + " " + to_string(x.second) + "\n");
    }
    summaryText.push_back("\n");
}

void ifSimplFormatting()
{
    summaryText.push_back("if-simpl\n");
    if(ifExists && ifSimplified)
        summaryText.push_back(to_string((int)!(ifSimplification)) + "\n");
    summaryText.push_back("\n");
}

inline bool comparatorConstProp(pair<ll, pair<string, ll> > p1, pair<ll, pair<string, ll> > p2)
{
    return p1.first < p2.first;
}

void constantPropagationFormatting()
{
    summaryText.push_back("constant-propagation\n");
    for(auto i : constantPropagation)
    {
        if(i.second.size() != 0)
        {
            string s = "";
            s += to_string(i.first) + " ";
            for(auto elements : i.second)
            {
                s += elements.second.first + " " + to_string(elements.second.second) + " ";
            }
            s.pop_back();
            s += "\n";
            summaryText.push_back(s);
        }
    }
    summaryText.push_back("\n");
}

struct setComparator
{
    bool operator() (const multiset<ll>& s1, const multiset<ll>& s2) const
    {
        return *(s1.begin()) < *(s2.begin());
    }
};

void cseFormatting()
{
    summaryText.push_back("cse\n");
    multiset<multiset<ll>, setComparator > S;
    for(auto i : cse)
    {
        S.insert(i.second);
    }
    for(auto i : S)
    {
        string s = "";
        for(auto ele : i)
        {
            s += to_string(ele) + " ";
        }
        s.pop_back();
        s += "\n";
        summaryText.push_back(s);
    }
}

int main(int argc, char* argv[]) 
{
    int a = yyparse();
    if(a == 0)
    {
        firstPass(ast);
        unusedVarsFormatting();
        //printtree(ast, 0);
        secondPass(ast);    
        thirdPass(ast);
        fourthPass(ast);
        asmCode.push_back(".text");
        asmCode.push_back(".section	.rodata");
        asmCode.push_back(".LC0:");
        asmCode.push_back(".string \"%d\\n\"");
        asmCode.push_back(".LC1:");
        asmCode.push_back(".string \"%d\"");
        asmCode.push_back(".text");
        codeGenerator(ast);
        ifSimplFormatting();
        strengthReductionFomatting();
        constantFoldingFormatting();
        constantPropagationFormatting();
        cseFormatting();
        ofstream myfile1;
        myfile1.open("summary.txt");
        for(auto ele : summaryText)
        {
            myfile1<<ele;
        }
        ofstream myfile2;
        myfile2.open("assembly.s");
        for(auto ele : asmCode)
        {
            myfile2<<ele<<endl;
        }
    }
    else
    {
        cout<<"syntax error"<<endl;
        exit(0);
    }
    return 0;
}