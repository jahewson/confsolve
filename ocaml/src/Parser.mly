/* File parser.mly */
%{
open ConfSolve
%}

%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token VAR AS INT BOOL
%token SEMICOLON
%token <string> IDENTIFIER
%token <int> INT_LITERAL
%token EOL
%token EOF

%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */

%start model             /* the entry point */

%type <ConfSolve.model> model
%type <globalDecl list> globalDeclList
%type <globalDecl> globalDecl
%type <_type> type

%%

model:
   globalDeclList EOF                { { declarations=$1 } }
;

globalDeclList:
    globalDecl                               { $1 :: [] }
  | globalDeclList SEMICOLON globalDecl      { $1 @ [$3] }
;

globalDecl:
    VAR IDENTIFIER AS type { G_Var ($2,$4) }
;

type:
    INT   { T_Int }
  | BOOL  { T_Bool }
;