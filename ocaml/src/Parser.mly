%{
open ConfSolve
%}

%token LCURLY RCURLY
%token LPAREN RPAREN
%token LSQUARE RSQUARE
%token EQ NEQ GT GE LT LE
%token ADD SUB MUL DIV MOD
%token AND OR IMPLIES IFF
%token NOT FORALL EXISTS SUM COUNT IN
%token SUBSET UNION INTERSECTION
%token VAR AS INT BOOL REF DOTS
%token DOT
%token CLASS EXTENDS END
%token SEMICOLON
%token WHERE MAXIMIZE
%token TRUE FALSE
%token <Lexing.position * string> ID
%token <int> INT_LITERAL
%token EOL
%token EOF

/* precedence: low to high*/
%left FORALL SUM
%left AND OR IMPLIES IFF
%left EQ NEQ GT GE LT LE
%left ADD SUB
%left IN SUBSET UNION INTERSECTION
%left MUL DIV MOD
%nonassoc UMINUS NOT

%start model

%type <ConfSolve.model> model
%type <_type> type

%%

model:
 declarationList EOF   { { declarations = $1 } }
;

declarationList:
  declaration                   { $1 :: [] }
| declarationList declaration   { $1 @ [$2] }
;

declaration:
  varDecl   SEMICOLON { $1 }
| classDecl { $1 }
/* enum... */
| globalConstraint SEMICOLON { $1 }
;

varDecl:
  VAR id AS type { G_Var ($2,$4) }
;

id:
  ID { snd $1 }
;

type:
 INT            { T_Int }
| finiteType    { $1 }
| id            { T_Class $1 }
| setType       { $1 }
/* ... */
;

finiteType:
| BOOL                          { T_Bool }
| INT_LITERAL DOTS INT_LITERAL  { T_Range ($1, $3) }
| REF id                        { T_Ref $2 }
;

setType:
  finiteType LSQUARE INT_LITERAL DOTS INT_LITERAL RSQUARE   { T_Set ($1, $3, $5) }
| finiteType LSQUARE INT_LITERAL RSQUARE                    { T_Set ($1, $3, $3) }
| id LSQUARE INT_LITERAL RSQUARE                            { T_Set (T_Class $1, $3, $3) } /* move to finiteType once var-card */

classDecl:
  CLASS id EXTENDS id memberDeclBlock  { G_Class { name=$2; super=Some $4; members=$5 } }
| CLASS id memberDeclBlock             { G_Class { name=$2; super=None; members=$3 } }
;

memberDeclBlock:
  LCURLY RCURLY                   { [] }
| LCURLY memberDeclList RCURLY    { $2 }
;

memberDeclList:
| memberDecl SEMICOLON                   { $1 :: [] }
| memberDeclList memberDecl SEMICOLON    { $1 @ [$2] }
;

memberDecl:
| memberVarDecl    { $1 }
| memberConstraint { $1 }
;

memberVarDecl:
  VAR id AS type { M_Var ($2,$4) }
;

memberConstraint:
  constr    { M_Constraint $1 }
;

globalConstraint:
  constr    { G_Constraint $1 }
;

constr:
  expr            { C_Where $1 }
| MAXIMIZE expr   { C_Maximise $2 }
;

foldKind:
  FORALL  { ForAll }
| EXISTS  { Exists }
| SUM     { Sum }

fold:
  foldKind id IN expr WHERE expr exprBlock  { E_Fold ($1, $2, $4, $6, $7) }
| foldKind id IN expr exprBlock             { E_Fold ($1, $2, $4, E_Bool true, $5) }

count: /* NOTE: parens are needed to avoid shift/reduce conflict */
  COUNT LPAREN id IN expr WHERE expr RPAREN { E_Fold (Sum, $3, $5, $7, E_Int 1) }
| COUNT LPAREN id IN expr RPAREN { E_Fold (Sum, $3, $5, E_Bool true, E_Int 1) }

expr:
| variable                { $1 }
/* ... */
| binaryExpr              { $1 }
/* ... */
| fold                    { $1 }
| count                   { $1 }
| SUB expr %prec UMINUS   { E_Neg $2 }
| NOT expr                { E_Not $2 }
| TRUE                    { E_Bool true }
| FALSE                   { E_Bool false }
| INT_LITERAL             { E_Int $1 }
| LPAREN expr RPAREN      { E_Paren $2 }
;

variable:
  id                      { E_Var $1 }
| variable DOT id         { E_Access ($1, $3) }

binaryExpr:
  expr EQ expr            { E_Op ($1, Eq, $3) }
| expr NEQ expr           { E_Op ($1, Neq, $3) }
| expr GT expr            { E_Op ($1, Gt, $3) }
| expr GE expr            { E_Op ($1, Ge, $3) }
| expr LT expr            { E_Op ($1, Lt, $3) }
| expr LE expr            { E_Op ($1, Le, $3) }

| expr IN expr            { E_Op ($1, In, $3) }
| expr SUBSET expr        { E_Op ($1, In, $3) }
| expr UNION expr         { E_Op ($1, In, $3) }
| expr INTERSECTION expr  { E_Op ($1, In, $3) }

| expr AND expr           { E_Op ($1, And, $3) }
| expr OR expr            { E_Op ($1, Or, $3) }
| expr IMPLIES expr       { E_Op ($1, Implies, $3) }
| expr IFF expr           { E_Op ($1, Iff, $3) }

| expr ADD expr           { E_Op ($1, Add, $3) }
| expr SUB expr           { E_Op ($1, Sub, $3) }
| expr MUL expr           { E_Op ($1, Mul, $3) }
| expr DIV expr           { E_Op ($1, Div, $3) }
| expr MOD expr           { E_Op ($1, Mod, $3) }
;

exprBlock:
  LCURLY exprList RCURLY  { $2 }
  
exprList:
  expr SEMICOLON          { $1 }
| exprList expr SEMICOLON { E_Op (E_Paren $1, And, E_Paren $2) }