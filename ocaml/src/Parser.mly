%{
open ConfSolve
open Util
%}

%token LCURLY RCURLY
%token LPAREN RPAREN
%token LSQUARE RSQUARE
%token EQ NEQ GT GE LT LE
%token ADD SUB MUL DIV MOD
%token AND OR IMPLIES IFF
%token NOT FORALL EXISTS SUM COUNT IN
%token SUBSET UNION INTERSECTION
%token VAR PARAM AS INT BOOL REF DOTS
%token DOT COMMA
%token CLASS EXTENDS ABSTRACT ENUM
%token SEMICOLON
%token OLD CHANGE INIT
%token WHERE MAXIMIZE MINIMIZE
%token TRUE FALSE
%token THIS
%token BOOL2INT ABS
%token <Lexing.position * string> ID
%token <int> INT_LITERAL
%token EOL
%token EOF

/* precedence: low to high*/
%left FORALL SUM
%left AND OR IMPLIES IFF
%left EQ NEQ GT GE LT LE
%left ABS
%left ADD SUB
%left MUL DIV MOD
%left IN SUBSET UNION INTERSECTION
%nonassoc UMINUS NOT
%left OLD
%left DOT

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
| paramDecl SEMICOLON { $1 }
| classDecl { $1 }
| enumDecl  { $1 }
| constraint_ SEMICOLON { $1 }
| constraintBlock { $1 }
;

constraintBlock:
  CHANGE LCURLY constraintList RCURLY   { Block (Change, $3) }
| INIT   LCURLY constraintList RCURLY   { Block (Init, $3) }
  
constraintList:
  constraintBody SEMICOLON                  { $1 :: [] }
| constraintList constraintBody SEMICOLON   { $1 @ [$2] }
;

varDecl:
  VAR id AS type { Var ($2,$4) }
;

paramDecl:
  PARAM id AS type { Param ($2,$4) }
;

id:
  ID { snd $1 }
;

type:
 INT            { T_Int }
| finiteType    { $1 }
| setType       { $1 }
;

finiteType:
| BOOL                          { T_Bool }
| range                         { $1 }
| finiteInt                     { $1 }
| REF id                        { T_Ref $2 }
| id                            { T_Symbol $1 } /* may or may not actually be finite */
;

range:
  INT_LITERAL DOTS INT_LITERAL  { T_BInt (makeSet $1 $3) } /* Q: allow nesting of any (constant) expression? */
;

finiteInt:
  LCURLY fiElementList RCURLY  { T_BInt $2 } /* Q: allow nesting of any (constant) expression? */
  
fiElementList: /* Q: allow nesting of any (constant) expression? */
| INT_LITERAL                      { IntSet.singleton $1 }
| fiElementList COMMA INT_LITERAL  { IntSet.add $3 $1 }
;

setType: /* Q: allow nesting of any (constant) expression, instead of INT_LITERAL? */
  finiteType LSQUARE INT_LITERAL DOTS INT_LITERAL RSQUARE   { T_Set ($1, $3, $5) }
| finiteType LSQUARE INT_LITERAL RSQUARE                    { T_Set ($1, $3, $3) }
| finiteType LSQUARE RSQUARE                                { T_Set ($1, -1, -1) }

enumDecl:
  ENUM id LCURLY enumElementList RCURLY  { Enum { enumName=$2; elements=$4 } }
;

enumElementList:
| enumElement                        { $1 :: [] }
| enumElementList COMMA enumElement  { $1 @ [$3] }
;

enumElement:
  id  { $1 }
;

classDecl:
  isAbstract CLASS id EXTENDS id memberDeclBlock  { Class { name=$3; super=Some $5; members=$6; isAbstract=$1 } }
| isAbstract CLASS id memberDeclBlock             { Class { name=$3; super=None; members=$4; isAbstract=$1 } }
;

isAbstract:
  ABSTRACT  { true }
|           { false }

memberDeclBlock:
  LCURLY RCURLY                   { [] }
| LCURLY memberDeclList RCURLY    { $2 }
;

memberDeclList:
| memberDecl SEMICOLON                   { $1 :: [] }
| memberDeclList memberDecl SEMICOLON    { $1 @ [$2] }
;

memberDecl:
| varDecl     { $1 }
| paramDecl   { $1 }
| constraint_ { $1 }
| constraintBlock { $1 }
;

constraint_:
  constraintBody    { Constraint $1 }
;

constraintBody:
  expr            { C_Where $1 }
| MAXIMIZE expr   { C_Maximise $2 }
| MINIMIZE expr   { C_Maximise (E_Neg $2) }
;

foldKind:
  FORALL  { ForAll }
| EXISTS  { Exists }
| SUM     { Sum }

fold:
  foldKind id IN expr WHERE expr exprBlock  { E_Fold ($1, ($2, T_Infer), $4, $6, $7) }
| foldKind id IN expr exprBlock             { E_Fold ($1, ($2, T_Infer), $4, E_Bool true, $5) }

count: /* NOTE: parens are needed to avoid shift/reduce conflict */
  COUNT LPAREN id IN expr WHERE expr RPAREN { E_Fold (Sum, ($3, T_Infer), $5, $7, E_Int 1) }
| COUNT LPAREN id IN expr RPAREN { E_Fold (Sum, ($3, T_Infer), $5, E_Bool true, E_Int 1) }

expr:
| expr DOT id                  { if $3 = "size" then E_Card $1 else E_Access ($1, $3) }
| symbol                       { $1 }
| binaryExpr                   { $1 }
| fold                         { $1 }
| count                        { $1 }
| setLiteral                   { $1 }
| BOOL2INT LPAREN expr RPAREN  { E_BoolToInt $3 }
| ABS LPAREN expr RPAREN       { E_Abs $3 }
| SUB expr %prec UMINUS        { E_Neg $2 }
| NOT expr                     { E_Not $2 }
| OLD expr                     { E_Old $2 }
| TRUE                         { E_Bool true }
| FALSE                        { E_Bool false }
| INT_LITERAL                  { E_Int $1 }
| LPAREN expr RPAREN           { E_Paren $2 }
;

setLiteral:
  LCURLY setElementList RCURLY  { E_Set $2 }
;

setElementList:
| expr                       { $1 :: [] }
| setElementList COMMA expr  { $1 @ [$3] }
;

symbol:
  id                      { E_Symbol $1 }
;

binaryExpr:
  expr EQ expr            { E_Op ($1, Eq, $3) }
| expr NEQ expr           { E_Op ($1, Neq, $3) }
| expr GT expr            { E_Op ($1, Gt, $3) }
| expr GE expr            { E_Op ($1, Ge, $3) }
| expr LT expr            { E_Op ($1, Lt, $3) }
| expr LE expr            { E_Op ($1, Le, $3) }

| expr IN expr            { E_Op ($1, In, $3) }
| expr SUBSET expr        { E_Op ($1, Subset, $3) }
| expr UNION expr         { E_Op ($1, Union, $3) }
| expr INTERSECTION expr  { E_Op ($1, Intersection, $3) }

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