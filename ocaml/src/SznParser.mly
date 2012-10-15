%{
open SznSolution
open Util
%}

%token LCURLY RCURLY
%token LPAREN RPAREN
%token LSQUARE RSQUARE
%token EQ
%token ARRAY1D
%token DOTS COMMA
%token SEMICOLON
%token TRUE FALSE
%token <string> ID
%token <int> INT_LITERAL
%token EOF
%token SOL_END ALL_SOL_END

%start solutions

%type <SznSolution.solutions> solutions

%%

solutions:
  solutionList EOF              { $1 }
| solutionList ALL_SOL_END EOF  { $1 }
  
solutionList:
  solution                { $1 :: [] }
| solutionList solution   { $1 @ [$2] }
;

solution:
 assignmentMap SOL_END   { $1 }
;

assignmentMap:
  assignment                  { StrMap.add (fst $1) (snd $1) StrMap.empty }
| assignmentMap assignment    { StrMap.add (fst $2) (snd $2) $1 }
;

assignment:
  ID EQ value SEMICOLON { ($1, $3) }
;

value:
  INT_LITERAL { V_Int $1 }
/*| ARRAY1D LPAREN INT_LITERAL DOTS INT_LITERAL COMMA LSQUARE valueList RSQUARE RPAREN { V_Array $8 }*/
| LSQUARE valueList RSQUARE { V_Array $2 }
| INT_LITERAL DOTS INT_LITERAL { V_Range ($1, $3) }
| LCURLY valueList RCURLY { V_Set $2 }
| TRUE { V_Bool true }
| FALSE { V_Bool false }

valueList:
                          { [] }
| value                   { $1 :: [] }
| valueList COMMA value   { $1 @ [$3] }
;