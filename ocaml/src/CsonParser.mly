%{
open CsonSolution
open Util
%}

%token LCURLY RCURLY
%token LPAREN RPAREN
%token LSQUARE RSQUARE
%token COLON REF DOT
%token DOTS COMMA
%token SEMICOLON
%token TRUE FALSE
%token <string> ID
%token <int> INT_LITERAL
%token EOF

%start solution

%type <CsonSolution.solution> solution

%%

solution:
 LCURLY assignmentMap RCURLY EOF { $2 }
;

assignmentMap:
                               { StrMap.empty }
| assignmentMapE optionalComma { $1 }

optionalComma:
          { }
| COMMA   { }
  
assignmentMapE:
  assignment                        { StrMap.add (fst $1) (snd $1) StrMap.empty }
| assignmentMapE COMMA assignment   { StrMap.add (fst $3) (snd $3) $1 }
;

assignment:
  ID COLON value { ($1, $3) }
;

value:
  INT_LITERAL                     { V_Int $1 }
| LSQUARE valueList RSQUARE       { V_Set $2 }
| TRUE                            { V_Bool true }
| FALSE                           { V_Bool false }
| ID LCURLY assignmentMap RCURLY  { V_Object { name = $1; members = $3 } }
| REF target                      { V_Ref $2 }
| ID DOT ID                       { V_Enum ($1, $3) }

target:
  ID                                  { R_Var $1 }
| target DOT ID                       { R_Access ($1, $3) }
| target LSQUARE INT_LITERAL RSQUARE  { R_Index ($1, $3) }

valueList:
                          { [] }
| value                   { $1 :: [] }
| valueList COMMA value   { $1 @ [$3] }
;