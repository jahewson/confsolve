%{
open SznSolution
%}

%token LCURLY RCURLY
%token LPAREN RPAREN
%token LSQUARE RSQUARE
%token EQ
%token DOT COMMA
%token SEMICOLON
%token SUB
%token TRUE FALSE
%token <Lexing.position * string> ID
%token <int> INT_LITERAL
%token EOL
%token EOF

/* precedence: low to high*/
%left EQ
%nonassoc UMINUS

%start solution

%type <SznSolution.solution> solution

%%

solution:
 /*assignmentList EOF   { { declarations = $1 } }*/
 EOF  { Todo 0 }
;

/*assignmentList:
  assignment                   { $1 :: [] }
| assignmentList assignment   { $1 @ [$2] }
;

assignment:
  todo SEMICOLON { $1 }
;

todo:
  INT_LITERAL { $1 }*/