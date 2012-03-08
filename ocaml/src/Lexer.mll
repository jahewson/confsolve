(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
open Lexing
}
rule token = parse
   [' ' '\t']     { token lexbuf }     (* skip blanks *)
 | ['\n' ]        { EOL }
 | ['0'-'9']+ { INT_LITERAL(int_of_string(lexeme lexbuf)) }
 | '+'            { PLUS }
 | '-'            { MINUS }
 | '*'            { TIMES }
 | '/'            { DIV }
 | '('            { LPAREN }
 | ')'            { RPAREN }
 | "var"          { VAR }
 | "as"           { AS }
 | "int"          { INT }
 | "bool"         { BOOL }
 | ['a'-'z' 'A'-'Z' '0'-'9']['a'-'z' 'A'-'Z' '0'-'9' '_']* { IDENTIFIER(lexeme lexbuf) }
 | ';'            { SEMICOLON }
 | eof            { EOF }