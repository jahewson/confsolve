{
open Parser
open Lexing

let incr_lineno lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }
    
let lineno lexbuf =
  (Lexing.lexeme_start_p lexbuf).pos_lnum
}

rule token = parse
   [' ' '\t' '\r']  { token lexbuf }     (* skip whitespace *)
 | "\n"             { incr_lineno lexbuf; token lexbuf }     (* skip whitespace *)
 | "/*"             { comment lexbuf }
 | "//"             { line_comment lexbuf }
 | ['0'-'9']+       { INT_LITERAL( int_of_string(lexeme lexbuf)) }
 | "true"           { TRUE }
 | "false"          { FALSE }
 
 | '+'              { ADD }
 | '-'              { SUB }
 | '*'              { MUL }
 | '/'              { DIV }
 | "mod"            { MOD }
 
 | '=' | "is"       { EQ }
 | "!=" | "is not"  { NEQ }
 | '>'              { GT }
 | ">="             { GE }
 | '<'              { LT }
 | "<="             { LE }
 
 | "subset"         { SUBSET }
 | "union"          { UNION }
 | "intersection"   { INTERSECTION }
 
 | '!' | "not"      { NOT }
 | "&&" | "and"     { AND }
 | "||" | "or"      { OR }
 | "->"             { IMPLIES }
 | "<->" | "iff"    { IFF }
 
 | "sum"            { SUM }
 | "forall" | "for" { FORALL }
 | "exists"         { EXISTS }
 | "count"          { COUNT }
 | "in"             { IN }
 | "where"          { WHERE }
 
 | "bool2int"       { BOOL2INT }
 
 | '('              { LPAREN }
 | ')'              { RPAREN }
 | '{'              { LCURLY }
 | '}'              { RCURLY }
 | '['              { LSQUARE }
 | ']'              { RSQUARE }
 
 | '.'              { DOT }
 | ','              { COMMA }
 
 | "var"            { VAR }
 | "as"             { AS }
 | "int"            { INT }
 | "bool"           { BOOL }
 | "ref"            { REF }
 | ".."             { DOTS }
 | "class"          { CLASS }
 | "extends"| "inherits" | ":"  { EXTENDS }
 | "abstract"       { ABSTRACT }
 | "enum"           { ENUM }
 
 | "maximize"       { MAXIMIZE }
 | "minimize"       { MINIMIZE }
 
 | ['a'-'z' 'A'-'Z' '0'-'9']['a'-'z' 'A'-'Z' '0'-'9' '_']* { ID(Lexing.lexeme_start_p lexbuf, lexeme lexbuf) }
 | ';'              { SEMICOLON }
 | eof              { EOF }
 
and comment = parse
 | "*/" 
     { token lexbuf }
 | '\n' 
     { incr_lineno lexbuf; comment lexbuf }
 | [^ '\n']
     { comment lexbuf }
 | eof
     { failwith ("Unterminated comment on line " ^ string_of_int (lineno lexbuf)) }

and line_comment = parse
  | '\n'
      { incr_lineno lexbuf; token lexbuf }
  | _
      { line_comment lexbuf }