{
open SznParser
open Lexing

let incr_lineno lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }
}

rule token = parse
   [' ' '\t' '\r']  { token lexbuf }                         (* skip whitespace *)
 | "\n"             { incr_lineno lexbuf; token lexbuf }     (* skip whitespace *)
 | "%"              { line_comment lexbuf }
 | "----------"     { SOL_END }
 | "=========="     { ALL_SOL_END }
 | ['0'-'9' '-']+   { INT_LITERAL(int_of_string (lexeme lexbuf)) }
 | "true"           { TRUE }
 | "false"          { FALSE }
 
 | "array1d"        { ARRAY1D }
 
 | '='              { EQ }
 
 | '('              { LPAREN }
 | ')'              { RPAREN }
 | '{'              { LCURLY }
 | '}'              { RCURLY }
 | '['              { LSQUARE }
 | ']'              { RSQUARE }
 
 | ".."             { DOTS }
 | ','              { COMMA }

 | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* { ID (lexeme lexbuf) }
 | ';'              { SEMICOLON }
 | eof              { EOF }
 
and line_comment = parse
 | '\n'
     { incr_lineno lexbuf; token lexbuf }
 | _
     { line_comment lexbuf }