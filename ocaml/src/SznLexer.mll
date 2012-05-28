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
 | ['0'-'9']+       { INT_LITERAL( int_of_string(lexeme lexbuf)) }
 | "true"           { TRUE }
 | "false"          { FALSE }
 
 | '-'              { SUB }
 | '='              { EQ }
 
 | '('              { LPAREN }
 | ')'              { RPAREN }
 | '{'              { LCURLY }
 | '}'              { RCURLY }
 | '['              { LSQUARE }
 | ']'              { RSQUARE }
 
 | '.'              { DOT }
 | ','              { COMMA }

 
 | ['a'-'z' 'A'-'Z' '0'-'9']['a'-'z' 'A'-'Z' '0'-'9' '_']* { ID(Lexing.lexeme_start_p lexbuf, lexeme lexbuf) }
 | ';'              { SEMICOLON }
 | eof              { EOF }