open Lexing;;
open Lexer;;
open Parser;;
open ConfSolve;;
open Semantics;;
open Debug;;
open Printf;;

module StrMap = Map.Make(String);;

(* for debugging *)
let rec printTokens (lexbuf) =
  let t = Lexer.token lexbuf in
  match t with
  | EOF -> print_string("EOF\n")
  | _ -> Debug.printToken t; printTokens lexbuf
  
(* parses command-line args imperatively *)
let main () =
  let (filename, showTokens, showAst, showCounting, hasComments) = 
      ((ref ""), (ref false), (ref false), (ref false), (ref false)) 
  in
  let arglist = [
    ("-c", Arg.Set hasComments, "comment the MiniZinc");
    ("--debug-tokens", Arg.Set showTokens, "print lexer tokens (debug)");
    ("--debug-ast", Arg.Set showAst, "print AST (debug)");
    ("--debug-counting", Arg.Set showCounting, "print object count (debug)")] 
  in
  let msg = "usage: filename [options]" in
  let _ = (Arg.parse arglist (fun s ->
      if String.length !filename = 0 then
        filename := s
      else
        raise (Arg.Bad (sprintf "unexpected argument '%s'" s))
      ) msg)
  in
  if String.length !filename = 0 then
    (Arg.usage arglist msg;
    exit 1)
  else
    let lexbuf = Lexing.from_channel (open_in !filename) in
    if !showTokens then
      printTokens lexbuf
    else
      try
        let ast = Parser.model Lexer.token lexbuf in
        if !showAst then
          Debug.printAst ast
        else
          let mz = Semantics.toMiniZinc ast !showCounting !hasComments in
          print_endline mz
      with
      | Parsing.Parse_error ->
        let tok = Lexing.lexeme lexbuf in
        let curr = lexbuf.Lexing.lex_curr_p in
        let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
        let line = curr.Lexing.pos_lnum in
        print_endline ("File \"" ^ !filename ^ "\", line " ^ string_of_int line ^ " character " ^ string_of_int cnum ^
                       ":\nError: Syntax error at `" ^ tok ^ "`");
        exit 1
      ;;
      
main();;