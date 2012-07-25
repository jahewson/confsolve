open Lexing;;
open Lexer;;
open Parser;;
open ConfSolve;;
open Forward;;
open Literals;;
open MiniZinc;;
open TypeCheck;;
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
  let (filename, showTokens, showAst, showCounting, hasComments, showVersion) = 
      ((ref ""), (ref false), (ref false), (ref false), (ref false), (ref false)) 
  in
  let arglist = [
    ("-c", Arg.Set hasComments, "comment the MiniZinc");
    ("-v", Arg.Set showVersion, "print version");
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
  if !showVersion then
    (print_endline "(C) 2011-2012 The University Court of the University of Edinburgh";
     print_endline ("Version 2.0a @ " ^ Version.sha);
    exit 0)
  else
    if String.length !filename = 0 then
      (Arg.usage arglist msg;
      exit 1)
    else
      let lexbuf = Lexing.from_channel (open_in !filename) in
      if !showTokens then
        printTokens lexbuf
      else
        let ast =
          try
            Parser.model Lexer.token lexbuf
          with
          | Parsing.Parse_error ->
            let tok = Lexing.lexeme lexbuf in
            let curr = lexbuf.lex_curr_p in
            let cnum = curr.pos_cnum - curr.pos_bol in
            let line = curr.pos_lnum in
            print_endline ("File \"" ^ !filename ^ "\", line " ^ string_of_int line ^ " character " ^ string_of_int cnum ^
                           ":\nError: Syntax error at `" ^ tok ^ "`");
            exit 1
          in
            if !showAst then
              Debug.printAst ast
            else
              let ast = resolveForwardDecls ast in
              typeCheck ast;
              let ast = decomposeLiterals ast in
              let mz = toMiniZinc ast !showCounting !hasComments in
              print_endline mz
        ;;
      
Printexc.record_backtrace true;;
main();;