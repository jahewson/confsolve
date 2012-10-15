open Lexing;;
open Lexer;;
open Parser;;
open ConfSolve;;
open Forward;;
open Literals;;
open MiniZinc;;
open TypeCheck;;
open Blocks;;
open Debug;;
open Printf;;

module StrMap = Map.Make(String);;

(* for debugging *)
let rec printTokens lexbuf =
  let t = Lexer.token lexbuf in
  match t with
  | EOF -> print_string("EOF\n")
  | _ -> Debug.printToken t; printTokens lexbuf
  
(* fail with a syntax error *)
let raiseSyntaxError lexbuf filename =
  let tok = Lexing.lexeme lexbuf in
  let curr = lexbuf.lex_curr_p in
  let cnum = curr.pos_cnum - curr.pos_bol in
  let line = curr.pos_lnum in
  print_endline ("File \"" ^ filename ^ "\", line " ^ string_of_int line ^
                 " character " ^ string_of_int cnum ^
                 ":\nError: Syntax error at `" ^ tok ^ "`");
  exit 1

(* parse a CSON file if it exists *)
let tryParseCson filename =
  if String.length filename = 0 then
    None
  else
    let lexbuf = Lexing.from_channel (open_in filename) in
    try Some (CsonParser.solution CsonLexer.token lexbuf)
    with Parsing.Parse_error -> raiseSyntaxError lexbuf filename
  
(* parses command-line args imperatively *)
let main () =
  let (filename, paramFilename, solnFilename, showTokens, showAst, showCounting, hasComments, showVersion, minChanges) = 
      ((ref ""), (ref ""), (ref ""), (ref false), (ref false), (ref false), (ref false), (ref false), (ref false)) 
  in
  let arglist = [
    ("-c", Arg.Set hasComments, " Comment generated MiniZinc");
    ("-m", Arg.Set minChanges, " Use the min-changes heuristic");
    ("-p", Arg.Set_string paramFilename, "filename.cson  Paramaters");
    ("-s", Arg.Set_string solnFilename, "filename.cson  Solution to re-configure");
    ("-v", Arg.Set showVersion, " Print version");
    ("--debug-tokens", Arg.Set showTokens, " Print lexer tokens (debug)");
    ("--debug-ast", Arg.Set showAst, " Print AST (debug)");
    ("--debug-counting", Arg.Set showCounting, " Print object count (debug)")] 
  in
    let msg = "usage: filename.csm [options]" in
  let _ = (Arg.parse arglist (fun s ->
      if String.length !filename = 0 then
        filename := s
      else
        raise (Arg.Bad (sprintf "unexpected argument '%s'" s))
      ) msg)
  in
  if !showVersion then
    (print_endline "(C) 2011-2012 The University Court of the University of Edinburgh";
     print_endline ("Version 0.6 @ " ^ Version.sha);
    exit 0)
  else
    if String.length !filename = 0 then
      (Arg.usage arglist msg;
      exit 1)
    else
      (* lex model *)
      let lexbuf = Lexing.from_channel (open_in !filename) in
      if !showTokens then
        printTokens lexbuf
      else
        (* parse model *)
        let ast =
          try Parser.model Lexer.token lexbuf
          with Parsing.Parse_error -> raiseSyntaxError lexbuf !filename
        in
        
        (* parse paramaters cson *)
        let params = tryParseCson !paramFilename in
        
        (* parse previous solution cson *)
        let soln = tryParseCson !solnFilename in
        
        (* apply conditional blocks *)
        let ast =
          match soln with
          | None -> applyBlocks ast Init
          | _ -> applyBlocks ast Change
        in
        
        (* process *)
        if !showAst then
          Debug.printAst ast
        else
          (* resolve forward declarations *)
          let ast = resolveForwardDecls ast in
          
          (* type check *)
          typeCheck ast;
          
          (* build name map *)
          let paths =
            match (soln, params) with
            | (Some _, _) | (_, Some _) -> Some (CSON.buildNameMap ast)
            | _ -> None
          in
          
          (* apply min-changes *)
          let ast =
            if !minChanges then
              match (soln, paths) with
              | (Some s, Some p) -> applyMinChanges ast s p
              | (_, _) -> print_endline "Error: use of -m requires -s also"; exit 1
            else ast
          in
          
          (* decompose set literals *)
          let ast = decomposeLiterals ast in
          
          (* generate MiniZinc *)
          let mz = toMiniZinc ast soln params paths !showCounting !hasComments in
          print_endline mz
        ;;
      
Printexc.record_backtrace true;;
main();;