open Lexing;;
open ConfSolve;;
open Forward;;
open SznParser;;
open SznSolution;;
open CSON;;
open Blocks;;
open Printf;;

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

let main () =
  (* parse command-line arguments imperatively *)
  let (csmFilename, sznFilename, paramFilename, isDebug) = 
      ((ref ""), (ref ""), (ref ""), (ref false)) 
  in
  let arglist = [
    ("-p", Arg.Set_string paramFilename, "filename.cson  Paramaters");
    ("--debug", Arg.Set isDebug, "print debugging info")] 
  in
  let msg = "usage: filename.csm filename.szn [options]" in
  let _ = (Arg.parse arglist (fun s ->
      if String.length !csmFilename = 0 then
        csmFilename := s
      else if String.length !sznFilename = 0 then
        sznFilename := s
      else
        raise (Arg.Bad (sprintf "unexpected argument '%s'" s))
      ) msg)
  in
  if String.length !csmFilename = 0 || String.length !sznFilename = 0 then
    (Arg.usage arglist msg;
    exit 1)
  else
    (* parse the ConfSolve model *)
    let lexbuf = Lexing.from_channel (open_in !csmFilename) in
    let ast =
      try Parser.model Lexer.token lexbuf
      with Parsing.Parse_error -> raiseSyntaxError lexbuf !csmFilename
    in
    
    (* resolve forward declarations *)
    let ast = resolveForwardDecls ast in
    
    (* parse paramaters cson *)
    let params = tryParseCson !paramFilename in
    
    (* build name map *)
    let paths =
      match params with
      | Some _ -> Some (CSON.buildNameMap ast)
      | _ -> None
    in
    
    (* TODO: decompose set literals ? *)
    
    (* we don't care about constraints so just use `init` blocks *)
    let ast = applyBlocks ast Init in
    
    (* parse the FlatZinc solution *)
    let lexbuf = Lexing.from_channel (open_in !sznFilename) in
    let solutions =
      try SznParser.solutions SznLexer.token lexbuf
      with Parsing.Parse_error -> raiseSyntaxError lexbuf !sznFilename
    in
    
    (* extract last solution *)
    let sln = List.nth solutions ((List.length solutions) - 1) in
    
    (* generae CSON *)
    let mz = toCSON ast sln params paths !isDebug in
    print_endline mz
  ;;
 
Printexc.record_backtrace true;;     
main();;