open Lexing;;
open ConfSolve;;
open Forward;;
open SznParser;;
open SznSolution;;
open CSON;;
open Printf;;

let main () =
  (* parse command-line arguments imperatively *)
  let (csmFilename, sznFilename, isDebug) = 
      ((ref ""), (ref ""), (ref false)) 
  in
  let arglist = [
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
      try
        Parser.model Lexer.token lexbuf
      with
      | Parsing.Parse_error ->
        let tok = Lexing.lexeme lexbuf in
        let curr = lexbuf.lex_curr_p in
        let cnum = curr.pos_cnum - curr.pos_bol in
        let line = curr.pos_lnum in
        print_endline ("File \"" ^ !csmFilename ^ "\", line " ^ string_of_int line ^ " character " ^ string_of_int cnum ^
                       ":\nError: Syntax error at `" ^ tok ^ "`");
        exit 1
      in
        let ast = resolveForwardDecls ast in
        
        (* parse the FlatZinc solution *)
        let lexbuf = Lexing.from_channel (open_in !sznFilename) in
        
        let solutions =
          try
            SznParser.solutions SznLexer.token lexbuf
          with
          | Parsing.Parse_error ->
            let tok = Lexing.lexeme lexbuf in
            let curr = lexbuf.lex_curr_p in
            let cnum = curr.pos_cnum - curr.pos_bol in
            let line = curr.pos_lnum in
            print_endline ("File \"" ^ !sznFilename ^ "\", line " ^ string_of_int line ^ " character " ^ string_of_int cnum ^
                           ":\nError: Syntax error at `" ^ tok ^ "`");
            exit 1
        in
        
        (* extract last solution *)
        let sln = List.nth solutions ((List.length solutions) - 1) in
        
        (* generae CSON *)
        let mz = toCSON ast sln !isDebug in
        print_endline mz
      ;;
 
Printexc.record_backtrace true;;     
main();;