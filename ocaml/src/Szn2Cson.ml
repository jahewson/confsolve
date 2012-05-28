open Lexing;;
open SznParser;;
open SznSolution;;
open ConfSolve;;
open Forward;;
open CSON;;
open Debug;;
open Printf;;

let main () =
  (* parse command-line arguments imperatively *)
  let (filename, isDebug) = 
      ((ref ""), (ref false)) 
  in
  let arglist = [
    ("--debug", Arg.Set isDebug, "print debugging info")] 
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
    (* parse the ConfSolve model *)
    let lexbuf = Lexing.from_channel (open_in !filename) in
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
        let ast = resolveForwardDecls ast in
        
        (* parse the FlatZinc solution *)
        let lexbuf = Lexing.from_channel (open_in !filename) in
        let sol = SznParser.solution SznLexer.token lexbuf in
        
        (* generae CSON *)
        let mz = toCSON ast sol !isDebug in
        print_endline mz
      ;;
      
main();;