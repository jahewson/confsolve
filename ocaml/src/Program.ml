open Lexing;;
open Lexer;;
open Parser;;
open ConfSolve;;
open Semantics;;
open Debug;;

let main () =
  let c = G_Class { name="C"; super=None; members=[M_Var ("V2", T_Int); M_Var ("V3", T_Class("C2"))] } in
  let v = G_Var ("V", T_Bool) in
  let v4 = G_Var ("V4", T_Class("C2")) in
  let example = { declarations=[c; v; v4]  } in
  Debug.printn example;
  let mz = Semantics.toMiniZinc(example) in
  print_endline mz;;
  
let rec printTokens (lexbuf) = 
  let tkn = Lexer.token lexbuf in
  match tkn with
  | VAR -> print_string "VAR\n"; printTokens lexbuf
  | AS -> print_string "AS\n"; printTokens lexbuf
  | INT -> print_string "INT\n"; printTokens lexbuf
  | IDENTIFIER(id) -> print_string("IDENTIFIER \"" ^ id ^ "\"\n"); printTokens lexbuf
  | EOF -> print_string("EOF\n")
  | SEMICOLON -> print_string("SEMICOLON\n"); printTokens lexbuf
  | _ -> print_string "todo...\n"; printTokens lexbuf
      
let new_parse () =
  let code = "var x as int; var y as int" in
  
  (* tokens *)
  let lexbuf = Lexing.from_string code in
  printTokens lexbuf;

  print_string "-------------------\n";
  
  (* parse *)
  let lexbuf2 = Lexing.from_string code in
  let model = Parser.model Lexer.token lexbuf2 in
  Debug.printn model;
  
  print_string "-------------------\n";
  
  (* semantics *)
  let mz = Semantics.toMiniZinc model in
  print_endline mz;;
  
(* main();; *)
new_parse();;