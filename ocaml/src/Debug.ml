open ConfSolve
open Parser

let rec typeToString (t: _type) =
  match t with
  | T_Int -> "T_Int"
  | T_Bool -> "T_Bool"
  | T_Range(m,n) -> "T_Range " ^ string_of_int m ^ ".." ^ string_of_int n
  | T_Class(c) -> "T_Class " ^ c
  | T_Ref(r) -> "T_Ref " ^ r
  | T_Set(t,lbound,ubound) -> "T_Set " ^ typeToString t ^ "[" ^ string_of_int lbound ^ "," ^ string_of_int ubound ^ "]"

let printClass (c: classDecl) =
  let super =
    match c.super with
    | Some(s) -> "Some \"" ^ s ^ "\""
    | None -> "None"
  in
  Printf.printf "{name = \"%s\";\n            super = %s;\n            members = [" c.name super;
  ignore (List.fold_left (fun count decl ->
    if count > 0 then print_string "; ";
    (match decl with
    | M_Var(n,t) -> print_string ("M_Var (\"" ^ n ^ "\", " ^ typeToString(t) ^ ")")
    | M_Constraint(_) -> print_string "M_CONSTRAINT");
    count + 1) 0 c.members);
  print_string "];}"

let printAst (model: ConfSolve.model) =
  print_string "{declarations =\n  [";
  ignore (List.fold_left (fun count decl ->
    if count > 0 then print_string ";\n   ";
    (match decl with
    | G_Class(c) -> print_string "G_Class "; printClass c
    | G_Var(n,t) -> print_string ("G_Var (\"" ^ n ^ "\", " ^ typeToString(t) ^ ")")
    | G_Constraint(_) -> print_string "G_CONSTRAINT");
    count + 1) 0 model.declarations);
  print_endline "];}"
  
let printToken (tkn: Parser.token) =
  match tkn with
  | LCURLY -> print_endline "LCURLY"
  | RCURLY -> print_endline "LCURLY"
  | VAR -> print_endline "VAR"
  | AS -> print_endline "AS"
  | INT -> print_endline "INT"
  | CLASS -> print_endline "CLASS"
  | EXTENDS -> print_endline "EXTENDS"
  | ABSTRACT -> print_endline "ABSTRACT"
  | WHERE -> print_endline "WHERE"
  | MAXIMIZE -> print_endline "MAXIMIZE"
  | ID(id) -> print_endline("ID \"" ^ snd(id) ^ "\"");
  | SEMICOLON -> print_endline "SEMICOLON"
  | EOF -> print_endline("EOF")
  | _ -> print_endline "todo...";