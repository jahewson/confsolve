open ConfSolve
open Parser
open Util

let rec typeToString (t: _type) =
  match t with
  | T_Infer -> "T_Infer"
  | T_Symbol s -> "T_Symbol " ^ s
  | T_Int -> "T_Int"
  | T_Bool -> "T_Bool"
  | T_BInt set ->
      "{" ^ (IntSet.fold (fun elem acc  ->
          if String.length acc = 0 then 
            string_of_int elem
          else
            acc ^ "," ^ string_of_int elem
        ) set "") ^ "}"
  | T_Class(c) -> "T_Class " ^ c
  | T_Ref(r) -> "T_Ref " ^ r
  | T_Set(t,lbound,ubound) -> "T_Set " ^ typeToString t ^ "[" ^ string_of_int lbound ^ "," ^ string_of_int ubound ^ "]"
  | T_Enum(e) -> "T_Enum " ^ e

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
    | Var(n,t) -> print_string ("Var (\"" ^ n ^ "\", " ^ typeToString(t) ^ ")")
    | Constraint(_) -> print_string "CONSTRAINT"
    | _ -> print_string "TODO");
    count + 1) 0 c.members);
  print_string "];}"

let opToString op =
  match op with
  | Eq -> "Eq"
  | Neq -> "Neq"
  | Gt -> "Gt"
  | Ge -> "Ge"
  | Lt -> "Lt"
  | Le -> "Le"
  | In -> "In"
  | Subset -> "Subset"
  | Union -> "Union"
  | Intersection -> "Intersection"
  | And -> "And"
  | Or -> "Or"
  | Implies -> "Implies"
  | Iff -> "Iff"
  | Add -> "Add"
  | Sub -> "Sub"
  | Div -> "Div"
  | Mul -> "Mul"
  | Pow -> "Pow"
  | Mod -> "Mod"

let rec exprToString (expr : ConfSolve.expr) =
  match expr with
  | E_Symbol id -> "(E_Symbol " ^ id ^ ")"
  | E_Var (id, _, _) -> "(E_Var" ^ id ^ ")"
  | E_Enum id -> "(E_Enum " ^ id ^ ")"
  | E_Access (e, id) -> "(E_Access " ^  exprToString e ^ " " ^ id ^ ")"
  | E_Op (e1, op, e2) -> "(E_Op " ^  exprToString e1 ^ " " ^ opToString op ^ " " ^ exprToString e2 ^ ")"
  | E_Card e -> "(E_Card " ^ exprToString e ^ ")"
  | E_Fold (op, (varName, _type), e1, e2, e3) ->
      "(E_Fold " ^ varName ^ " " ^ exprToString e1 ^ " " ^ exprToString e2 ^ " " ^ exprToString e3 ^ ")"
  | E_Neg e -> "(E_Neg " ^ exprToString e ^ ")"
  | E_Not e -> "(E_Not " ^ exprToString e ^ ")"
  | E_Old e -> "(E_Old " ^ exprToString e ^ ")"
  | E_Bool b -> string_of_bool b
  | E_Int i -> string_of_int i
  | E_Set elist ->
      List.fold_left (fun acc e ->
        let acc = (if acc = "" then "" else acc ^ " ") in
        acc ^ exprToString e
      ) "" elist
  | E_Paren e -> "(E_Paren " ^ exprToString e ^ ")"
  | E_BoolToInt e -> "(E_BoolToInt " ^ exprToString e ^ ")"
  | E_Abs e -> "(E_Abs " ^ exprToString e ^ ")"
  
let constraintToString con =
  match con with
  | C_Where e -> "Where " ^ exprToString e
  | C_Maximise e -> "Maximise " ^ exprToString e
  
let printAst (model: ConfSolve.model) =
  print_string "{declarations =\n  [";
  ignore (List.fold_left (fun count decl ->
    if count > 0 then print_string ";\n   ";
    (match decl with
    | Class(c) -> print_string "Class "; printClass c
    | Var(n,t) -> print_string ("Var (\"" ^ n ^ "\", " ^ typeToString(t) ^ ")")
    | Param(n,t) -> print_string ("Param (\"" ^ n ^ "\", " ^ typeToString(t) ^ ")")
    | Constraint(con) -> print_string (constraintToString con)
    | Enum(en) -> print_string ("Enum ...")
    | Block(kind,cons) ->
        print_string ("BLOCK (" ^ (List.fold_left (fun acc con ->
         acc ^ constraintToString con
        ) "" cons) ^ ")"));
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