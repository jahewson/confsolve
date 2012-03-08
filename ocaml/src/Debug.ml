open ConfSolve

let typeToString (t: _type) =
  match t with
  | T_Int -> "T_Int"
  | T_Bool -> "T_Bool"
  | T_Range(m,n) -> "T_Range " ^ string_of_int m ^ ".." ^ string_of_int n
  | T_Class(c) -> "T_Class " ^ c
  | T_Ref(r) -> "T_Ref " ^ r

let print (model: ConfSolve.model) =
  print_string "{declarations = [";
  ignore (List.fold_left (fun count decl ->
    if count > 0 then print_string "; ";
    (match decl with
    | G_Class(_) -> print_string "CLASS"
    | G_Var(n,t) -> print_string ("G_Var (\"" ^ n ^ "\", " ^ typeToString(t) ^ ")")
    | G_Constraint(_) -> print_string "CONSTRAINT");
    count + 1) 0 model.declarations);
  print_string "];}"

let printn (model: ConfSolve.model) =
  print model;
  print_string "\n"