open ConfSolve

let translateType (t: _type) =
  match t with
  | T_Int -> "int"
  | T_Bool -> "bool"
  | T_Range(m,n) -> string_of_int m ^ ".." ^ string_of_int n
  | T_Class(c) -> "1.." ^ "COUNT(t)"
  | T_Ref(r) -> "var 1.." ^ "COUNT(t)"

let translateExpression (x) =
  "TODO"

let translateVariable (v: varDecl) =
  let name = fst(v) in
  let typ = snd(v) in
  match typ with
  (* ...set *)
  | T_Class(c) -> translateType(typ) ^ ": " ^ name ^ " = " ^ "NEW_INDEX" ^ ";\n"
  | _ -> translateType(typ) ^ ": " ^ name ^ ";\n"

let translateClass (c: classDecl) =
  List.fold_left (fun mz m ->
    match m with
    | M_Var(name,T_Class(c)) ->
       mz ^ "array[1.." ^ "COUNT(t)" ^ "] of " ^ translateType(T_Class(c)) ^ ": " ^ name ^ " = "
        ^ "[" ^ "NEW_INDEXES" ^ "];\n"
    | M_Var(name,typ) ->
        mz ^ "array[1.." ^ "COUNT(t)" ^ "] of " ^ translateType(typ) ^ ": " ^ name ^ ";\n"
    | _ -> mz) "" c.members

let translateConstraint(c: _constraint) =
  "TODO"

let toMiniZinc (m: ConfSolve.model) =
  List.iter (fun d ->
    let msg : string =
      match d with
      | G_Class(x) -> translateClass(x)
      | G_Var(x) -> translateVariable(x)
      | G_Constraint(x) -> translateConstraint(x) in
    Printf.printf "%s" msg) m.declarations;
    
  "not implemented, ok!"