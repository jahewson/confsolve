open ConfSolve
open State
open Binding
open Counting
open Forward

module StrMap = Map.Make(String)

exception UnexpectedError
exception NotImplemented of string

(* translation ********************************************************************)

(* check if a range is contiguous *)
let isContiguous list =
  let (_,_,is) =
    List.fold_left (fun (prev, idx, is) elem ->
      if idx = 0 then
        (elem, 1, true)
      else
        (elem, idx + 1, is && elem = prev + 1)
    ) (0, 0, true) list
  in is

(* integer list to MiniZinc array literal,
   with collapsing to a range if contiguous *)
let intListToMz list =
  if isContiguous list then
    string_of_int (List.hd list) ^ ".." ^ string_of_int (List.nth list ((List.length list) - 1))
  else
    "{" ^
      List.fold_left (fun mzn elem ->
        if String.length mzn = 0 then
          string_of_int elem
        else
          mzn ^ "," ^ string_of_int elem
      ) "" list
    ^ "}"

(* translates a type *)
let rec translateType t state =
  match t with
  | T_Symbol sym -> raise UnexpectedError
  | T_Int -> "int"
  | T_Bool -> "bool"
  | T_Range (m, n) -> string_of_int m ^ ".." ^ string_of_int n
  | T_Class cname ->
      let cls = (resolveClass cname state) in
      if cls.isAbstract then raise (AbstractInstance (cls.name)) else ();
      if (count cname state) = 0 then raise (NoInstancesOfClass cname) else ();
      "1.." ^ string_of_int (count cname state)
  | T_Enum ename ->
      "1.." ^ string_of_int (List.length (resolveEnum ename state).elements)
  | T_Ref cname ->
      ignore (resolveClass cname state);
      intListToMz (getSubclassIds cname state)
  | T_Set (t, lbound, ubound) ->
      "set of " ^ translateType t state

(* list to MiniZinc array literal *)
let listToMz list =
  List.fold_left (fun mzn elem ->
    if String.length mzn = 0 then
      elem
    else
      mzn ^ "," ^ elem
  ) "" list
        
(* records variables for MiniZinc `output` section *)
let output mz_vname state =
  { state with mzn_output = ("\n  show(" ^ (mz_vname) ^ ")") :: state.mzn_output }
       
 (* translates a binary operator *)
let translateOp op =
  match op with
  | Eq -> "=" | Neq -> "!=" | Gt -> ">" | Ge -> ">=" | Lt -> "<" | Le -> "<="
  | Intersection -> "intersection" | Union -> "union" | Subset -> "subset" | In -> "in"
  | And -> "/\\" | Or -> "\\/" | Implies -> "->" | Iff -> "<->"
  | Add -> "+" | Sub -> "-" | Div -> "div" | Mul -> "*" | Pow -> raise UnexpectedError | Mod -> "mod"

(* translates an expression *)
let rec translateExpr expr state =
  match expr with
  | E_Symbol sym -> raise UnexpectedError
  
  | E_Var vname ->
      ignore (resolveVar vname state);
      (match state.scope.node with
       | S_Global -> vname
       | S_Class cls ->
           let (_, clsDecl) = resolveMemberVar cls vname state in
           (match clsDecl with
           | None -> vname
           | Some clsDecl ->
             if vname = "this" then
              "this"
             else
               clsDecl.name ^ "_" ^ vname ^ "[this]")
       | S_Expr e -> vname)

  | E_Access (E_Enum ename, mname) ->
      let enm = resolveEnum ename state in
      let (_, idx) = 
        List.fold_left (fun (i, found) elem ->
          if elem = mname then
            (i + 1, i)
          else
            (i + 1, found)
        ) (1, -1) enm.elements
      in
      if idx = -1 then
        raise (EnumMemberNotDefined (ename, mname))
      else
        string_of_int idx
      
  | E_Enum _ ->
      ""
  | E_Access (e, mname) ->
      let (cls, (_,mbr), clsDecl) = resolveFieldAccess e mname state in
      cls.name ^ "_" ^ mname ^ "[" ^ translateExpr e state ^ "]"

  | E_Fold (op, name, collection, where, body) ->
      let bodyState = pushScope (S_Expr expr) state in
      ignore (resolveVar name bodyState);
      (match typeof collection state with
      | T_Set (T_Set (_,_,_),_,_) -> raise (SetOfSet name)
      | T_Set (t, lbound, ubound) ->
          let isConst =
            match t with
            | T_Class cname -> ubound = lbound
            | _ -> false
          in
          let mzBody = 
            let guard =
              if isConst then
                E_Bool true
              else
                (E_Op (E_Var name, In, collection))
            in
            let guard =
              match (guard, where) with
              | (E_Bool true, E_Bool true) -> E_Bool true
              | (E_Bool true, _) -> where
              | (_, E_Bool true) -> guard
              | (_, _) -> (E_Op (guard, And, where))
            in
            if guard <> E_Bool true then
              match op with
              | ForAll | Exists -> translateExpr guard bodyState ^ " -> " ^ translateExpr (E_Paren body) bodyState
              | Sum -> "bool2int(" ^ translateExpr guard bodyState ^ ") * " ^ translateExpr (E_Paren body) bodyState
            else
              translateExpr (E_Paren body) bodyState
          in
          let mznRange =
            match t with
            | T_Class cname ->
              if isConst then
                translateExpr collection bodyState
              else
                "1.." ^ string_of_int (count cname state)
            | T_Ref cname -> "1.." ^ string_of_int (count cname state)
            | _ -> string_of_int lbound ^ ".." ^ string_of_int ubound
          in
          (match op with | ForAll -> "forall" | Exists -> "exists" | Sum -> "sum")
          ^ " (" ^ name ^ " in " ^ mznRange ^ ") (\n"
          ^ "    " ^ mzBody ^ "\n"
          ^ "  )"
      | _ -> raise (ExpectedSet1 name)) 

  | E_Op (e1, Pow, e2) -> "pow (" ^ translateExpr e1 state ^ "," ^ translateExpr e2 state ^ ")"
  | E_Op (e1, op, e2) -> translateExpr e1 state ^ " " ^ translateOp op ^ " " ^ translateExpr e2 state
  | E_Card e -> "card(" ^ translateExpr e state ^ ")"
  | E_Neg e -> "- " ^ translateExpr e state
  | E_Not e -> "not (" ^ translateExpr e state ^ ")"
  | E_Bool b -> string_of_bool b
  | E_Int i -> string_of_int i
  | E_BoolToInt e -> "bool2int(" ^ translateExpr e state ^ ")"
  | E_Paren e -> "(" ^ translateExpr e state ^ ")"
  
(* creates a cardinality constraint for a set *)
let cardConstraint vname lbound ubound state =
  if lbound = ubound then
    E_Op (E_Card (E_Var vname), Eq, E_Int lbound)
  else
    let elb = E_Op (E_Card (E_Var vname), Ge, E_Int lbound) in
    let eub = E_Op (E_Card (E_Var vname), Le, E_Int ubound) in
    E_Op (elb, And, eub)
    
(* translates a class-member variable *)
let translateMemberVar cls var state =
  let (vname, t) = var in
  let state = output (cls.name  ^ "_" ^ vname) state in
  let ccount = count cls.name state in
  let mzn = if state.comments then "\n% ." ^ vname ^ " as " ^ typeToString t ^ "\n" else "" in
  let mzn = mzn ^ "array[1.." ^ string_of_int ccount ^ "] of " in
  match t with
  | T_Class cname ->
      let (indices, state) = newIndices cname ccount state in
      let mzn = mzn ^ translateType t state ^ ": " ^ cls.name ^ "_" ^ vname ^ " = [" ^ listToMz indices ^ "];\n" in
      (mzn, state)
  | T_Set (T_Class cname, lbound, ubound) ->
      if lbound <> ubound then
        raise (NotImplemented "variable cardinality sets of objects")
      else
        let (indices, state) =
          (List.fold_left (fun (arr, state) elem ->
            let (indices, state) = newIndices cname ubound state in
            let set = "{" ^ (listToMz indices) ^ "}" in
            (arr @ [set], state)
          ) ([], state) (seq 1 ccount))
        in
        let mzn = mzn ^ translateType t state ^ ": " ^ cls.name ^ "_" ^ vname ^ " = [" ^ listToMz indices ^ "];\n" in
        (mzn, state)
  | T_Set (_, lbound, ubound) ->
      let mzn = mzn ^ "var " ^ translateType t state ^ ": " ^ cls.name ^ "_" ^ vname ^ ";\n" in
      let expr = cardConstraint vname lbound ubound state in
      let mzn = mzn ^ "constraint forall (this in 1.." ^ string_of_int ccount ^ ") (" ^ translateExpr expr state ^ ");\n" in
      (mzn, state)
  | _ ->
      let mzn = mzn ^ "var " ^ translateType t state ^ ": " ^ cls.name ^ "_" ^ vname ^ ";\n" in
      (mzn, state)
  
(* translates a global variable *)
let rec translateGlobalVar var state =
  let (vname, t) = var in
  let state = output vname state in
  let mzn = if state.comments then "\n% " ^ vname ^ " as " ^ typeToString t ^ "\n" else "" in
  match t with
  | T_Class cname ->
      let (idx, state) = newIndex cname state in
      let mzn = mzn  ^ translateType t state ^ ": " ^ vname ^ " = " ^ idx ^ ";\n" in
      (mzn, state)
  | T_Set (T_Class cname, lbound, ubound) ->
      if lbound <> ubound then
        raise (NotImplemented "variable cardinality sets of objects")
      else
        let (indices, state) = newIndices cname ubound state in
        let mzn = mzn ^ translateType t state ^ ": " ^ vname in
        let mzn = mzn ^ " = {" ^ listToMz indices ^ "};\n" in
        (mzn, state)
  | T_Set (_, lbound, ubound) ->
      let mzn = mzn ^ "var " ^ translateType t state ^ ": " ^ vname ^ ";\n" in
      let expr = cardConstraint vname lbound ubound state in
      let mzn = mzn ^  "constraint " ^ translateExpr expr state ^ ";\n" in
      (mzn, state)
  | _ ->
      let mzn = mzn ^ "var " ^ translateType t state ^ ": " ^ vname ^ ";\n" in
      (mzn, state)
    
(* translates a class-level constraint *)    
let translateClassConstraint cls con state =
  let mzIds = intListToMz (getSubclassIds cls.name state) in
  match con with
  | C_Where expr ->
      let mzn =
        "\nconstraint\n  forall (this in " ^ mzIds ^ ") (\n    " ^ translateExpr expr state ^ "\n  );\n"
      in (mzn, state)
  | C_Maximise expr ->
      let state = { state with maximise_count = state.maximise_count + 1 } in
      let mzn =
        "\nvar int: objective_" ^ string_of_int state.maximise_count ^ ";\n" ^
        "constraint\n  objective_" ^ string_of_int state.maximise_count ^ 
            " = sum (i in " ^ mzIds ^ ") (\n    " ^ translateExpr expr state ^ "\n  );\n\n"
      in (mzn, state)
    
(* translates a global constraint *)
let translateGlobalConstraint con state =
  match con with
  | C_Where expr ->
      let mzn =
        (if state.comments then "\n% global" else "") ^
        "\nconstraint\n  " ^ translateExpr expr state ^ ";\n"
      in (mzn, state)
  | C_Maximise expr ->
      let state = { state with maximise_count = state.maximise_count + 1 } in
      let mzn =
        (if state.comments then "\n% global" else "") ^
        "\nvar int: objective_" ^ string_of_int state.maximise_count ^ ";\n" ^
        "constraint objective_" ^ string_of_int state.maximise_count ^ " = " ^ translateExpr expr state ^ ";\n\n"
      in (mzn, state)

(* raise error on non-abstract inheritance *)
let checkForAbstractInheritance cls state =
  match cls.super with
  | Some cname ->
      let super = (resolveClass cname state) in
      if not super.isAbstract then
        raise (NotImplemented ("`" ^ cls.name ^ "` inherits non-abstract class `" ^ cname ^ "`"))
      else ()
  | None -> ()

(* translates a class *)
let translateClass cls state =
  if rawCount cls.name state = 0 then
    ("", state)
  else (
    checkForAbstractInheritance cls state;
    let state = pushScope (S_Class cls) state in
    let mzn = if state.comments then "\n%\n% class " ^ cls.name ^ "\n%\n" else ""
    in
    let (mzn, state) = 
      List.fold_left (fun (mzn, state) mbr ->
        let (mzn', state) =
          match mbr with
          | Var var -> translateMemberVar cls var state
          | Constraint con -> translateClassConstraint cls con state
          | Enum _ | Class _ -> raise UnexpectedError in
        (mzn  ^ mzn', state)
      ) (mzn, state) (allMembers cls state)
    in
    (mzn, popScope state)
  )

let translateEnum enm state =
  ("", state)

(* translates the entire model *)
let translateModel state =
  List.fold_left (fun (mzn, state) decl ->
    let (mzn', state) =
      match decl with
      | Var var -> translateGlobalVar var state
      | Class cls -> translateClass cls state
      | Enum enm -> translateEnum enm state
      | Constraint con -> translateGlobalConstraint con state in
    (mzn  ^ mzn', state)
  ) ("", state) state.model.declarations
    
(* entry point ********************************************************************)

(* translates the model into a string of MiniZinc *)
let toMiniZinc csModel showCounting hasComments =
  (* init *)
  let scope = { parent = None; node = S_Global} in
  let state = { counts = StrMap.empty; indexes = StrMap.empty; model = csModel; 
                scope = scope; subclasses = StrMap.empty; show_counting = showCounting; 
                mzn_output = []; comments = hasComments; maximise_count = 0 } in
                
  (* 1st pass: count objects *)
  let state = countModel state in
  if showCounting then
    (printCounts state;
    "")
  else
    (* 2nd pass: translate to MiniZinc *)
    let (mzn, state) = translateModel state in
    let state =
      if state.maximise_count > 0 then
        output "total_objective" state
      else state
    in
    let mzn =
      mzn ^
      if state.maximise_count > 0 then
        "var int: total_objective;\n" ^
        "constraint total_objective = " ^
        List.fold_left (fun acc elem ->
          let vname = "objective_" ^ string_of_int elem in
          if String.length acc = 0 then
            vname
          else
            acc ^ " + " ^ vname
        ) "" (seq 1 state.maximise_count)
        ^ ";\n"
      else ""
    in
    let solve =
      if state.maximise_count = 0 then
        "satisfy" 
      else
        "maximize total_objective"
    in
    mzn ^ "\nsolve " ^ solve ^ ";\n\n"
        ^ "output ["
        ^ (listToMz state.mzn_output)
        ^ "];"