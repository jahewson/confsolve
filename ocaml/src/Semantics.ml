open ConfSolve
module StrMap = Map.Make(String)

exception UnexpectedError
exception NotImplemented of string (* TODO: should be none of these left *)

(* state ************************************************************************)

(* AST nodes with scope *)
type scopedNode =
  | S_Global                        (* global *)
  | S_Class of ConfSolve.classDecl  (* class *)
  | S_Expr of ConfSolve.expr        (* expression, e.g. E_Fold *)

(* scope tree *)
type scope = {
  parent: scope option;   (* enclosing scope *)
  node: scopedNode;       (* AST node *)
}

(* translation state *)
type state = {
  model: ConfSolve.model;         (* ConfSolve AST *)
  counts: int StrMap.t;           (* object counts per-class *)
  subclasses: int list StrMap.t;  (* map of superclass names to subclass indices *)
  indexes: int StrMap.t;          (* current index per-class *)
  scope: scope;                   (* scope tree *)
  mzn_output: string list;        (* MiniZinc `output` variables *)
  show_counting: bool;            (* debugging - print the object counts *)
  comments: bool;                 (* debugging - comment MiniZinc output *)
}

(* binding ************************************************************************)

exception ClassNotDefined of string                 (* class not defined *)
exception VarNotDefined of string                   (* variable not defined *)
exception MemberVarNotDefined of string * string    (* member variable not defined (name, class) *)
exception ExpectedClass of string                   (* expected a class *)
exception ExpectedSet1 of string                    (* expected a set *)
exception ExpectedSet2 of string                    (* expected a set *)
exception SetOfSet of string                        (* sets of sets are not permitted *)
exception InvalidFieldAccess of string * string     (* invalid field access (field, type) *)
exception IncompatibleTypes of string               (* expression of incompatible types *)
exception NoInstancesOfClass of string              (* no instances of class for reference to resolve to *)
exception AbstractInstance of string                (* cannot create an instance of an abstract class *)

(* resolves a class symbol *)
let resolveClass name state =
  try
    let res =
      List.find (fun decl ->
        match decl with
        | G_Class c -> c.name = name
        | _ -> false
      ) state.model.declarations
    in
    match res with
    | G_Class c -> c
    | _ -> raise (ExpectedClass name)
  with
  | Not_found -> raise (ClassNotDefined name)
  
(* resolves a global variable symbol *)
let resolveGlobalVar name state =
  let res = (List.fold_left (fun res decl ->
    match decl with
    | G_Var v ->
      (match res with
       | Some v -> res
       | None -> if fst v = name then Some v else None
      )
    | _ -> res
  ) None state.model.declarations)
  in
  match res with
  | Some v -> v
  | None -> raise (VarNotDefined name)

(* resolves a class-level variable symbol *)
let rec actualResolveMemberVar cls name state =
  let member =
    List.fold_left (fun res decl ->
      match decl with
      | M_Var v ->
        (match res with
         | Some v -> res
         | None -> if fst v = name then Some v else None
        )
      | _ -> res
    ) None cls.members
  in
  match (member, cls.super) with
  | (None, None) 
  | (Some _, _) -> (member, cls)
  | (None, Some cname) ->
      let super = (resolveClass cname state) in
      actualResolveMemberVar super name state
    
(* resolves a class-level variable symbol, only *)
let strictResolveMemberVar cls name state =
  match actualResolveMemberVar cls name state with
  | (Some v, c) -> (v, c)
  | (None, _) -> raise (MemberVarNotDefined (name, cls.name))
    
(* resolves a class-level variable symbol, with global fallback,
   returning both the variable and class which defined it *)
let resolveMemberVar cls name state =
  match actualResolveMemberVar cls name state with
  | (Some v, c) -> (v, c)
  | (None, c) -> (resolveGlobalVar name state, c)
    
(* for reporting compile errors *)
let rec typeToString (t: _type) =
  match t with
  | T_Int -> "int"
  | T_Bool -> "bool"
  | T_Range(m,n) -> string_of_int m ^ ".." ^ string_of_int n
  | T_Class(c) -> c
  | T_Ref(r) -> "ref " ^ r
  | T_Set(t, lbound, ubound) -> typeToString t ^ "[" ^ string_of_int lbound ^ "," ^ string_of_int ubound ^ "]"

(* determines the type of an expression *)
let rec typeof expr state =
  match expr with
  | E_Var vname -> snd (resolveVar vname state)
  | E_Access (e, fname) -> let (_,v,_) = resolveFieldAccess e fname state in snd (v)
  | E_Op (_, Eq, _) -> T_Bool
  | E_Op (_, Neq, _) -> T_Bool
  | E_Op (_, Gt, _) -> T_Bool
  | E_Op (_, Ge, _) -> T_Bool
  | E_Op (_, Lt, _) -> T_Bool
  | E_Op (_, Le, _) -> T_Bool
  | E_Op (_, In, _) -> T_Bool
  | E_Op (_, Subset, _) -> T_Bool
  | E_Op (e1, Union, e2) -> typeOfSetOp e1 Union e2 state
  | E_Op (e1, Intersection, e2) -> typeOfSetOp e1 Intersection e2 state
  | E_Op (_, And, _) -> T_Bool
  | E_Op (_, Or, _) -> T_Bool
  | E_Op (_, Implies, _) -> T_Bool
  | E_Op (_, Iff, _) -> T_Bool
  | E_Op (_, Add, _) -> T_Int
  | E_Op (_, Sub, _) -> T_Int
  | E_Op (_, Div, _) -> T_Int
  | E_Op (_, Mul, _) -> T_Int
  | E_Op (_, Pow, _) -> T_Int
  | E_Op (_, Mod, _) -> T_Int
  | E_Fold (op, _, _, _, _) -> T_Bool
  | E_Neg e -> T_Int
  | E_Not e -> T_Bool
  | E_Bool b -> T_Bool
  | E_Int i -> T_Int
  | E_Card e -> T_Int
  | E_Paren e -> typeof e state

(* return a list of all ancestors of a class, sorted by distance *)
and ancestors cls ancs state =
  let ancs = ancs @ [cls.name] in
  match cls.super with
  | None -> ancs
  | Some cname -> ancestors (resolveClass cname state) ancs state

(* intersect two lists *)
and intersect l1 l2 =
  List.filter (fun v ->
    List.mem v l2
  ) l1
        
(* find the closest common ancestor of two classes *)
and commonAncestor cls1 cls2 state =
  let a1 = ancestors cls1 [] state in
  let a2 = ancestors cls2 [] state in
  let ancs = intersect a1 a2 in
  if List.length ancs = 0 then
    None
  else
    Some (List.hd ancs)
  
(* determines the type of a setexpression *)
and typeOfSetOp e1 op e2 state =
  let t1 = typeof e1 state and t2 = typeof e2 state in
    match (t1, t2) with
    | (T_Set (t1, lb1, ub1), T_Set (t2, lb2, ub2)) ->
        let t = 
          match (t1, t2) with
          | (T_Bool, T_Bool) -> T_Bool
          | (T_Range (m1,n1), T_Range (m2,n2)) ->
              (match op with
               | Intersection -> T_Range (max m1 m2, min n1 n2)
               | Union ->        T_Range (min m1 m2, max n1 n2)
               | _ -> raise UnexpectedError)
          | (T_Ref cname1, T_Ref cname2)
          | (T_Class cname1, T_Class cname2) ->
              (match commonAncestor (resolveClass cname1 state) (resolveClass cname2 state) state with
               | Some cname -> T_Class cname
               | None -> raise (IncompatibleTypes (cname1 ^ ", " ^ cname2)))
          | _ -> raise (IncompatibleTypes (typeToString t1 ^ ", " ^ typeToString t2))
        in
        (match op with
         | Intersection -> T_Set (t, 0, min ub1 ub2)
         | Union ->        T_Set (t, 0, ub1 + ub2)
         | _ -> raise UnexpectedError)
    | _ -> raise (IncompatibleTypes (typeToString t1 ^ ", " ^ typeToString t2))

(* resolves both class and field of a field access expression *)
and resolveFieldAccess expr fname state =
  let cls = 
    match typeof expr state with
    | T_Ref cname
    | T_Class cname -> resolveClass cname state
    | _ -> raise (InvalidFieldAccess (fname, typeToString (typeof expr state)))
  in
  let r = strictResolveMemberVar cls fname state in
  (snd(r), fst(r), snd(r))    (* <-- NEW: actually, just return the clsDecl rather than the straight class! *)

(* returns the element type of a collection *)
and elementType t name =
  match t with
  | T_Set (t', _, _) -> t'
  | _ -> raise (ExpectedSet2 name)

(* resolves an expression-level variable symbol, with member/global fallback *)
and resolveExprVar expr name state =
  let res = 
    match expr with
    | E_Fold (op, vname, collection, where, body) ->
      if vname = name then
        Some collection
      else
        None
    | _ -> raise UnexpectedError
  in
  match res with
  | Some collection -> (name, elementType (typeof collection state) name)
  | None ->
    match state.scope.node with
     | S_Global -> resolveGlobalVar name state
     | S_Class cls -> fst (resolveMemberVar cls name state)
     | S_Expr expr -> 
        resolveExprVar expr name { state with scope = match state.scope.parent with
                                                      | Some s -> s
                                                      | None -> raise UnexpectedError }

(* resolves any variable symbol *)
and resolveVar name state =
  match state.scope.node with
  | S_Global -> resolveGlobalVar name state
  | S_Class cls -> fst (resolveMemberVar cls name state)
  | S_Expr expr -> resolveExprVar expr name state 

(* pushes a scope to the state *)
let pushScope node state =
  { state with scope = { parent = Some state.scope; node = node; } }
    
(* pops a scope from the state *)
let popScope state =
  match state.scope.parent with
  | Some parent -> { state with scope = parent }
  | None -> raise UnexpectedError
  
(* counting ***********************************************************************)

(* generates a list of integers min..max *)
let seq min max =
  let rec next cur range =
    if cur = max then
      range @ [cur]
    else
      next (cur + 1) (range @ [cur])
  in
  next min []

(* increment count of key in lst *)
let incr key lst =
  if StrMap.mem key lst then
    StrMap.add key ((StrMap.find key lst) + 1) lst
  else
    StrMap.add key 1 lst

(* get the object-count for class `cls` *)
let count cname state =
  try
    StrMap.find cname state.counts
  with
  | Not_found -> 0
  
  
(* EXPERIMENTAL *)
let addSubclass super sub state =
  { state with subclasses =
      if StrMap.mem super state.subclasses then
        StrMap.add super ((StrMap.find super state.subclasses) @ [sub]) state.subclasses
      else
        StrMap.add super (sub :: []) state.subclasses
  }
    
(* increment object count *)
let rec countObj cls state =
  let state = { state with counts = incr cls.name state.counts } 
  in
  match cls.super with
  | Some sname ->
    let id = count cls.name state in
    let state = addSubclass sname id state in
    countObj (resolveClass sname state) state
  | None -> state
    
(* count objects for a varDecl *)
let rec countVarDecl var state =
  match snd var with
  | T_Class cname ->
      if state.show_counting then print_endline (fst var ^ ": " ^ cname) else ();
      let cls = resolveClass cname state in
      let state = countObj cls state in
      List.fold_left (fun state mbr ->
        match mbr with
        | M_Var v -> countVarDecl v state
        | _ -> state
      ) state cls.members
  | T_Set (T_Class cname, lbound, ubound) ->
      if state.show_counting then print_endline (fst var ^ ": " ^ cname ^ "[" ^ string_of_int ubound ^ "]") else ();
      let cls = resolveClass cname state in
      List.fold_left (fun state elem ->
        let state = countObj cls state in
        List.fold_left (fun state mbr ->
          match mbr with
          | M_Var v -> countVarDecl v state
          | _ -> state
        ) state cls.members
      ) state (seq 1 ubound)
  | _ -> state

(* count all objects in the model *)
let countModel state =
  List.fold_left (fun state d ->
    match d with
    | G_Var v -> countVarDecl v state
    | _ -> state
  ) state state.model.declarations
    
(* for debugging *)
let printCounts state =
  StrMap.iter (fun k v ->
    Printf.printf "%s: %d\n" k v
  ) state.counts

(* translation ********************************************************************)

(* generates a new index for an object *)
let newIndex cname state =
  let indexes' = incr cname state.indexes in
  let str = string_of_int (StrMap.find cname indexes') in
  (str, { state with indexes = indexes' })

(* generates a list of `num` new indices *)
let newIndices cname num state =
  let rec next i indices state =
    let (index, state) = newIndex cname state in
    let indices =  indices @ [index] in
    if i < num then
      next (i+1) indices state
    else
      (indices, state)
  in
  next 1 [] state

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
    List.fold_left (fun mzn elem ->
      if String.length mzn = 0 then
        string_of_int elem
      else
        mzn ^ "," ^ string_of_int elem
    ) "" list
  
(* translates a type *)
let rec translateType t state =
  match t with
  | T_Int -> "int"
  | T_Bool -> "bool"
  | T_Range (m, n) -> string_of_int m ^ ".." ^ string_of_int n
   | T_Class cname ->
      let cls = (resolveClass cname state) in
      if cls.abstract then raise (AbstractInstance (cls.name)) else ();
      "1.." ^ string_of_int (count cname state)
  | T_Ref cname ->
      let cls = (resolveClass cname state) in
      if cls.abstract then
        intListToMz (StrMap.find cname state.subclasses)
      else
        "1.." ^ string_of_int (count cname state)
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
  | E_Var vname ->
      ignore (resolveVar vname state);
      (match state.scope.node with
       | S_Global -> vname
       | S_Class cls -> cls.name ^ "_" ^ vname ^ "[i]"
       | S_Expr e -> vname)

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
            | T_Class cname -> ubound = lbound (* TODO: Do these really have to be the same? (not that i have varcard anyway)*)
            | _ -> false
            (* TODO: MUST detect anything non-constant inside this expression! *)
          in
          let mzBody = 
            let guard = (* <- TODO: optimise for fixed-card set of T_Class case *)
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
            (* TODO: auto-max for class refs... *)
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
        raise (NotImplemented "var-card sets of objects") (* which will require card constraints! *)
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
      let mzn = mzn ^ "constraint forall (i in 1.." ^ string_of_int ccount ^ ") (" ^ translateExpr expr state ^ ");\n" in
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
      let (idx, state) = newIndex cname state in       (* <---- THIS SHALL BE MY APPROACH TO TRANSLATION - ACTUALLY use state *)
      let mzn = mzn  ^ translateType t state ^ ": " ^ vname ^ " = " ^ idx ^ ";\n" in
      (mzn, state)
  | T_Set (T_Class cname, lbound, ubound) ->
      if lbound <> ubound then
        raise (NotImplemented "var-card sets of objects")  (* which will require card constraints! *)
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
  match con with
  | C_Where expr ->
    let ccount = string_of_int (count cls.name state) in
    "\nconstraint\n  forall (i in 1.." ^ ccount ^ ") (\n    " ^ translateExpr expr state ^ "\n  );\n"
  | C_Maximise expr -> raise (NotImplemented "class-level maximise") (* TODO *)
    
(* translates a global constraint *)
let translateGlobalConstraint con state =
  match con with
  | C_Where expr ->
    (if state.comments then "\n% global" else "") ^
    "\nconstraint\n  " ^ translateExpr expr state ^ ";\n"
  | C_Maximise expr -> raise (NotImplemented "global maximise") (* TODO *)

(* translates a class *)
let rec translateClassBody cls clsSuper mzn state =
  List.fold_left (fun (mzn, state) mbr ->
    let (mzn', state) =
      match mbr with
      | M_Var var -> translateMemberVar cls var state
      | M_Constraint con -> ("", state) (translateClassConstraint cls con state, state DEBUG*) in
    (mzn  ^ mzn', state)
  ) (mzn, state) clsSuper.members

(* translates a class *)
and translateClass cls state =
  let state = pushScope (S_Class cls) state in
  let mzn = if state.comments then "\n% class " ^ cls.name ^ "\n" else "" in
  let (mzn, state) =
    (match cls.super with
    | Some cname ->
        let super = (resolveClass cname state) in
        if not super.abstract then
          raise (NotImplemented ("`" ^ cls.name ^ "` inherits non-abstract class `" ^ cname ^ "`"))
        else ();
        (mzn, state)
    | None -> (mzn, state))
  in
  (* current class *)
  let mzn = mzn ^ "\n% own members\n" in
  let (mzn, state) = translateClassBody cls cls mzn state in
  (mzn, popScope state)

(* translates the entire model *)
let translateModel state =
  List.fold_left (fun (mzn, state) d ->
    let (mzn', state) =
      match d with
      | G_Var var -> translateGlobalVar var state
      | G_Class cls -> translateClass cls state
      | G_Constraint con -> (translateGlobalConstraint con state, state) in
    (mzn  ^ mzn', state)
  ) ("", state) state.model.declarations

(* entry point ********************************************************************)

(* translates the model into a string of MiniZinc *)
let toMiniZinc csModel showCounting hasComments =
  (* 1st pass: count objects *)
  let scope = { parent = None; node = S_Global} in
  let state = { counts = StrMap.empty; indexes = StrMap.empty; model = csModel; 
                scope = scope; subclasses = StrMap.empty; show_counting = showCounting; 
                mzn_output = []; comments = hasComments } in
  let state = countModel state in
  if showCounting then
    (printCounts state;
    "")
  else
    (* 2nd pass: translate to MiniZinc *)
    let (mzn, state) = translateModel state in
    mzn ^ "\nsolve satisfy;\n\n"
        ^ "output ["
        ^ (listToMz state.mzn_output)
        ^ "];"