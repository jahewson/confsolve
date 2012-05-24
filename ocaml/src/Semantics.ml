open ConfSolve
module StrMap = Map.Make(String)

exception UnexpectedError
exception NotImplemented of string

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
  maximise_count: int;            (* count of maximise terms *)
  scope: scope;                   (* scope tree *)
  mzn_output: string list;        (* MiniZinc `output` variables *)
  show_counting: bool;            (* debugging - print the object counts *)
  comments: bool;                 (* debugging - comment MiniZinc output *)
}

(* binding ************************************************************************)

exception SymbolNotDefined of string                (* symbol not defined *)
exception TypeNotDefined of string                  (* type not defined *)
exception ClassNotDefined of string                 (* class not defined *)
exception VarNotDefined of string                   (* variable not defined *)
exception MemberVarNotDefined of string * string    (* member variable not defined (name, class) *)
exception EnumMemberNotDefined of string * string   (* enum member not defined *)
exception ExpectedType of string                    (* expected a type *)
exception ExpectedSet1 of string                    (* expected a set *)
exception ExpectedSet2 of string                    (* expected a set *)
exception SetOfSet of string                        (* sets of sets are not permitted *)
exception InvalidFieldAccess of string * string     (* invalid field access (field, type) *)
exception IncompatibleTypes of string               (* expression of incompatible types *)
exception NoInstancesOfClass of string              (* no instances of class for reference to resolve to *)
exception AbstractInstance of string                (* cannot create an instance of an abstract class *)

(* for reporting compile errors *)
let rec typeToString (t: _type) =
  match t with
  | T_Symbol sym -> raise UnexpectedError
  | T_Int -> "int"
  | T_Bool -> "bool"
  | T_Range(m,n) -> string_of_int m ^ ".." ^ string_of_int n
  | T_Class(c) -> c
  | T_Enum(c) -> c
  | T_Ref(r) -> "ref " ^ r
  | T_Set(t, lbound, ubound) -> typeToString t ^ "[" ^ string_of_int lbound ^ ".." ^ string_of_int ubound ^ "]"

(* types -------------------------------------------------------------------------*)

(* determines the type of an expression *)
let rec typeof expr state =
  match expr with
  | E_Symbol sym -> raise UnexpectedError
  | E_Var vname -> snd (resolveVar vname state)
  | E_Enum ename -> T_Enum ename
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
  let t1 = typeof e1 state in
  let t2 = typeof e2 state in
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

(* returns the element type of a collection *)
and elementType t name =
  match t with
  | T_Set (t', _, _) -> t'
  | _ -> raise (ExpectedSet2 name)

(* scope -------------------------------------------------------------------------*)

(* pushes a scope to the state *)
and pushScope node state =
  { state with scope = { parent = Some state.scope; node = node; } }

(* pops a scope from the state *)
and popScope state =
  match state.scope.parent with
  | Some parent -> { state with scope = parent }
  | None -> raise UnexpectedError

(* resolution ---------------------------------------------------------------------*)
  
(* resolve global symbol *)
and resolveGlobalSymbol name state =
  let found = List.fold_left (fun found decl ->
    match found with
    | Some _ -> found
    | None ->
        match decl with
        | Var (vname, t) -> if vname = name then Some decl else None
        | Enum enm -> if enm.enumName = name then Some decl else None
        | Class cls -> if cls.name = name then Some decl else None
        | Constraint _ -> found
  ) None state.model.declarations
  in
  match found with
  | Some f -> f
  | None -> raise (SymbolNotDefined name)
  
(* resolves a class-level symbol, returning both the variable and class which defined it *)
and resolveMemberSymbolImpl noGlobal cls name state =
  if name = "this" then
    (Var ("this", T_Class cls.name), Some cls)
  else
    let found = List.fold_left (fun found mbr ->
      match found with
      | Some _ -> found
      | None ->
          match mbr with
          | Var (vname, t) -> if vname = name then Some mbr else None
          | Constraint _ -> found
          | Enum _ | Class _ -> raise UnexpectedError
    ) None cls.members
    in
    match found with
    | Some mbr -> (mbr, Some cls)
    | None ->
        match cls.super with
        | Some cname -> resolveMemberSymbolImpl noGlobal (resolveClass cname state) name state
        | None ->
          if noGlobal then
            raise (SymbolNotDefined name)
          else
            (resolveGlobalSymbol name (popScope state), None)

(* resolve a member symbol *)
and resolveMemberSymbol cls name state =
  resolveMemberSymbolImpl false cls name state
  
(* resolve a member symbol, with no global fallback *)
and resolveMemberSymbolOnly cls name state =
  resolveMemberSymbolImpl true cls name state

(* resolve expression-level symbol *)
and resolveExprSymbol vname collection name state =
  if vname = name then
    Var (name, elementType (typeof collection state) name)
  else
   resolveSymbol name (popScope state)
    
(* resolve any symbol *)
and resolveSymbol name state =
  match state.scope.node with
  | S_Global -> resolveGlobalSymbol name state
  | S_Class cls -> fst (resolveMemberSymbol cls name state)
  | S_Expr (E_Fold (_, vname, collection, _, _)) -> resolveExprSymbol vname collection name state
  | S_Expr _ -> raise UnexpectedError
  
(* resolution helpers -------------------------------------------------------------------*)
  
(* resolve an enum *)
and resolveEnum name state =
  match resolveGlobalSymbol name state with
  | Enum enm -> enm
  | _ -> raise UnexpectedError
  
(* resolve a variable *)
and resolveVar name state =
  match resolveSymbol name state with
  | Var var -> var
  | _ -> raise UnexpectedError
    
(* resolve a field access *)
and resolveFieldAccess expr fname state =
  let cls = 
    match typeof expr state with
    | T_Ref cname
    | T_Class cname -> resolveClass cname state
    | _ -> raise (InvalidFieldAccess (fname, typeToString (typeof expr state)))
  in
  let r =
    match resolveMemberSymbolOnly cls fname state with
    | (Var var, Some cls) -> (var, cls)
    | _ -> raise UnexpectedError
  in
  (snd(r), fst(r), snd(r))

(* resolve a member variable to a (var, class) *)
and resolveMemberVar cls name state =
  match resolveMemberSymbol cls name state with
  | (Var var, optCls) -> (var, optCls)
  | _ -> raise UnexpectedError

(* resolve a class *)
and resolveClass name state =
  match resolveGlobalSymbol name state with
  | Class c -> c
  | _ -> raise UnexpectedError

(* gets all inherited members of a class *)
let rec allMembers cls state =
  match cls.super with
  | Some cname -> cls.members @ allMembers (resolveClass cname state) state
  | None -> cls.members
    
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
  (*print_endline ("+1 " ^ key);*)
  if StrMap.mem key lst then
    StrMap.add key ((StrMap.find key lst) + 1) lst
  else
    StrMap.add key 1 lst

(* get the current object-count for class `cls` *)
let getCurCount cname state =
  try
    StrMap.find cname state.counts
  with
  | Not_found -> 0
  
(* add a subclass of `super` *)
let addSubclass super sub state =
  { state with
      subclasses =
        if StrMap.mem super state.subclasses then
          StrMap.add super ((StrMap.find super state.subclasses) @ [sub]) state.subclasses
        else
          StrMap.add super (sub :: []) state.subclasses
  }
 
let getSubclasses cname state =
  try
    StrMap.find cname state.subclasses
  with
  | Not_found ->
      print_endline cname;
      raise Not_found
 
(* get the root class of a subclass *)
let rec rootClass cls state =
  match cls.super with
  | Some cname -> rootClass (resolveClass cname state) state
  | None -> cls
  
(* increment object count *)
let rec countObj cls state =
  (* increment the root count, and fetch the new id *)
  let state = { state with counts = incr (rootClass cls state).name state.counts } in
  let id = getCurCount (rootClass cls state).name state 
  in
  (* always a subclass of itself *)
  let state = addSubclass cls.name id state
  in
  match cls.super with
  | Some sname ->
    (* count this subclass, as only the root was counted above *)
    let state = { state with counts = incr cls.name state.counts }
    in 
    (* now count its superclasses *)
    countSuperObj id (resolveClass sname state) state
  | None -> state
 
(* helper - counts superclasses *)
and countSuperObj id cls state =
    let root = rootClass cls state in
    if root = cls then
      (* root already counted by countObj, so we only need to track the subclasss *)
      addSubclass root.name id state
    else
      (* increment the superclass count *)
      let state = { state with counts = incr cls.name state.counts } 
      in
      (* track the subclasss *)
      let state = addSubclass cls.name id state
      in
      (* next superclass *)
      match cls.super with
      | Some sname -> countSuperObj id (resolveClass sname state) state
      | None -> state
    
(* count objects for a varDecl *)
let rec countVarDecl (vname, t) state =
  match t with
  | T_Class cname ->
      if state.show_counting then print_endline ("\n" ^ vname ^ ": " ^ cname) else ();
      let cls = resolveClass cname state in
      if cls.isAbstract then raise (AbstractInstance (vname ^ ": " ^ cname)) else ();
      let state = countObj cls state in
      List.fold_left (fun state mbr ->
        match mbr with
        | Var v -> countVarDecl v state
        | _ -> state 
      ) state (allMembers cls state)
      
  | T_Set (T_Class cname, lbound, ubound) ->
      if state.show_counting then print_endline ("\n" ^ vname ^ ": " ^ cname ^ "[" ^ string_of_int ubound ^ "]") else ();
      let cls = resolveClass cname state in
      List.fold_left (fun state elem ->
        let state = countObj cls state in
        List.fold_left (fun state mbr ->
          match mbr with
          | Var v -> countVarDecl v state
          | _ -> state
        ) state (allMembers cls state)
      ) state (seq 1 ubound)
      
  | _ -> state

(* count all objects in the model *)
let countModel state =
  List.fold_left (fun state d ->
    match d with
    | Var v -> countVarDecl v state
    | _ -> state
  ) state state.model.declarations
    
(* for debugging *)
let printCounts state =
  print_endline "\n------------\n";
  StrMap.iter (fun k v ->
    Printf.printf "%s: %d\n" k v
  ) state.counts

(* translation ********************************************************************)

(* get the object-count for `cls` *)
let rawCount cname state =
  try
    StrMap.find cname state.counts
  with
  | Not_found -> 0

(* get the object-count for root(`cls`) *)
let count cname state =
  let cname = (rootClass (resolveClass cname state) state).name in
  rawCount cname state
  
(* generates a new index for an object of type root(`cls`) *)
let newIndex cname state =
  let cname = (rootClass (resolveClass cname state) state).name
  in
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
      intListToMz (getSubclasses cname state)
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
  let mzIds = intListToMz (getSubclasses cls.name state) in
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

(* forward declarations ***********************************************************)

let rec forwardExpr expr state =
  match expr with
  | E_Var _ | E_Enum _ -> raise UnexpectedError

  | E_Symbol name ->
      (match resolveSymbol name state with
      | Var _ -> E_Var name
      | Enum _ -> E_Enum name
      | Class _ | Constraint _ -> raise UnexpectedError)
      
  | E_Fold (op, name, collection, where, body) ->
      let collection = forwardExpr collection state in
      let bodyState = pushScope (S_Expr (E_Fold (op, name, collection, where, body))) state in
      let where = forwardExpr where bodyState in
      let body = forwardExpr body bodyState in
      E_Fold (op, name, collection, where, body)

  | E_Op (e1, op, e2) -> E_Op ((forwardExpr e1 state), op, (forwardExpr e2 state))
  | E_Card e -> E_Card (forwardExpr e state)
  | E_Neg e -> E_Neg (forwardExpr e state)
  | E_Not e -> E_Not (forwardExpr e state)
  | E_Paren e -> E_Paren (forwardExpr e state)
  | E_Access (e, mname) -> E_Access ((forwardExpr e state), mname)
  | E_Bool _ | E_Int _ -> expr
      
let forwardConstraint con state =
  match con with
  | C_Where expr -> C_Where (forwardExpr expr state)
  | C_Maximise expr -> C_Maximise (forwardExpr expr state)

let rec forwardType t state =
  match t with
  | T_Symbol name ->
      (match resolveSymbol name state with
       | Class cls -> T_Class name
       | Enum enm -> T_Enum name
       | Constraint _ | Var _ -> raise (ExpectedType name))
  | T_Set (t, lb, ub) ->
      T_Set (forwardType t state, lb, ub)
  | _ -> t

let forwardVar (vname, t) state =
  (vname, forwardType t state)
  
let forwardEnum enm state =
  enm
  
let forwardClassDecl cls state =
  let state = pushScope (S_Class cls) state in
  { cls with members =
      List.map (fun mbr ->
        match mbr with
        | Var var -> Var (forwardVar var state)
        | Constraint con -> mbr
        | Enum _ | Class _ -> raise UnexpectedError
      ) cls.members
    }

let forwardClassUse cls state =
  let state = pushScope (S_Class cls) state in
  { cls with members =
      List.map (fun mbr ->
        match mbr with
        | Constraint con -> Constraint (forwardConstraint con state)
        | Var var -> mbr
        | Enum _ | Class _ -> raise UnexpectedError
      ) cls.members
    }

(* resolve forward declarations i.e. T_Symbol and E_Symbol *)
let forwardModel state =
  let state =
    (* 1st pass - declarations *)
    { state with model = { declarations =
        List.map (fun decl ->
          match decl with
          | Var var -> Var (forwardVar var state)
          | Class cls -> Class (forwardClassDecl cls state)
          | Enum enm -> Enum (forwardEnum enm state)
          | Constraint con -> decl
        ) state.model.declarations
      }}
  in
  (* 2nd pass - constraints *)
  { state with model = { declarations =
      List.map (fun decl ->
        match decl with
        | Class cls -> Class (forwardClassUse cls state)
        | Constraint con -> Constraint (forwardConstraint con state)
        | Var _ | Enum _ -> decl
      ) state.model.declarations
    }}
    
(* entry point ********************************************************************)

(* translates the model into a string of MiniZinc *)
let toMiniZinc csModel showCounting hasComments =
  (* init *)
  let scope = { parent = None; node = S_Global} in
  let state = { counts = StrMap.empty; indexes = StrMap.empty; model = csModel; 
                scope = scope; subclasses = StrMap.empty; show_counting = showCounting; 
                mzn_output = []; comments = hasComments; maximise_count = 0 } in
                
  (* 1st pass: resolve forward declarations *)
  let state = forwardModel state in          
                
  (* 2nd pass: count objects *)
  let state = countModel state in
  if showCounting then
    (printCounts state;
    "")
  else
    (* 3rd pass: translate to MiniZinc *)
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