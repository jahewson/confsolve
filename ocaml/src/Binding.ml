open ConfSolve
open State

exception UnexpectedError
exception NotImplemented of string

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
  | E_BoolToInt e -> T_Int
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