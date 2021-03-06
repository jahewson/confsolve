open ConfSolve
open DeclBinding
open Counting
open State
open Util

(* binding ************************************************************************)

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
exception BadlyTypedExpression of string            (* expression is not well-typed *)
exception NoInstancesOfClass of string              (* no instances of class for reference to resolve to *)

(* for reporting compile errors *)
let rec typeToString (t: _type) =
  match t with
  | T_Infer -> "T_Infer"
  | T_Symbol s -> "T_Symbol "
  | T_Int -> "int"
  | T_Bool -> "bool"
  | T_BInt set ->
      if isSetContiguous set then
        string_of_int (IntSet.min_elt set) ^ ".." ^ string_of_int (IntSet.max_elt set)
      else
        "{" ^ (IntSet.fold (fun elem acc  ->
            if String.length acc = 0 then 
              string_of_int elem
            else
              acc ^ "," ^ string_of_int elem
          ) set "") ^ "}"
  | T_Class c -> c
  | T_Enum c -> c
  | T_Ref r -> "ref " ^ r
  | T_Set (t, -1, -1) -> typeToString t ^ "[]"
  | T_Set (t, lbound, ubound) -> typeToString t ^ "[" ^ string_of_int lbound ^ ".." ^ string_of_int ubound ^ "]"

(* for reporting expression type errors *)
let opToString op =
  match op with
  | Eq -> "="
  | Neq -> "!="
  | Gt -> ">"
  | Ge -> ">="
  | Lt -> "<"
  | Le -> "<="
  | In -> "in"
  | Subset -> "subset"
  | Union -> "union"
  | Intersection -> "intersection"
  | And -> "&&"
  | Or -> "||"
  | Implies -> "->"
  | Iff -> "iff"
  | Add -> "+"
  | Sub -> "-"
  | Div -> "/"
  | Mul -> "*"
  | Pow -> "^"
  | Mod -> "mod"

(* for reporting expression type errors *)
let foldOpToString op =
  match op with
  | ForAll -> "ForAll"
  | Exists -> "Exists"
  | Sum -> "Sum"
  
(* constants ***********************************************************)
(*
(* determines if an expression is constant *)
let rec isConstant expr state =
  match expr with
  | E_Symbol _ ->
      raise UnexpectedError     
  | E_Var (vname, t, scope) ->
      let sym = Var (vname, t) in
      isVarConstant sym state     
  | E_Access (e, fname) ->
      let (_,v,_) = resolveFieldAccess e fname state in
      isVarConstant (Var v) state
  | E_Op (e1, Or, e2) ->
      isConstant e1 state || isConstant e2 state
  | E_Op (e1, _, e2) ->
      isConstant e1 state && isConstant e2 state
  | E_Fold (op, name, collection, where, body) ->
      let bodyState = pushScope (S_Expr (E_Fold (op, name, collection, where, body))) state in
      isConstant collection state && 
      isConstant where bodyState && 
      isConstant body bodyState
  | E_Card e
  | E_Neg e
  | E_BoolToInt e
  | E_Not e
  | E_Paren e ->
      isConstant e state
  | E_Set elist ->
      List.for_all (fun e -> isConstant e state) elist
  | E_Enum _
  | E_Bool _
  | E_Int _ ->
      true

(* determines if a variable has a constant equality constraint over it *)
and isVarConstant var state =
  match getVarConstantExpr var state with
  | Some _ -> true
  | None -> false

(* returns the constant equality constraint for a variable *)
and getVarConstantExpr var state =
  let process = fun some_cls con state ->
    (* if top-level is an assignment, and RHS or LHS is an E_Var, and E_Var = vname *)
    match con with
    | C_Where E_Op (E_Var (vname, t, scope), Eq, e)
    | C_Where E_Op (e, Eq, E_Var (vname, t, scope)) ->
      (* resolve vname to check that this is actually the same variable (because of nested scope) *)
      if resolveExprSymbol vname state = var then
        if isConstant e state then
          Some e
        else
          None
      else
        None
    | C_Where e -> None
    | C_Maximise e | C_MinChange_Maximise e -> None
  in
  (* for every constraint *)
  List.fold_left (fun e decl ->
    match e with
    | Some _ -> e
    | None ->
        (match decl with
        | Constraint con ->
            process None con state
        | Class cls ->
            let state = pushScope (S_Class cls) state in
            List.fold_left (fun acc mbr ->
              match mbr with
              | Constraint con -> process (Some cls) con state
              | Var _ -> acc
              | Enum _ | Class _ -> raise UnexpectedError
            ) None cls.members
        | Var _ | Enum _ -> e)
  ) None state.model.declarations
*)            

(* types -------------------------------------------------------------------------*)

(* determines the type of an expression, and type-checks it *)
let rec typeof expr state =
  let t =
    match expr with
    | E_Symbol _ -> raise UnexpectedError
  
    | E_Var (vname, t, scope) ->
        (* requires constants - also is recursive with getVarConstantExpr and resolveVar *)
        (*let var =
          (match state.scope.node with
           | S_Global 
           | S_Expr _ ->
              Var (resolveVar vname state)
           | S_Class cls ->
               let (v, clsDecl) = resolveMemberVar cls vname state in
               Var v)
        in
        (match getVarConstantExpr var state with
        | Some e ->
            let t = typeof e state in
            (match t with
            | T_Int _ -> raise UnexpectedError
            | _ -> t)
        | None ->
            snd (resolveVar vname state))
        *)
         t
  
    | E_Enum ename -> T_Enum ename
  
    | E_Access (E_Enum ename, mname) ->
        let enm = resolveEnum ename state.scope in
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
          T_Enum ename
  
    | E_Access (e, fname) -> let (_,v,_) = resolveFieldAccess e fname state in snd v
  
    | E_Op (e1, Eq, e2)
    | E_Op (e1, Neq, e2) ->
        let t1 = typeof e1 state and t2 = typeof e2 state in
        if
          (match (t2, t2) with
          | (T_Infer, _)
          | (_, T_Infer)
          | (T_Symbol _, _)
          | (_, T_Symbol _) -> raise UnexpectedError
        
          | (T_Int _, _)
          | (_, T_Int _) -> true
   
          | (T_Bool, T_Bool)-> true
          | (T_Bool, _) 
          | (_, T_Bool) -> false
        
          | (T_BInt _, T_BInt _) -> true
          | (T_BInt _, _)
          | (_, T_BInt _) -> false
        
          | (T_Enum ename1, T_Enum ename2) -> ename1 = ename2
          | (T_Enum _, _)
          | (_, T_Enum _) -> false
        
          | (T_Class cname1, T_Class cname2)
          | (T_Ref cname1, T_Class cname2)
          | (T_Class cname1, T_Ref cname2) ->
              (match commonAncestor (resolveClass cname1 state.scope) (resolveClass cname2 state.scope) state with
              | Some _ -> true
              | None -> false)
          | (T_Class _, _) 
          | (_, T_Class _) -> false

          | (T_Ref cname1, T_Ref cname2) ->
              (match commonAncestor (resolveClass cname1 state.scope) (resolveClass cname2 state.scope) state with
              | Some _ -> true
              | None -> false)
          | (T_Ref _, _)
          | (_, T_Ref _) -> false
        
          | (T_Set (_,_,_), T_Set (_,_,_)) -> ignore (typeOfSetOpWithTypes t1 Union t2 state); true
          (*| (T_Set (_,_,_), _)
          | (_, T_Set (_,_,_)) -> false <-- "this match case is unused" because the rules above already cover it*)
          )
        then
          T_Bool
        else
          let op = (match expr with
            | E_Op (_, op, _) -> op
            | _ -> raise UnexpectedError
          ) in
          raise (BadlyTypedExpression (typeToString t1 ^ " " ^ (opToString op) ^ " " ^  typeToString t2))

    | E_Op (e1, Le, e2)
    | E_Op (e1, Lt, e2)
    | E_Op (e1, Ge, e2)
    | E_Op (e1, Gt, e2) -> 
      (match (typeof e1 state, typeof e2 state) with
      | (T_Int, T_Int)
      | (T_BInt _, T_Int)
      | (T_Int, T_BInt _)
      | (T_BInt _, T_BInt _) -> T_Bool
      | (t1, t2) ->
          let op = (match expr with
            | E_Op (_, op, _) -> op
            | _ -> raise UnexpectedError
          ) in
          raise (BadlyTypedExpression (typeToString t1 ^ " " ^ (opToString op) ^ " " ^ typeToString t2)))
    
    | E_Op (e1, In, e2) ->
        let t1 = typeof e1 state and t2 = typeof e2 state in
        if
          (match (t1, t2) with
          | (T_BInt _, T_Set (T_BInt _, _, _)) -> true
          | (T_Bool, T_Set (T_Bool, _, _)) -> true
          | (T_Enum ename1, T_Set (T_Enum ename2, _, _)) -> ename1 = ename2
          | (T_Class cname1, T_Set (T_Class cname2, _, _))
          | (T_Ref cname1, T_Set (T_Ref cname2, _, _))
          | (T_Class cname1, T_Set (T_Ref cname2, _, _))
          | (T_Ref cname1, T_Set (T_Class cname2, _, _)) ->
              (match commonAncestor (resolveClass cname1 state.scope) (resolveClass cname2 state.scope) state with
              | Some cname -> true
              | None -> false)
          | _ -> false)
        then
          T_Bool
        else
          raise (BadlyTypedExpression (typeToString t1 ^ " in " ^ typeToString t2))

    | E_Op (e1, Subset, e2) ->
      let t1 = typeof e1 state and t2 = typeof e2 state in
      if
        (match (t1, t2) with
        | (T_Set (T_BInt _, _, _), T_Set (T_BInt _, _, _)) -> true
        | (T_Set (T_Bool, _, _), T_Set (T_Bool, _, _)) -> true
        | (T_Set (T_Enum ename1, _, _), T_Set (T_Enum ename2, _, _)) -> ename1 = ename2
        | (T_Set (T_Class cname1, _, _), T_Set (T_Class cname2, _, _))
        | (T_Set (T_Ref cname1, _, _), T_Set (T_Ref cname2, _, _))
        | (T_Set (T_Class cname1, _, _), T_Set (T_Ref cname2, _, _))
        | (T_Set (T_Ref cname1, _, _), T_Set (T_Class cname2, _, _)) ->
            (match commonAncestor (resolveClass cname1 state.scope) (resolveClass cname2 state.scope) state with
            | Some cname -> true
            | None -> false)
        | _ -> false)
      then
        T_Bool
      else
        raise (BadlyTypedExpression (typeToString t1 ^ " subset " ^ typeToString t2))
  
    | E_Op (e1, Union, e2) -> typeOfSetOp e1 Union e2 state
    | E_Op (e1, Intersection, e2) -> typeOfSetOp e1 Intersection e2 state
  
    | E_Op (e1, And, e2)
    | E_Op (e1, Or, e2)
    | E_Op (e1, Implies, e2)
    | E_Op (e1, Iff, e2) ->
       (match (typeof e1 state, typeof e2 state) with
       | (T_Bool, T_Bool) -> T_Bool
       | (t1, t2) ->
         let op = (match expr with
           | E_Op (_, op, _) -> op
           | _ -> raise UnexpectedError
         ) in
         raise (BadlyTypedExpression (typeToString t1 ^ " " ^ (opToString op) ^ " " ^ typeToString t2)))
  
    | E_Op (e1, Add, e2) -> typeOfIntOpToInt e1 e2 (+) "+" state
    | E_Op (e1, Sub, e2) -> typeOfIntOpToInt e1 e2 (-) "-" state
    | E_Op (e1, Div, e2) -> typeOfIntOpToInt e1 e2 (/) "/" state
    | E_Op (e1, Mul, e2) -> typeOfIntOpToInt e1 e2 ( * ) "*" state
    | E_Op (e1, Pow, e2) -> typeOfIntOpToInt e1 e2 (fun x y -> int_of_float (float_of_int x ** float_of_int y)) "^" state
    | E_Op (e1, Mod, e2) -> typeOfIntOpToInt e1 e2 (mod) "mod" state
  
    | E_Neg e ->
        (match typeof e state with
        | T_Int -> T_Int
        | T_BInt s -> T_BInt (IntSet.fold (fun x s' -> IntSet.add (-x) s') s IntSet.empty)
        | t -> raise (BadlyTypedExpression ("- " ^ typeToString t)))
  
    | E_Not e ->
        (match typeof e state with
        | T_Bool _ -> T_Bool
        | t -> raise (BadlyTypedExpression ("not " ^ typeToString t)))
  
    | E_Old e ->
        typeof e state
  
    | E_Card e ->
        (match typeof e state with
        | T_Set (T_Bool, _, _) -> T_BInt (makeSet 0 2)
        | T_Set (T_BInt set, _, _) -> T_BInt (makeSet 0 (IntSet.cardinal set))
        | T_Set (T_Class cname, _, _)
        | T_Set (T_Ref cname, _, _) ->
            if hasCounts state then
              T_BInt (makeSet 0 (count cname state))
            else
              raise UnexpectedError
        | t -> raise (BadlyTypedExpression (typeToString t ^ " .size")))
  
    | E_BoolToInt e -> 
        (match typeof e state with
        | T_Bool _ -> T_BInt (makeSet 0 1)
        | t -> raise (BadlyTypedExpression ("bool2int(" ^ typeToString t ^ ")")))
  
    | E_Abs e -> 
        (match typeof e state with
        | T_Int -> T_Int
        | T_BInt s -> T_BInt (IntSet.fold (fun x s' -> IntSet.add (abs x) s') s IntSet.empty)
        | t -> raise (BadlyTypedExpression ("abs(" ^ typeToString t ^ ")")))
        
    | E_Fold (op, var, collection, where, body) ->
        let vname = fst var in
        
        let bodyState = {
          state with scope =
            pushScope (S_Expr (E_Fold (op, var, collection, where, body))) state.scope
          }
        in
      
        (match typeof collection state with 
        | T_Set (T_Set (_,_,_),_,_) -> raise (SetOfSet vname)
        | T_Set (_, _, _) -> ()
        | t -> raise (BadlyTypedExpression (foldOpToString op ^ " " ^ typeToString t))
        );
      
        let t = typeof where bodyState in
        (match t with
        | T_Bool -> ()
        | _ -> raise (BadlyTypedExpression (foldOpToString op ^  " ... where " ^ typeToString t))
        );

        let isSum =
          match op with
          | ForAll | Exists -> false
          | Sum -> true
        in
        let t = typeof body bodyState in
        let ok =
          match t with
          | T_Int
          | T_BInt _ -> isSum
          | T_Bool -> not isSum
          | _ -> false
        in
        if not ok then
          raise (BadlyTypedExpression (foldOpToString op ^  " ... { " ^ typeToString t ^ " }"))
        else
          if isSum then
            T_Int (* Q: T_BInt ? *)
          else
            T_Bool
  
    | E_Bool b -> T_Bool
    | E_Int i -> T_BInt (IntSet.singleton i)
    | E_Set elist -> typeOfSetLiteral elist state
  
    | E_Paren e -> typeof e state
  in
  match t with
  | T_Symbol s -> raise (BadlyTypedExpression (s ^ " : T_Symbol"))
  | _ -> t
  
(* the type of an int -> int operation *)
and typeOfIntOpToInt e1 e2 fold foldstr state =
  (match (typeof e1 state, typeof e2 state) with
  | (T_Int, T_Int)
  | (T_BInt _, T_Int)
  | (T_Int, T_BInt _) -> T_Int
  | (T_BInt s1, T_BInt s2) -> T_BInt (cartesian_product_fold s1 s2 fold)
  | _ -> raise (BadlyTypedExpression foldstr))

(* applies a function to the pairs in the cartesian product of two sets *)
and cartesian_product_fold xs ys f =
  IntSet.fold (fun x acc -> 
    IntSet.fold (fun y acc -> 
      IntSet.add (f x y) acc
    ) ys acc
  ) xs IntSet.empty

(* determines the type of a set literal *)
and typeOfSetLiteral elist state =
  let bound = List.length elist in
  let hdType = typeof (List.hd elist) state in
    List.fold_left (fun t expr' ->
        let t' = typeof expr' state in
        typeOfSetOpWithTypes t Union (T_Set (t', bound, bound)) state
    ) (T_Set (hdType, bound, bound)) elist
  
(* return a list of all ancestors of a class, sorted by distance *)
and ancestors cls ancs state =
  let ancs = ancs @ [cls.name] in
  match cls.super with
  | None -> ancs
  | Some cname -> ancestors (resolveClass cname state.scope) ancs state

(* determines the type of a set expression *)
and typeOfSetOp e1 op e2 state =
  let t1 = typeof e1 state in
  let t2 = typeof e2 state in
  typeOfSetOpWithTypes t1 op t2 state
      
(* determines the type of a set expression, given two types for those expressions *)
(* note: this could be refactored now that cards are all -1 *)
and typeOfSetOpWithTypes t1 op t2 state =
  match (t1, t2) with
  | (T_Set (T_Int, lb1, ub1), T_Set (T_Int, lb2, ub2)) ->
      raise (BadlyTypedExpression "int[]")
  
  | (T_Set (T_BInt s1, lb1, ub1), T_Set (T_BInt s2, lb2, ub2)) ->
      let s' = 
        (match op with
        | Union -> IntSet.union s1 s2
        | Intersection -> IntSet.inter s1 s2
        | _ -> raise UnexpectedError)
      in T_Set (T_BInt s', -1, -1)
            
  | (T_Set (T_Bool, lb1, ub1), T_Set (T_Bool, lb2, ub2)) ->
      (* 0/1 *)
      let t = T_Bool in
      T_Set (t, -1, -1)
      
  | (T_Set (T_Ref cname1, lb1, ub1), T_Set (T_Class cname2, lb2, ub2))
  | (T_Set (T_Class cname1, lb1, ub1), T_Set (T_Ref cname2, lb2, ub2))
  | (T_Set (T_Ref cname1, lb1, ub1), T_Set (T_Ref cname2, lb2, ub2))
  | (T_Set (T_Class cname1, lb1, ub1), T_Set (T_Class cname2, lb2, ub2)) ->
      let t = 
        (match commonAncestor (resolveClass cname1 state.scope) (resolveClass cname2 state.scope) state with
        | Some cname -> T_Class cname
        | None -> raise (BadlyTypedExpression (cname1 ^ ", " ^ cname2)))
      in T_Set (t, -1, -1)
       
  | _ -> raise (BadlyTypedExpression (typeToString t1 ^ ", " ^ typeToString t2))

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
    
(* returns the element type of a collection *)
and elementType t name =
  match t with
  | T_Set (t', _, _) -> t'
  | _ -> raise (ExpectedSet2 name)

(* resolution ---------------------------------------------------------------------*)

(* resolve expression-level symbol *)
and resolveExprSymbol op var collection where body name scope =
  let (vname, t) = var in
  if vname = name then
    (* generally, inference must be done as another pass,
       however, the case for only fold variables is simple. *)
    
    let model =
      match getGlobalScope scope with
      | { parent = _; node = S_Global model } -> model
      | _ -> raise UnexpectedError
    in
    
    let scope = { parent = None; node = S_Global model } in
    let state = { counts = StrMap.empty; indexes = StrMap.empty;
                  scope = scope; subclasses = StrMap.empty; } in
    let state = { state with scope = pushScope (S_Expr (E_Fold (op, var, collection, where, body))) state.scope } in
    
    (* count objects - hugely inefficient to put this here - TODO: put counts in the AST? *)
    let state = Counting.countModel false model state in
    
    Var (name, elementType (typeof collection state) name)
  else
    let scope = popScope scope in
    match scope.node with
    | S_Global _ -> resolveDeclSymbol name scope
    | S_Class _ -> resolveDeclSymbol name scope
    | S_Expr _ -> resolveExpressionSymbol name scope
    
(* resolve any symbol *)
and resolveExpressionSymbol name scope =
  match scope.node with
  | S_Global model -> resolveGlobalSymbol name scope
  | S_Class cls -> fst (resolveMemberSymbol cls name scope)
  | S_Expr (E_Fold (op, var, collection, where, body)) -> resolveExprSymbol op var collection where body name scope
  | S_Expr _ -> raise UnexpectedError
  
(* resolve any symbol and declaring class *)
and resolveExpressionSymbolAndDecl name scope =
  match scope.node with
  | S_Global model -> (resolveGlobalSymbol name scope, None)
  | S_Class cls -> resolveMemberSymbol cls name scope
  | S_Expr (E_Fold (op, var, collection, where, body)) -> (resolveExprSymbol op var collection where body name scope, None)
  | S_Expr _ -> raise UnexpectedError
  
(* resolution helpers -------------------------------------------------------------------*)

(* resolve a variable *)
and resolveVar name scope =
  match resolveExpressionSymbol name scope with
  | Var var -> var
  | _ -> raise UnexpectedError

(* resolve a field access *)
and resolveFieldAccess expr fname state =
  let cls = 
    match typeof expr state with
    | T_Ref cname
    | T_Class cname -> resolveClass cname state.scope
    | _ -> raise (InvalidFieldAccess (fname, typeToString (typeof expr state)))
  in
  let r =
    match resolveMemberSymbolOnly cls fname state.scope with
    | (Var var, Some cls) -> (var, cls)
    | (Param var, Some cls) -> (var, cls)
    | _ -> raise UnexpectedError
  in
  (snd(r), fst(r), snd(r))