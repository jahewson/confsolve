open ConfSolve
open State
open Binding
open Util

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
  | E_BoolToInt e -> E_BoolToInt (forwardExpr e state)
  | E_Bool _ | E_Int _ -> expr
  | E_Set elist -> E_Set (List.map (fun e -> forwardExpr e state) elist)
      
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
let resolveForwardDecls csModel =
  let scope = { parent = None; node = S_Global} in
  let state = { counts = StrMap.empty; indexes = StrMap.empty; model = csModel; 
                scope = scope; subclasses = StrMap.empty; show_counting = false; 
                mzn_output = []; comments = false; maximise_count = 0; set_count = 0 } in
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
  { declarations =
      List.map (fun decl ->
        match decl with
        | Class cls -> Class (forwardClassUse cls state)
        | Constraint con -> Constraint (forwardConstraint con state)
        | Var _ | Enum _ -> decl
      ) state.model.declarations
    }