open ConfSolve
open State
open DeclBinding
open ExprBinding
open Util

(* forward declarations ***********************************************************)

let rec forwardExpr expr scope =
  match expr with
  | E_Var _ | E_Enum _ -> raise UnexpectedError

  | E_Symbol name ->
      (match resolveExpressionSymbolAndDecl name scope with
      | (Param (vname, t), Some cls)
      | (Var (vname, t), Some cls) -> E_Var (vname, t, Some cls.name)
      | (Param (vname, t), None)
      | (Var (vname, t), None) -> E_Var (vname, t, None)
      | (Enum _, _) -> E_Enum name
      | (Class _, _) | (Constraint _, _) | (Block _, _)-> raise UnexpectedError)
      
  | E_Fold (op, var, collection, where, body) ->
      let collection = forwardExpr collection scope in
      let bodyScope = pushScope (S_Expr (E_Fold (op, var, collection, where, body))) scope in
      (* generally, inference must be done as another pass,
         however, the case for only fold variables is simple. *)
      let var = resolveVar (fst var) bodyScope in
      let where = forwardExpr where bodyScope in
      let body = forwardExpr body bodyScope in
      E_Fold (op, var, collection, where, body)

  | E_Op (e1, op, e2) -> E_Op ((forwardExpr e1 scope), op, (forwardExpr e2 scope))
  | E_Card e -> E_Card (forwardExpr e scope)
  | E_Neg e -> E_Neg (forwardExpr e scope)
  | E_Not e -> E_Not (forwardExpr e scope)
  | E_Old e -> E_Old (forwardExpr e scope)
  | E_Paren e -> E_Paren (forwardExpr e scope)
  | E_Access (e, mname) -> E_Access ((forwardExpr e scope), mname)
  | E_BoolToInt e -> E_BoolToInt (forwardExpr e scope)
  | E_Abs e -> E_Abs (forwardExpr e scope)
  | E_Bool _ | E_Int _ -> expr
  | E_Set elist -> E_Set (List.map (fun e -> forwardExpr e scope) elist)
  
let forwardConstraint con scope =
  match con with
  | C_Where expr -> C_Where (forwardExpr expr scope)
  | C_Maximise expr -> C_Maximise (forwardExpr expr scope)
	| C_MinChange_Maximise expr -> C_MinChange_Maximise (forwardExpr expr scope)

let rec forwardType t scope =
  match t with
  | T_Symbol name ->
      (match resolveDeclSymbol name scope with
       | Class cls -> T_Class name
       | Enum enm -> T_Enum name
       | Constraint _ | Var _ | Param _ | Block _ -> raise (ExpectedType name))
  | T_Set (t, lb, ub) ->
      T_Set (forwardType t scope, lb, ub)
  | _ -> t

let forwardVar (vname, t) scope =
  (vname, forwardType t scope)
  
let forwardEnum enm scope =
  enm
  
let forwardClassDecl cls scope =
  let scope = pushScope (S_Class cls) scope in
  { cls with members =
      List.map (fun mbr ->
        match mbr with
        | Var (vname, t) -> Var (forwardVar (vname, t) scope)
        | Param (vname, t) -> Param (forwardVar (vname, t) scope)
        | Constraint con -> mbr
        | Enum _ | Class _ | Block _ -> raise UnexpectedError
      ) cls.members
    }

let forwardClassUse cls scope =
  let scope = pushScope (S_Class cls) scope in
  { cls with members =
      List.map (fun mbr ->
        match mbr with
        | Constraint con -> Constraint (forwardConstraint con scope)
        | Var _ | Param _ -> mbr
        | Enum _ | Class _ | Block _ -> raise UnexpectedError
      ) cls.members
    }

(* resolve forward declarations i.e. T_Symbol and E_Symbol *)
let resolveForwardDecls model =
  let scope = { parent = None; node = S_Global model } in
  let model =
    (* 1st pass - declarations *)
    { declarations =
        List.map (fun decl ->
          match decl with
          | Var var -> Var (forwardVar var scope)
          | Param var -> Param (forwardVar var scope)
          | Class cls -> Class (forwardClassDecl cls scope)
          | Enum enm -> Enum (forwardEnum enm scope)
          | Constraint con -> decl
          | Block _ -> decl
        ) model.declarations
      }
  in
  (* now update the scope with the new model *)
  let scope = { parent = None; node = S_Global model } in
  (* 2nd pass - constraints *)
  { declarations =
      List.map (fun decl ->
        match decl with
        | Class cls -> Class (forwardClassUse cls scope)
        | Constraint con -> Constraint (forwardConstraint con scope)
        | Var _ | Param _ | Enum _ | Block _ -> decl
      ) model.declarations
    }