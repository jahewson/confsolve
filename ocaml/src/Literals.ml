open ConfSolve
open State
open DeclBinding
open ExprBinding
open Util
open Counting

(* decompose literals ***********************************************************)

let rec walkExpr expr decls state =
  match expr with
  | E_Symbol _ -> raise UnexpectedError
  
  | E_Fold (op, name, collection, where, body) ->
      let e =
        let (collection, decls, state) = walkExpr collection decls state in
        let bodyState = { state with scope = pushScope (S_Expr (E_Fold (op, name, collection, where, body))) state.scope } in
        let (where, decls, state) = walkExpr where decls bodyState in
        let (body, decls, state) = walkExpr body decls bodyState in
        E_Fold (op, name, collection, where, body)
      in (e, decls, state)
  | E_Op (e1, op, e2) ->
      let (e1, decls, state) = (walkExpr e1 decls state) in
      let (e2, decls, state) = (walkExpr e2 decls state) in
      let e = E_Op (e1, op, e2) in
      (e, decls, state)
  | E_Card e ->
      let (e, decls, state) = (walkExpr e decls state) in
      let e = E_Card e in
      (e, decls, state)
  | E_Neg e ->
      let (e, decls, state) = (walkExpr e decls state) in
      let e = E_Neg e in 
      (e, decls, state)
  | E_Not e ->
      let (e, decls, state) = (walkExpr e decls state) in
      let e = E_Not e in
      (e, decls, state)
  | E_Old e ->
      let (e, decls, state) = (walkExpr e decls state) in
      let e = E_Old e in
      (e, decls, state)
  | E_Paren e ->
      let (e, decls, state) = (walkExpr e decls state) in
      let e = E_Paren e in
      (e, decls, state)
  | E_Access (e, mname) ->
      let (e, decls, state) = (walkExpr e decls state) in
      let e = E_Access (e, mname) in
      (e, decls, state)
  | E_BoolToInt e ->
      let (e, decls, state) = (walkExpr e decls state) in
      let e = E_BoolToInt e in
      (e, decls, state)
  | E_Abs e ->
      let (e, decls, state) = (walkExpr e decls state) in
      let e = E_Abs e in
      (e, decls, state)
  | E_Bool _ | E_Int _ | E_Var _ | E_Enum _ -> 
      (expr, decls, state)
      
  | E_Set elist ->
      (* constants *)
      (*let isConst = isConstant expr state in*)
      
      (* note: because of object literals, set literals can be nested, so we have to walk them: *)
      let (elist, decls, state) =
        List.fold_left (fun (elist, decls, state) e ->
          let (e, decls, state) = (walkExpr e decls state) in
          (elist @ [e], decls, state)
        ) ([], decls, state) elist
      in
      
      let isConst = false in  (* <-- disabled (requires constants) *)
      
      if isConst then
        (* special case - the expression is a constant *)
        (E_Set elist, decls, state)
      else
        (* new variable *)
        let state = { state with set_count = state.set_count + 1 } in
        let t = typeof expr state in
        let vname = "set__" ^ string_of_int state.set_count in
        let vexpr = E_Var (vname, t, None) in
        let v = Var ("set__" ^ string_of_int state.set_count, t) in
        let decls = decls @ [v] in
      
        (* new constraints *)
        let e =
          List.fold_left (fun e e' ->
            let e' = E_Op (e', In, vexpr) in
            match e with
            | None -> Some e'
            | Some e -> Some (E_Op (e, And, e'))
          ) None elist 
        in
        let e =
          match e with
          | Some e -> e
          | None -> raise UnexpectedError
        in
        let c = Constraint (C_Where e) in
        let decls = decls @ [c] in
        (vexpr, decls, state)
      
let walkConstraint con decls state =
  match con with
  | C_Where expr -> 
      let (e, decls, state) = (walkExpr expr decls state) in
      (C_Where e, decls, state)
  
  | C_Maximise expr ->
      let (e, decls, state) = (walkExpr expr decls state) in
      (C_Maximise e, decls, state)
  
let walkClassDecl cls decls state =
  let state = { state with scope = pushScope (S_Class cls) state.scope } in
  
  let (members, decls, state) =
    List.fold_left (fun (members, decls, state) mbr ->
      match mbr with
      | Var _ | Param _ -> (members @ [mbr], decls, state)
      | Constraint con ->
          let (con, decls, state) = walkConstraint con decls state in
          let members = members @ [Constraint con] in
          let members = members @ decls in
          (members, decls, state)
          
      | Enum _ | Class _ | Block _ -> raise UnexpectedError
    ) ([], decls, state) cls.members
  in
  ({ cls with members = members }, decls, state)

let decomposeLiterals csModel =
  let scope = { parent = None; node = S_Global csModel } in
  let state = { counts = StrMap.empty; indexes = StrMap.empty; model = csModel; 
                scope = scope; subclasses = StrMap.empty;
                mzn_output = []; maximise_count = 0; set_count = 0 } 
  in
  (* 1st pass: count objects (needed for `typeof`) *)
  let state = countModel false state
  in
  (* 2nd pass: translate literals *)
  { declarations =
      let (decls, _) =
        List.fold_left (fun (decls, state) decl ->
          match decl with
          | Class cls ->
            let (cls, newDecls, state) = walkClassDecl cls [] state in
            let decls = decls @ newDecls in
            let decls = decls @ [Class cls] in
            (decls, state)

          | Constraint con ->
              let (con, newDecls, state) = walkConstraint con [] state in
              let decls = decls @ newDecls in
              let decls = decls @ [Constraint con] in
              (decls, state)
          
          | Var _ | Param _ | Enum _ | Block _ -> (decls @ [decl], state)
        ) ([], state) state.model.declarations
      in decls
    }