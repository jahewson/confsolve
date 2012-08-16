open ConfSolve
open State
open DeclBinding
open ExprBinding
open Util
open Counting

(* type checking ******************************************************************)

(* wrapper around typeof in Binding *)
let checkExpr expr state =
  ignore (typeof expr state)
  
let checkConstraint con state =
  match con with
  | C_Where expr -> checkExpr expr state
  | C_Maximise expr -> checkExpr expr state
  
let checkClassDecl cls state =
  let state = { state with scope = pushScope (S_Class cls) state.scope } in
  List.iter (fun mbr ->
    match mbr with
    | Var var -> () (* requires constants - if var is a T_Int make sure it has a constant assignment *)
    | Constraint con -> checkConstraint con state
    | Enum _ | Class _ -> raise UnexpectedError
  ) cls.members
    
(* type check expressions - this might be more useful returning a list *)
let typeCheck csModel =
  let scope = { parent = None; node = S_Global csModel } in
  let state = { counts = StrMap.empty; indexes = StrMap.empty; model = csModel; 
                scope = scope; subclasses = StrMap.empty; show_counting = false; 
                mzn_output = []; comments = false; maximise_count = 0; set_count = 0 } in
  
  (* 1st pass: count objects (needed for `typeof`) *)
  let state = countModel state
  in
  (* 2nd pass: type check *)
  List.iter (fun decl ->
    match decl with
    | Var var -> () (* requires constants - if var is a T_Int make sure it has a constant assignment *)
    | Class cls -> checkClassDecl cls state
    | Enum enm -> ()
    | Constraint con -> checkConstraint con state
  ) state.model.declarations