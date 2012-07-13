open Util

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
  set_count: int;                 (* count of set literal variables *)
  scope: scope;                   (* scope tree *)
  mzn_output: string list;        (* MiniZinc `output` variables *)
  show_counting: bool;            (* debugging - print the object counts *)
  comments: bool;                 (* debugging - comment MiniZinc output *)
}

(* scope -------------------------------------------------------------------------*)

(* pushes a scope to the state *)
let pushScope node state =
  { state with scope = { parent = Some state.scope; node = node; } }

(* pops a scope from the state *)
let popScope state =
  match state.scope.parent with
  | Some parent -> { state with scope = parent }
  | None -> raise UnexpectedError