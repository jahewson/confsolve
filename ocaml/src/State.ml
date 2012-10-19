open Util

(* scope ************************************************************************)

(* AST nodes with scope *)
type scopedNode =
  | S_Global of ConfSolve.model     (* global *)
  | S_Class of ConfSolve.classDecl  (* class *)
  | S_Expr of ConfSolve.expr        (* expression, e.g. E_Fold *)

(* scope tree *)
type scope = {
  parent: scope option;   (* enclosing scope *)
  node: scopedNode;       (* AST node *)
}

(* state ************************************************************************)

(* translation state *)
type state = {
  model: ConfSolve.model;         (* ConfSolve AST *)
  counts: int StrMap.t;           (* object counts per-class *)
  subclasses: int list StrMap.t;  (* map of superclass names to subclass indices *)
  indexes: int StrMap.t;          (* current index per-class *)
  scope: scope;                   (* scope tree *)
  mzn_output: string list;        (* MiniZinc `output` variables *)
}