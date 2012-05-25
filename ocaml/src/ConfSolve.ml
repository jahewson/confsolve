type className = string
type fieldName = string
type varName = string
type enumName = string
type enumElement = string

type _type =
  | T_Symbol of string   (* untyped *)
  | T_Class of className (* typed *)
  | T_Enum of enumName   (* typed *)
  | T_Int
  | T_Bool
  | T_Range of int * int
  | T_Ref of className
  | T_Set of _type * int * int (* lbound, ubound *)

type op =
  | Eq | Neq | Gt | Ge | Lt | Le | In | Subset
  | Union | Intersection
  | And | Or | Implies | Iff
  | Add | Sub | Div | Mul | Pow | Mod

type foldOp =
  | ForAll
  | Exists
  | Sum

type expr =
  | E_Symbol of string  (* untyped *)
  | E_Var of varName    (* typed *)
  | E_Enum of enumName  (* typed *)
  | E_Access of expr * fieldName
  | E_Op of expr * op * expr
  | E_Card of expr (* e.size *)
  | E_Fold of foldOp * varName * expr * expr * expr (* collection, where, body *)
  | E_Neg of expr 
  | E_Not of expr
  | E_Bool of bool
  | E_Int of int
  | E_Paren of expr
  | E_BoolToInt of expr

type _constraint =
  | C_Where of expr
  | C_Maximise of expr

type varDecl = varName * _type

type classDecl = {
   name: className;
   super: className option;
   members: decl list;
   isAbstract: bool;
}

and enumDecl = {
   enumName: enumName;
   elements: enumElement list;
}

and decl =
 | Class of classDecl
 | Enum of enumDecl
 | Var of varDecl
 | Constraint of _constraint
 
and model = {
 declarations: decl list;
}