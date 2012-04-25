module StrMap = Map.Make(String)

type className = string
type fieldName = string
type varName = string

type _type =
  | T_Int
  | T_Bool
  | T_Range of int * int
  | T_Class of className
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
  | E_Var of varName
  | E_Access of expr * fieldName
  | E_Op of expr * op * expr
  | E_Card of expr (* e.size *)
  | E_Fold of foldOp * varName * expr * expr * expr (* collection, where, body *)
  | E_Neg of expr 
  | E_Not of expr
  | E_Bool of bool
  | E_Int of int
  | E_Paren of expr

type _constraint =
  | C_Where of expr
  | C_Maximise of expr

type varDecl = varName * _type

type memberDecl =
  | M_Var of varDecl
  | M_Constraint of _constraint

type classDecl = {
   name: className;
   super: className option;
   members: memberDecl list;
   isAbstract: bool;
 }
 
type globalDecl =
 | G_Class of classDecl
 | G_Var of varDecl
 | G_Constraint of _constraint

type model = {
 declarations: globalDecl list;
}