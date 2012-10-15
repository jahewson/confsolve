open Util
  
type className = string
type fieldName = string
type varName = string
type enumName = string
type enumElement = string

type _type =
  | T_Infer              (* untyped *)
  | T_Symbol of string   (* untyped *)
  | T_Class of className (* typed *)
  | T_Enum of enumName   (* typed *)
  | T_Bool
  | T_Int                (* unbounded int *)
  | T_BInt of IntSet.t   (* bounded int *)
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

type var =
  varName * _type * className option

type expr =
  | E_Symbol of string  (* untyped *)
  | E_Var of var (* typed *)
  | E_Enum of enumName  (* typed *)
  | E_Access of expr * fieldName
  | E_Op of expr * op * expr
  | E_Card of expr (* e.size *)
  | E_Fold of foldOp * (varName * _type) * expr * expr * expr (* collection, where, body *)
  | E_Neg of expr 
  | E_Not of expr
  | E_Old of expr
  | E_Bool of bool
  | E_Int of int
  | E_Set of expr list
  | E_Paren of expr
  | E_BoolToInt of expr
  | E_Abs of expr
  
type _constraint =
  | C_Where of expr
  | C_Maximise of expr

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

and varDecl =
  varName * _type

and blockKind =
  | Change
  | Init

and decl =
 | Class of classDecl
 | Enum of enumDecl
 | Var of varDecl
 | Param of varDecl
 | Constraint of _constraint
 | Block of blockKind * _constraint list
 
and model = {
 declarations: decl list;
}