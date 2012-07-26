open Util

type solutions =
  solution list

and solution =
  value StrMap.t

and value =
  | V_Bool of bool
  | V_Int of int
  | V_Set of value list
  | V_Object of obj
  | V_Ref of reference
  | V_Enum of string * string
  
and obj = {
  name: string;
  members: value StrMap.t;
}

and reference =
  | R_Var of string
  | R_Access of reference * string
  | R_Index of reference * int