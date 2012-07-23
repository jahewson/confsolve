open Util

type solutions =
  solution list

and solution =
  value StrMap.t

and value =
  | V_Bool of bool
  | V_Int of int
  | V_Array of value list
  | V_Set of value list
  | V_Range of int * int