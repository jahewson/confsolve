open Util
open Str

(* CSON **************************************************************************)

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

(* ref target  ********************************************************************)

let rec csonRefId reference globals paths =
  let path = refToPath reference in
    let hits =
      StrIntMap.filter (fun key value ->
        value = path
      ) paths
    in
    let ((cname,id), _) = StrIntMap.choose hits in
    id

and csonRefValue reference globals =
  let path = refToPath reference in
  csonPathValue path globals false

and refToPath reference =
  match reference with
  | R_Var name ->  name
  | R_Access (r, name) -> refToPath r ^ "." ^ name
  | R_Index (r, index) -> refToPath r ^ "[" ^ string_of_int index ^ "]"

(* path lookup ********************************************************************)

(* gets a CSON value for a path *)
and csonPathValue path globals isQuiet =
  let parts = full_split (regexp "\\.\\|\\[\\|\\]") path in
  let parts =
    List.map (fun part ->
      match part with
      | Text part -> part
      | Delim part -> raise UnexpectedError
    ) (List.filter (fun part ->
      match part with
      | Text part -> true
      | Delim part -> false
    ) parts)
  in

  (* Now walk the path recursively... *)
  try
    List.fold_left (fun parent part ->
      match parent with
      | V_Set lst -> 
          List.nth lst (int_of_string part)
      | V_Object obj ->
          StrMap.find part obj.members
      | V_Bool _      | V_Int _
      | V_Ref _
      | V_Enum _ ->
          parent
    ) (V_Object { name = "global"; members = globals }) parts
  with
  | e ->
      if not isQuiet then
        output_string stderr ("Warning: CSON value not found `" ^ path ^ "`\n")
        else ();
      raise e