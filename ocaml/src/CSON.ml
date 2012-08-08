open ConfSolve
open SznSolution
open Binding
open Counting
open State
open Util

module PathMap = Map.Make(struct type t = string * int let compare = compare end)

exception MissingVariable of string

(* CSON *************************************************************************)

let convertModel state solution map =
  
  let rec convertValue value t state =
    match (value, t) with

    | (V_Int i, T_Enum ename) -> 
        let enm = resolveEnum ename state in
        ename ^ "." ^ List.nth enm.elements (i - 1)
  
    | (V_Int i, T_Ref cname) ->
        "ref " ^ PathMap.find (cname, i) map
      
    | (V_Bool true, _) -> "true"
    | (V_Bool false, _) -> "false"
    | (V_Int i, _) -> string_of_int i
    | (V_Set lst, T_Set (t',_,_)) ->
        "[" ^
        List.fold_left (fun acc elem ->
          let acc = acc ^ if String.length acc = 0 then "" else ", " in
          acc ^ convertValue elem t' state
        ) "" lst
        ^ "]"
    | (V_Range (lb, ub), T_Set (t',_,_)) ->
        "[" ^
        List.fold_left (fun acc num ->
          let acc = acc ^ if String.length acc = 0 then "" else ", " in
          acc ^ convertValue (V_Int num) t' state
        ) "" (Util.seq lb ub)
        ^ "]"
    | (V_Array _, _)
    | (V_Set _, _) 
    | (V_Range _, _) -> raise UnexpectedError 

  and convertObject id cls state indent =
    let state = pushScope (S_Class cls) state in
    let indent = indent + 1 in
    let (cson, state) =
      List.fold_left (fun (cson, state) (cls, mbr) ->
          match mbr with
        | Constraint _ -> (cson, state)
        | Enum _ | Class _ -> raise UnexpectedError
        | Var var ->
            let (cson', state) = convertVar (Some (id, cls)) var state indent in
            (cson ^ cson', state)
      ) ("", state) (allMembersWithClasses cls state)
    in
    let pad = String.make ((indent - 1) * 2) ' ' in
    let cson = cls.name ^ " {\n" ^ cson ^ pad ^ "}" in
    (cson, state)

  and getValueHelper id_cls vname =
    try
      match id_cls with
      | None ->
          StrMap.find vname solution
      | Some (id, cls) ->
          let values = StrMap.find (cls.name ^ "_" ^ vname) solution in
          match values with
          | V_Array lst -> List.nth lst (id - 1)
          | _ -> raise UnexpectedError
     with
    | Not_found ->
        match id_cls with
        | None ->
            raise (MissingVariable ("Error: variable `" ^ vname ^ "` not in solution"))
        | Some (id, cls) ->
            raise (MissingVariable ("Error: variable `" ^ (cls.name ^ "_" ^ vname) ^ "` not in solution"))

  and convertVar id_cls (vname, t) state indent =
    let getValue = getValueHelper id_cls in
    let pad = String.make (indent * 2) ' ' in
    let (cson, state) =
      match t with
      | T_Class cname ->
          let (id, state) = newIndex cname state in
          let id = int_of_string id in
          let cls = (resolveClass cname state) in
          let (cson, state) = convertObject id cls state indent in
          let cson = vname ^ ": " ^ cson ^ ",\n" in
          (cson, state)
  
      | T_Set(T_Class cname, _, ubound) ->
          let (indices, state) = newIndices cname ubound state in
          let (cson, state, _) = List.fold_left (fun (cson, state, i) id ->
            let id = int_of_string id in
            let cls = (resolveClass cname state) in
            let (cson', state) = convertObject id cls state (indent + 1) in
            let cson = (if String.length cson = 0 then "" else cson ^ ",\n" ^ pad ^ "  ") ^ cson' in
            (cson, state, i + 1)
          ) ("", state, 0) indices
          in
          let cson = vname ^ ": [\n" ^  (pad ^ "  ") ^ cson ^ pad ^ "],\n" in
          (cson, state)
  
      | _ ->
          let cson = vname ^ ": " ^ convertValue (getValue vname) t state ^ ",\n" in
          (cson, state)
    in
    (pad ^ cson, state)

  (* convertModel *)
  in
  let (cson, state) =
    List.fold_left (fun (cson, state) decl ->
      match decl with
      | Var var -> 
          let (cson', state) = convertVar None var state 1 in
          (cson ^ cson', state)
      | Class _ | Enum _ | Constraint _ -> (cson, state)
    ) ("", state) state.model.declarations
  in
  "{\n" ^ cson ^ "}"

(* name map ***********************************************************************)

let rec mapObject id cls state map path =
  let state = pushScope (S_Class cls) state in
  List.fold_left (fun (map, state) (cls, mbr) ->
    match mbr with
    | Constraint _ -> (map, state)
    | Enum _ | Class _ -> raise UnexpectedError
    | Var var -> mapVar (Some (id, cls)) var state map (path ^ "." ^ fst var)
  ) (map, state) (allMembersWithClasses cls state)
  
and mapVar id_cls (vname, t) state map path =
  match t with
  | T_Class cname ->
      let (id, state) = newIndex cname state in
      let id = int_of_string id in
      
      let map = PathMap.add (cname, id) path map in
      (*print_endline (cname ^ " " ^ string_of_int id ^ " -> " ^ path);*)
      
      let map = PathMap.add ((rootClass (resolveClass cname state) state).name, id) path map in
      (*print_endline ((rootClass (resolveClass cname state) state).name ^ " " ^ string_of_int id ^ " --> " ^ path);*)
      
      mapObject id (resolveClass cname state) state map path
      
  | T_Set(T_Class cname, _, ubound) ->
      let (indices, state) = newIndices cname ubound state in
      let (map, state, _) = List.fold_left (fun (map, state, i) id ->
        let id = int_of_string id in
        let path = path ^ "[" ^ string_of_int i ^ "]" in
        
        let map = PathMap.add (cname, id) path map in
        (*print_endline (cname ^ " " ^ string_of_int id ^ " => " ^ path);*)
        
        let map = PathMap.add ((rootClass (resolveClass cname state) state).name, id) path map in
        (*print_endline ((rootClass (resolveClass cname state) state).name ^ " " ^ string_of_int id ^ " ==> " ^ path);*)
        
        let (map, state) = mapObject id (resolveClass cname state) state map path in
        (map, state, i + 1)
      ) (map, state, 0) indices
      in
      (map, state) 
      
  | _ -> (map, state)

let buildNameMapHelper state =
  let (map, state) =
    List.fold_left (fun (map, state) decl ->
      match decl with
      | Var var ->
          mapVar None var state map (fst var)
      | Class _ | Enum _ | Constraint _ -> (map, state)
    ) (PathMap.empty, state) state.model.declarations
  in map

(* public *)
let buildNameMap csModel =
  (* init *)
  let scope = { parent = None; node = S_Global} in
  let state = { counts = StrMap.empty; indexes = StrMap.empty; model = csModel; 
                scope = scope; subclasses = StrMap.empty; show_counting = false; 
                mzn_output = []; comments = false; maximise_count = 0; set_count = 0 } in
  buildNameMapHelper state
  
(* entry point ********************************************************************)

(* translates the model into a string of CSON *)
let toCSON csModel solution isDebug =
  (* init *)
  let scope = { parent = None; node = S_Global} in
  let state = { counts = StrMap.empty; indexes = StrMap.empty; model = csModel; 
                scope = scope; subclasses = StrMap.empty; show_counting = false; 
                mzn_output = []; comments = false; maximise_count = 0; set_count = 0 } in

  let map = buildNameMapHelper state in
  convertModel state solution map