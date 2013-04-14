open ConfSolve
open SznSolution
open DeclBinding
open Counting
open State
open Util

exception MissingVariable of string

(* CSON *************************************************************************)

let convertModel (model : ConfSolve.model)
                 (state : State.state)
                 (solution : SznSolution.solution)
                 (params : CsonSolution.solution option)
                 (paths : ConfSolve.varName StrIntMap.t option)
                 (map : ConfSolve.varName StrIntMap.t)
                 (isJSON : bool) =
  
  let rec convertValue value t state =
    match (value, t) with

    | (V_Int i, T_Enum ename) -> 
        let enm = resolveEnum ename state.scope in
        if isJSON then
          "\"" ^ ename ^ "." ^ List.nth enm.elements (i - 1) ^ "\""
        else
          "\"" ^ ename ^ "." ^ List.nth enm.elements (i - 1) ^ "\""
				
    | (V_Int i, T_Ref cname) ->
        if isJSON then
          "\"@" ^ StrIntMap.find (cname, i) map ^ "\""
        else
          "ref " ^ StrIntMap.find (cname, i) map
    
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
    let state = { state with scope = pushScope (S_Class cls) state.scope } in
    let indent = indent + 1 in
    let (cson, state) =
      List.fold_left (fun (cson, state) (cls, mbr) ->
          match mbr with
        | Constraint _ -> (cson, state)
        | Enum _ | Class _ -> raise UnexpectedError
        | Var var | Param var ->
            let (cson', state) = convertVar (Some (id, cls)) var state indent in
            let cson' = (if cson = "" then "" else ",\n") ^ cson' in
            (cson ^ cson', state)
        | Block _ -> raise UnexpectedError
      ) ("", state) (allMembersWithClasses cls state.scope)
    in
		let cson = cson ^ "\n" in
    let pad = String.make ((indent - 1) * 2) ' ' in
    let cson =
      if isJSON
        then "{\n" ^ (pad ^ "  \"_class\": \"" ^ cls.name ^ "\",\n") ^ cson ^ pad ^ "}"
        else cls.name ^ " {\n" ^ cson ^ pad ^ "}"
		in (cson, state)

  and getValueHelper id_cls vname =
    try
      (* variable *)
      match id_cls with
      | None ->
          StrMap.find vname solution
      | Some (id, cls) ->
          let values = StrMap.find (cls.name ^ "_" ^ vname) solution in
          match values with
          | V_Array lst ->
              (try
                List.nth lst (id - 1)
              with _ ->
                print_endline ("ERROR: List.nth lst " ^ string_of_int (id - 1));
                print_endline ("FOR: " ^ cls.name ^ "_" ^ vname);
                print_endline ("REASON: this is probably not the CSON solution for this CSM model\n");
                raise Not_found)
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
		let fmt_vname = if isJSON then "\"" ^ vname ^ "\"" else vname in
    let (cson, state) =
      match t with
      | T_Class cname ->
          let (id, state) = newIndex cname state in
          let id = int_of_string id in
          let cls = (resolveClass cname state.scope) in
          let (cson, state) = convertObject id cls state indent in
          let cson = fmt_vname ^ ": " ^ cson in
          (cson, state)
  
      | T_Set(T_Class cname, _, ubound) ->
          let (indices, state) = newIndices cname ubound state in
          let (cson, state, _) = List.fold_left (fun (cson, state, i) id ->
            let id = int_of_string id in
            let cls = (resolveClass cname state.scope) in
            let (cson', state) = convertObject id cls state (indent + 1) in
            let cson = (if String.length cson = 0 then "" else cson ^ ",\n" ^ pad ^ "  ") ^ cson' in
            (cson, state, i + 1)
          ) ("", state, 0) indices
          in
          let cson = fmt_vname ^ ": [\n" ^  (pad ^ "  ") ^ cson ^ "\n" ^ pad ^ "]" in
          (cson, state)
  
      | _ ->
          let cson = fmt_vname ^ ": " ^ convertValue (getValue vname) t state in
          (cson, state)
    in
    (pad ^ cson, state)

  (* convertModel *)
  in
  let (cson, state) =
    List.fold_left (fun (cson, state) decl ->
      match decl with
      | Var var | Param var -> 
          let (cson', state) = convertVar None var state 1 in
          let cson' = (if cson = "" then "" else ",\n") ^ cson' in
          (cson ^ cson', state)
      | Class _ | Enum _ | Constraint _ -> (cson, state)
      | Block _ -> raise UnexpectedError
    ) ("", state) model.declarations
  in
  "{\n" ^ cson ^ "\n}"

(* name map ***********************************************************************)

let rec mapObject id cls state map path =
  let state = { state with scope = pushScope (S_Class cls) state.scope } in
  List.fold_left (fun (map, state) (cls, mbr) ->
    match mbr with
    | Constraint _ -> (map, state)
    | Enum _ | Class _ -> raise UnexpectedError
    | Var var | Param var -> mapVar (Some (id, cls)) var state map (path ^ "." ^ fst var)
    | Block _ -> raise UnexpectedError
  ) (map, state) (allMembersWithClasses cls state.scope)
  
and mapVar id_cls (vname, t) state map path =
  match t with
  | T_Class cname ->
      let (id, state) = newIndex cname state in
      let id = int_of_string id in
      
      let map = StrIntMap.add (cname, id) path map in
      (*print_endline (cname ^ " " ^ string_of_int id ^ " -> " ^ path);*)
      
      let map = StrIntMap.add ((rootClass (resolveClass cname state.scope) state.scope).name, id) path map in
      (*print_endline ((rootClass (resolveClass cname state.scope) state).name ^ " " ^ string_of_int id ^ " --> " ^ path);*)
      
      mapObject id (resolveClass cname state.scope) state map path
      
  | T_Set(T_Class cname, _, ubound) ->
      let (indices, state) = newIndices cname ubound state in
      let (map, state, _) = List.fold_left (fun (map, state, i) id ->
        let id = int_of_string id in
        let path = path ^ "[" ^ string_of_int i ^ "]" in
        
        let map = StrIntMap.add (cname, id) path map in
        (*print_endline (cname ^ " " ^ string_of_int id ^ " => " ^ path);*)
        
        let map = StrIntMap.add ((rootClass (resolveClass cname state.scope) state.scope).name, id) path map in
        (*print_endline ((rootClass (resolveClass cname state.scope) state.scope).name ^ " " ^ string_of_int id ^ " ==> " ^ path);*)
        
        let (map, state) = mapObject id (resolveClass cname state.scope) state map path in
        (map, state, i + 1)
      ) (map, state, 0) indices
      in
      (map, state) 
      
  | _ -> (map, state)

let buildNameMapHelper model state =
  let (map, state) =
    List.fold_left (fun (map, state) decl ->
      match decl with
      | Var var | Param var ->
          mapVar None var state map (fst var)
      | Class _ | Enum _ | Constraint _ -> (map, state)
      | Block _ -> raise UnexpectedError
    ) (StrIntMap.empty, state) model.declarations
  in map

(* public *)
let buildNameMap model =
  (* init *)
  let scope = { parent = None; node = S_Global model } in
  let state = { counts = StrMap.empty; indexes = StrMap.empty;
                scope = scope; subclasses = StrMap.empty; } in
  buildNameMapHelper model state
  
(* entry point ********************************************************************)

(* translates the model into a string of CSON *)
let toCSON model solution params paths isDebug =
  (* init *)
  let scope = { parent = None; node = S_Global model } in
  let state = { counts = StrMap.empty; indexes = StrMap.empty;
                scope = scope; subclasses = StrMap.empty; } in

  let map = buildNameMapHelper model state in
  convertModel model state solution params paths map false

(* translates the model into a string of JSON *)
let toJSON model solution params paths isDebug =
  (* init *)
  let scope = { parent = None; node = S_Global model } in
  let state = { counts = StrMap.empty; indexes = StrMap.empty;
                scope = scope; subclasses = StrMap.empty; } in

  let map = buildNameMapHelper model state in
  convertModel model state solution params paths map true