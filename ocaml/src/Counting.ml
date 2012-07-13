open ConfSolve
open State
open Binding
open Util

(* counting ***********************************************************************)

(* increment count of key in lst *)
let incr key lst =
  (*print_endline ("+1 " ^ key);*)
  if StrMap.mem key lst then
    StrMap.add key ((StrMap.find key lst) + 1) lst
  else
    StrMap.add key 1 lst

(* get the current object-count for class `cls` *)
let getCurCount cname state =
  try
    StrMap.find cname state.counts
  with
  | Not_found -> 0
  
(* add a subclass of `super` *)
let addSubclass super sub state =
  { state with
      subclasses =
        if StrMap.mem super state.subclasses then
          StrMap.add super ((StrMap.find super state.subclasses) @ [sub]) state.subclasses
        else
          StrMap.add super (sub :: []) state.subclasses
  }
 
let getSubclassIds cname state =
  try
    StrMap.find cname state.subclasses
  with
  | Not_found ->
      print_endline cname;
      raise Not_found
 
(* get the root class of a subclass *)
let rec rootClass cls state =
  match cls.super with
  | Some cname -> rootClass (resolveClass cname state) state
  | None -> cls
  
(* increment object count *)
let rec countObj cls state =
  (* increment the root count, and fetch the new id *)
  let state = { state with counts = incr (rootClass cls state).name state.counts } in
  let id = getCurCount (rootClass cls state).name state 
  in
  (* always a subclass of itself *)
  let state = addSubclass cls.name id state
  in
  match cls.super with
  | Some sname ->
    (* count this subclass, as only the root was counted above *)
    let state = { state with counts = incr cls.name state.counts }
    in 
    (* now count its superclasses *)
    countSuperObj id (resolveClass sname state) state
  | None -> state
 
(* helper - counts superclasses *)
and countSuperObj id cls state =
    let root = rootClass cls state in
    if root = cls then
      (* root already counted by countObj, so we only need to track the subclasss *)
      addSubclass root.name id state
    else
      (* increment the superclass count *)
      let state = { state with counts = incr cls.name state.counts } 
      in
      (* track the subclasss *)
      let state = addSubclass cls.name id state
      in
      (* next superclass *)
      match cls.super with
      | Some sname -> countSuperObj id (resolveClass sname state) state
      | None -> state
    
(* count objects for a varDecl *)
let rec countVarDecl (vname, t) state =
  match t with
  | T_Class cname ->
      if state.show_counting then print_endline ("\n" ^ vname ^ ": " ^ cname) else ();
      let cls = resolveClass cname state in
      if cls.isAbstract then raise (AbstractInstance (vname ^ ": " ^ cname)) else ();
      let state = countObj cls state in
      List.fold_left (fun state mbr ->
        match mbr with
        | Var v -> countVarDecl v state
        | _ -> state 
      ) state (allMembers cls state)
      
  | T_Set (T_Class cname, lbound, ubound) ->
      if state.show_counting then print_endline ("\n" ^ vname ^ ": " ^ cname ^ "[" ^ string_of_int ubound ^ "]") else ();
      let cls = resolveClass cname state in
      List.fold_left (fun state elem ->
        let state = countObj cls state in
        List.fold_left (fun state mbr ->
          match mbr with
          | Var v -> countVarDecl v state
          | _ -> state
        ) state (allMembers cls state)
      ) state (seq 1 ubound)
      
  | _ -> state

(* count all objects in the model *)
let countModel state =
  let state = { state with counts = StrMap.add "!hasCounts" 1 state.counts } in
  List.fold_left (fun state d ->
    match d with
    | Var v -> countVarDecl v state
    | _ -> state
  ) state state.model.declarations

(* for debugging *)
let printCounts state =
  print_endline "\n------------\n";
  StrMap.iter (fun k v ->
    Printf.printf "%s: %d\n" k v
  ) state.counts
  
(* index generation ***************************************************************)

(* get the object-count for `cls` *)
let rawCount cname state =
  try
    StrMap.find cname state.counts
  with
  | Not_found -> 0

(* get the object-count for root(`cls`) *)
let count cname state =
  let cname = (rootClass (resolveClass cname state) state).name in
  rawCount cname state
  
(* generates a new index for an object of type root(`cls`) *)
let newIndex cname state =
  let cname = (rootClass (resolveClass cname state) state).name
  in
  let indexes' = incr cname state.indexes in
  let str = string_of_int (StrMap.find cname indexes') in
  (str, { state with indexes = indexes' })
    
(* generates a list of `num` new indices *)
let newIndices cname num state =
  let rec next i indices state =
    let (index, state) = newIndex cname state in
    let indices =  indices @ [index] in
    if i < num then
      next (i+1) indices state
    else
      (indices, state)
  in
  next 1 [] state