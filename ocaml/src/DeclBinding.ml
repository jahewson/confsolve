open ConfSolve
open State
open Util

(* binding ************************************************************************)

exception SymbolNotDefined of string                (* symbol not defined *)
exception AbstractInstance of string                (* cannot create an instance of an abstract class *)

(* scope ---------------------------------------------------------------------------*)

(* pushes a scope *)
let pushScope node scope =
  { parent = Some scope; node = node; }

(* pops a scope *)
let popScope scope =
  match scope.parent with
  | Some parent -> parent
  | None -> raise UnexpectedError
  
(* resolution ---------------------------------------------------------------------*)

(* resolve global symbol *)
let rec resolveGlobalSymbol name scope =
  match scope.node with
  | S_Global model ->
      (let found = List.fold_left (fun found decl ->
        match found with
        | Some _ -> found
        | None ->
            match decl with
            | Var (vname, t) -> if vname = name then Some decl else None
            | Enum enm -> if enm.enumName = name then Some decl else None
            | Class cls -> if cls.name = name then Some decl else None
            | Constraint _ -> found
      ) None model.declarations
      in
      match found with
      | Some f -> f
      | None -> raise (SymbolNotDefined name))
  | _ -> raise UnexpectedError
  
(* resolves a class-level symbol, returning both the variable and class which defined it *)
and resolveMemberSymbolImpl noGlobal cls name scope =
  if name = "this" then
    (Var ("this", T_Class cls.name), Some cls)
  else
    let found = List.fold_left (fun found mbr ->
      match found with
      | Some _ -> found
      | None ->
          match mbr with
          | Var (vname, t) -> if vname = name then Some mbr else None
          | Constraint _ -> found
          | Enum _ | Class _ -> raise UnexpectedError
    ) None cls.members
    in
    match found with
    | Some mbr -> (mbr, Some cls)
    | None ->
        match cls.super with
        | Some cname -> resolveMemberSymbolImpl noGlobal (resolveClass cname scope) name scope
        | None ->
          if noGlobal then
            raise (SymbolNotDefined name)
          else
            (resolveGlobalSymbol name (popScope scope), None)

(* resolve a member symbol *)
and resolveMemberSymbol cls name scope =
  resolveMemberSymbolImpl false cls name scope
  
(* resolve a member symbol, with no global fallback *)
and resolveMemberSymbolOnly cls name scope =
  resolveMemberSymbolImpl true cls name scope
    
(* resolve any symbol *)
and resolveDeclSymbol name scope =
  match scope.node with
  | S_Global model -> resolveGlobalSymbol name scope
  | S_Class cls -> fst (resolveMemberSymbol cls name scope)
  | S_Expr _ -> raise UnexpectedError
  
(* resolution helpers -------------------------------------------------------------------*)
  
(* gets the global scope *)
and getGlobalScope scope =
  match scope with
  | { parent = Some p; node = _ } -> getGlobalScope p
  | s -> s

(* resolve an enum *)
and resolveEnum name scope =
  match resolveGlobalSymbol name (getGlobalScope scope) with
  | Enum enm -> enm
  | _ -> raise UnexpectedError
  
(* resolve a member variable to a (var, class) *)
and resolveMemberVar cls name scope =
  match resolveMemberSymbol cls name scope with
  | (Var var, optCls) -> (var, optCls)
  | _ -> raise UnexpectedError

(* resolve a class *)
and resolveClass name scope =
  match resolveGlobalSymbol name (getGlobalScope scope) with
  | Class c -> c
  | _ -> raise UnexpectedError

(* gets all inherited members of a class *)
let rec allMembers cls scope =
  match cls.super with
  | Some cname -> cls.members @ allMembers (resolveClass cname scope) scope
  | None -> cls.members
    
(* gets all inherited members of a class, as (class,member) pairs *)
let rec allMembersWithClasses cls scope =
  match cls.super with
  | Some cname -> (makeMemberClassList cls) @ allMembersWithClasses (resolveClass cname scope) scope
  | None -> (makeMemberClassList cls)
  
and makeMemberClassList cls =
  List.map (fun mbr -> (cls, mbr)) cls.members
  
(* get the root class of a subclass *)
and rootClass cls scope =
  match cls.super with
  | Some cname -> rootClass (resolveClass cname scope) scope
  | None -> cls