open ConfSolve
open Util

(* apply init/change blocks *******************************************************)

let applyBlockClass cls kind =
  let members =
    List.fold_left (fun members mbr ->
      match mbr with
      | Block (k, ctrs) when k = kind -> members @ List.map (fun ctr -> Constraint ctr) ctrs
      | Block _ -> members
      | _ -> mbr :: members
    ) [] cls.members
  in Class { cls with members = members }

let applyBlocks (ast : model)
								(kind : blockKind)
								: model =
  let decls =
    List.fold_left (fun decls decl ->
      match decl with
      | Class cls -> applyBlockClass cls kind :: decls
      | Block (k, ctrs) when k = kind -> decls @ List.map (fun ctr -> Constraint ctr) ctrs
      | Block _ -> decls
      | _ -> decl :: decls
    ) [] ast.declarations
  in { declarations = decls }

(* remove init/change blocks ******************************************************)

let removeBlockClass cls =
  let members =
    List.fold_left (fun members mbr ->
      match mbr with
      | Block _ -> members
      | _ -> mbr :: members
    ) [] cls.members
  in Class { cls with members = members }

let removeBlocks (ast : model)
								 : model =
  let decls =
    List.fold_left (fun decls decl ->
      match decl with
      | Class cls -> removeBlockClass cls :: decls
      | Block _ -> decls
      | _ -> decl :: decls
    ) [] ast.declarations
  in { declarations = decls }
 
(* min-changes ********************************************************************)

let applyMinChanges (ast : model) 
										(globals: CsonSolution.solution)
										(paths : varName StrIntMap.t)
										: model =

  let rec applyMinChangesVar cls var =
    let (vname, t) = var in
    let decls = [Var var] in
    let decls =
      match t with
      | T_Class _
      | T_Set (T_Class _, _, _) ->
          decls (* constant *)
      | T_Enum _ | T_Bool 
      | T_Int | T_BInt _ 
      | T_Ref _ | T_Set _ ->
          let cname =
            match cls with
            | None -> None
            | Some cls -> Some cls.name
          in
          let e = E_Op (E_Var (vname, t, cname), Eq, E_Old (E_Var (vname, t, cname))) in
          Constraint (C_MinChange_Maximise (E_BoolToInt e)) :: decls
      | T_Symbol _| T_Infer ->
          raise UnexpectedError
    in decls

  and applyMinChangesClass cls =
    let members =
      List.fold_left (fun members mbr ->
        match mbr with
        | Var var -> members @ applyMinChangesVar (Some cls) var
        | _ -> mbr :: members
      ) [] cls.members
    in Class { cls with members = members }
  
  in (* applyMinChanges *)
    let decls =
      List.fold_left (fun decls decl ->
        match decl with
        | Var var -> decls @ applyMinChangesVar None var
        | Class cls -> applyMinChangesClass cls :: decls
        | _ -> decl :: decls
      ) [] ast.declarations
    in { declarations = decls }