open ConfSolve
open State
open DeclBinding
open ExprBinding
open Counting
open Forward
open Util

(* translation ********************************************************************)

(* translates the model into a string of MiniZinc *)
let toMiniZinc model solution params paths showCounting hasComments noMinChangeConstraints =
  let prev =
    match (solution, paths) with
    | (Some s, Some p) -> Some (s,p)
    | _ -> None
  in
  
	let maximiseTerms = ref [] in						(* non-ignored terms to be maximised *)
	let maximiseTermsIgnored = ref [] in		(* ignored terms to be maximised *)
	let maximiseCount = ref 0 in						(* counter for generating maximised terms *)
	let mzn_output = ref [] in							(* buffer for MiniZinc `output` section *)

  (* CSON value to MiniZinc, almost but not quite a ConfSolve expression *)
  let rec csonToMz value paths globals state =
    match value with
    | CsonSolution.V_Bool true -> "true"
    | CsonSolution.V_Bool false -> "false"
    | CsonSolution.V_Int i -> string_of_int i
    | CsonSolution.V_Set lst ->
        "{" ^
        List.fold_left (fun acc elem ->
          let acc = acc ^ if String.length acc = 0 then "" else ", " in
          acc ^ csonToMz elem paths globals state
        ) "" lst
        ^ "}"
    | CsonSolution.V_Object obj ->
        (* constant, so we never need to write its old value *)
        raise UnexpectedError
    | CsonSolution.V_Ref reference ->
        let id = CsonSolution.csonRefId reference globals paths in
        string_of_int id
    | CsonSolution.V_Enum (ename, mname) -> 
        translateEnumExpr ename mname state

  (* translates an enum *)
  and translateEnumExpr ename mname state =
    let enm = resolveEnum ename state.scope in
    let (_, idx) = 
      List.fold_left (fun (i, found) elem ->
        if elem = mname then
          (i + 1, i)
        else
          (i + 1, found)
      ) (1, -1) enm.elements
    in
    if idx = -1 then
      raise (EnumMemberNotDefined (ename, mname))
    else
      string_of_int idx
          
  (* integer list to MiniZinc array literal,
     with collapsing to a range if contiguous *)
  and intListToMz list =
    if isContiguous list then
      string_of_int (List.hd list) ^ ".." ^ string_of_int (List.nth list ((List.length list) - 1))
    else
      "{" ^
        List.fold_left (fun mzn elem ->
          if String.length mzn = 0 then
            string_of_int elem
          else
            mzn ^ "," ^ string_of_int elem
        ) "" list
      ^ "}"

  (* translates a type *)
  and translateType t state =
    match t with
    | T_Infer
    | T_Symbol _ -> raise UnexpectedError
    | T_Int -> "int"
    | T_Bool -> "bool"
    | T_BInt set ->
        if isSetContiguous set then
          string_of_int (IntSet.min_elt set) ^ ".." ^ string_of_int (IntSet.max_elt set)
        else
          "{" ^ (IntSet.fold (fun elem acc  ->
              if String.length acc = 0 then 
                string_of_int elem
              else
                acc ^ "," ^ string_of_int elem
            ) set "") ^ "}"
    | T_Class cname ->
        let cls = (resolveClass cname state.scope) in
        if cls.isAbstract then raise (AbstractInstance (cls.name)) else ();
        if (count cname state) = 0 then raise (NoInstancesOfClass cname) else ();
        "1.." ^ string_of_int (count cname state)
    | T_Enum ename ->
        "1.." ^ string_of_int (List.length (resolveEnum ename state.scope).elements)
    | T_Ref cname ->
        intListToMz (getSubclassIds cname state)
    | T_Set (t, lbound, ubound) ->
        "set of " ^ translateType t state

  (* list to MiniZinc array literal *)
  and listToMz list =
    List.fold_left (fun mzn elem ->
      if String.length mzn = 0 then
        elem
      else
        mzn ^ "," ^ elem
    ) "" list
        
  (* records variables for MiniZinc `output` section *)
  and output mz_vname =
    mzn_output := ("\n  \"" ^ mz_vname ^ " = \", show(" ^ (mz_vname) ^ "), \";\\n\"") :: !mzn_output
       
   (* translates a binary operator *)
  and translateOp op =
    match op with
    | Eq -> "=" | Neq -> "!=" | Gt -> ">" | Ge -> ">=" | Lt -> "<" | Le -> "<="
    | Intersection -> "intersection" | Union -> "union" | Subset -> "subset" | In -> "in"
    | And -> "/\\" | Or -> "\\/" | Implies -> "->" | Iff -> "<->"
    | Add -> "+" | Sub -> "-" | Div -> "div" | Mul -> "*" | Pow -> raise UnexpectedError | Mod -> "mod"
    
  (* checks if a variable is quantified *)
  and isVarQuantified name state =
    match state.scope with
    | { node = S_Expr (E_Fold (op, (vname, t), collection, where, body)); parent = _} ->
        if name = vname then true
        else isVarQuantified name { state with scope = popScope state.scope }
    | { node = S_Expr _; parent = _ } -> raise UnexpectedError
    | _ -> false
    
  (* translates an expression *)
  and translateExpr (expr : expr) (isOld : bool) (state : state) =
    match expr with
    | E_Symbol _ -> raise UnexpectedError
  
    | E_Var (vname, t, scope) ->
        (match scope with
        | None ->
            (if isOld && not (isVarQuantified vname state) then "old_" else "")
            ^ vname
        | Some cname ->
            if vname = "this" then
              "this"
             else
               (if isOld && not (isVarQuantified vname state) then "old_" else "")
               ^ cname ^ "_" ^ vname ^ "[this]")
               
    | E_Access (E_Enum ename, mname) ->
        translateEnumExpr ename mname state
      
    | E_Enum _ ->
        ""
    | E_Access (e, mname) ->
        let (cls, (_,mbr), clsDecl) = resolveFieldAccess e mname state in
        (if isOld then "old_" else "") ^ cls.name ^ "_" ^ mname ^ "[" ^ translateExpr e isOld state ^ "]"

    | E_Fold (op, var, collection, where, body) ->
        let (vname, vtype) = var in
        let bodyState = { state with scope = pushScope (S_Expr expr) state.scope } in
        (match typeof collection state with
        | T_Set (T_Set (_,_,_),_,_) -> raise (SetOfSet vname)
        | T_Set (t, lbound, ubound) ->
            let isConst =
              match t with
              | T_Class cname -> ubound = lbound
              | _ -> false
            in
            let mzBody = 
              let guard =
                if isConst then
                  E_Bool true
                else
                  (E_Op (E_Var (vname, vtype, None), In, collection))
              in
              let guard =
                match (guard, where) with
                | (E_Bool true, E_Bool true) -> E_Bool true
                | (E_Bool true, _) -> where
                | (_, E_Bool true) -> guard
                | (_, _) -> (E_Op (guard, And, where))
              in
              if guard <> E_Bool true then
                match op with
                | ForAll | Exists -> translateExpr guard isOld bodyState ^ " -> " ^ translateExpr (E_Paren body) isOld bodyState
                | Sum -> "bool2int(" ^ translateExpr guard isOld bodyState ^ ") * " ^ translateExpr (E_Paren body) isOld bodyState
              else
                translateExpr (E_Paren body) isOld bodyState
            in
            let mznRange =
              match t with
              | T_Class cname ->
                if isConst then
                  translateExpr collection isOld bodyState
                else
                  "1.." ^ string_of_int (count cname state)
              | T_Ref cname -> "1.." ^ string_of_int (count cname state)
              | T_Bool -> "{true,false}"
              | T_BInt _ 
              | T_Enum _ ->
                  translateType t state
              | T_Symbol _
              | T_Infer
              | T_Set _
              | T_Int -> 
                  raise UnexpectedError
            in
            (match op with | ForAll -> "forall" | Exists -> "exists" | Sum -> "sum")
            ^ " (" ^ vname ^ " in " ^ mznRange ^ ") (\n"
            ^ "    " ^ mzBody ^ "\n"
            ^ "  )"
        | _ -> raise (ExpectedSet1 vname)) 

    | E_Set elist ->
        raise (NotImplemented "set literals")
        (* requires constants *)
        (*let mzn =
          List.fold_left (fun mzn e ->
            let mzn = mzn ^ if String.length mzn = 0 then "" else "," in
            mzn ^ (translateExpr e isOld state)
          ) "" elist
        in
        "{" ^ mzn ^ "}"*)

    | E_Old e ->
        (match (prev, isOld) with
        | (None, _) -> print_endline "Error: model contains the ~ operator, but the -s flag was not used";
                  exit 1
        | (Some _, true) -> failwith "Error: nested ~ expression"
        | _ -> translateExpr e true state)
        (* Q: error if e is a quantified var? *)
      
    | E_Op (e1, Pow, e2) -> "pow (" ^ translateExpr e1 isOld state ^ "," ^ translateExpr e2 isOld state ^ ")"
    | E_Op (e1, op, e2) -> translateExpr e1 isOld state ^ " " ^ translateOp op ^ " " ^ translateExpr e2 isOld state
    | E_Card e -> "card(" ^ translateExpr e isOld state ^ ")"
    | E_Neg e -> "-" ^ translateExpr e isOld state
    | E_Not e -> "not (" ^ translateExpr e isOld state ^ ")"
    | E_Bool b -> string_of_bool b
    | E_Int i -> string_of_int i
    | E_BoolToInt e -> "bool2int(" ^ translateExpr e isOld state ^ ")"
    | E_Abs e -> "abs(" ^ translateExpr e isOld state ^ ")"
    | E_Paren e -> "(" ^ translateExpr e isOld state ^ ")"
  
  (* creates a cardinality constraint for a set *)
  and cardConstraint var lbound ubound state =
    if lbound = ubound then
      E_Op (E_Card (E_Var var), Eq, E_Int lbound)
    else
      let elb = E_Op (E_Card (E_Var var), Ge, E_Int lbound) in
      let eub = E_Op (E_Card (E_Var var), Le, E_Int ubound) in
      E_Op (elb, And, eub)
    
  (* translates a class-member variable *)
  and translateMemberVar cls var state =
    let (vname, t) = var in
    output (cls.name  ^ "_" ^ vname);
    let ccount = count cls.name state in
    let mzn = if hasComments then "\n% ." ^ vname ^ " as " ^ typeToString t ^ "\n" else "" in
    let mzn = mzn ^ "array[1.." ^ string_of_int ccount ^ "] of " in
    let (mzn, state) =
      match t with
      | T_Class cname ->
          let (indices, state) = newIndices cname ccount state in
          let mzn = mzn ^ translateType t state ^ ": " ^ cls.name ^ "_" ^ vname ^ " = [" ^ listToMz indices ^ "];\n" in
          (* NOTE: constant, no need for old values :) *)
          (mzn, state)
      | T_Set (T_Class cname, lbound, ubound) ->
          if lbound <> ubound then
            raise (NotImplemented "variable cardinality sets of objects")
          else
            let (indices, state) =
              (List.fold_left (fun (arr, state) elem ->
                let (indices, state) = newIndices cname ubound state in
                let set = "{" ^ (listToMz indices) ^ "}" in
                (arr @ [set], state)
              ) ([], state) (seq 1 ccount))
            in
            let mzn = mzn ^ translateType t state ^ ": " ^ cls.name ^ "_" ^ vname ^ " = [" ^ listToMz indices ^ "];\n" in
            (* NOTE: constant, no need for old values :) *)
            (mzn, state)
      | T_Set (_, lbound, ubound) ->
          let mzn = mzn ^ "var " ^ translateType t state ^ ": " ^ cls.name ^ "_" ^ vname ^ ";\n" in
          let expr = cardConstraint (vname, t, Some cls.name) lbound ubound state in
          let mzn = 
            if lbound != -1 then
              mzn ^ "constraint forall (this in 1.." ^ string_of_int ccount ^ ") (" ^ translateExpr expr false state ^ ");\n"
            else mzn
          in
          (mzn, state)
      | _ ->
          let mzn = mzn ^ "var " ^ translateType t state ^ ": " ^ cls.name ^ "_" ^ vname ^ ";\n" in
          (mzn, state)
    in
    oldMemberVar mzn ccount cls var state

  (* translates old value of a class-member variable *)
  and oldMemberVar mzn ccount cls var state =
    let (vname, t) = var in
    let mzn =
      match t with
      | T_Class _
      | T_Set (T_Class _, _, _) -> mzn
      | _ ->
          match prev with
          | None -> mzn
          | Some (globals, paths) ->
            (* get all objs by using ids 1..count in the path map, with cname *)
            mzn ^ "array[1.." ^ string_of_int ccount ^ "] of var " ^
             translateType t state ^ ": " ^ "old_" ^ cls.name ^ "_" ^ vname ^ " = [" ^
              List.fold_left (fun mzn id ->
                let path = StrIntMap.find (cls.name, id) paths in
								let delim = if String.length mzn = 0 then "" else ", " in
								try
									let parent = CsonSolution.csonPathValue path globals in
	                match parent with
	                | CsonSolution.V_Object obj ->
	                    let value = StrMap.find vname obj.CsonSolution.members in
	                    mzn ^ delim ^ csonToMz value paths globals state
	                | _ -> raise UnexpectedError
								with _ ->
                  output_string stderr ("Warning: missing solution variable: `" ^ vname ^ "` in class `" ^ cls.name ^ "`\n");
									mzn ^ delim ^ "_" (* _ is MiniZinc's anonymous variable *)
              ) "" (seq 1 (count (rootClass cls state.scope).name state))
            ^ "];\n" 
    in
    (mzn, state)
  
  (* translates a class-member paramater *)
  and translateMemberParam cls (vname, t) state =
    output (cls.name  ^ "_" ^ vname);
    match (params, paths) with
    | (Some params, Some paths) ->
        (match t with
        | T_Class _
        | T_Set (T_Class _, _, _) ->
            (* constant *)
            translateMemberVar cls (vname, t) state
        | _ ->
            let ccount = count cls.name state in
            let mzn =
            "array[1.." ^ string_of_int ccount ^ "] of " ^
             translateType t state ^ ": " ^ cls.name ^ "_" ^ vname ^ " = [" ^
              List.fold_left (fun mzn id ->
                let path = StrIntMap.find (cls.name, id) paths in
                (try
                  let parent = CsonSolution.csonPathValue path params in
                  (match parent with
                  | CsonSolution.V_Object obj ->
                      let delim = if String.length mzn = 0 then "" else ", " in
                      let value = StrMap.find vname obj.CsonSolution.members in
                      mzn ^ delim ^ csonToMz value paths params state
                  | _ ->
                      raise UnexpectedError)
                 with Not_found ->
                    output_string stderr ("Error: missing param: `" ^ vname ^ "` in class `" ^ cls.name ^ "`\n");
                    exit 1)
              ) "" (seq 1 (count (rootClass cls state.scope).name state))
              ^ "]; /* param */\n"
            in
          (mzn, state))
    | (_, _) ->
        failwith "this model uses paramaters but the -p flag was not set"
  
  (* translates a global variable *)
  and translateGlobalVar var state =
    let (vname, t) = var in
    output vname;
    let mzn = if hasComments then "\n% " ^ vname ^ " as " ^ typeToString t ^ "\n" else "" in

    let (mzn, state) =
      match t with
      | T_Class cname ->
          let (idx, state) = newIndex cname state in
          let mzn = mzn  ^ translateType t state ^ ": " ^ vname ^ " = " ^ idx ^ ";\n" in
          (mzn, state)
      | T_Set (T_Class cname, lbound, ubound) ->
          if lbound <> ubound then
            raise (NotImplemented "variable cardinality sets of objects")
          else
            let (indices, state) = newIndices cname ubound state in
            let mzn = mzn ^ translateType t state ^ ": " ^ vname in
            let mzn = mzn ^ " = {" ^ listToMz indices ^ "};\n" in
            (mzn, state)
      | T_Set (_, lbound, ubound) ->
          let mzn = mzn ^ "var " ^ translateType t state ^ ": " ^ vname ^ ";\n" in
          let expr = cardConstraint (vname, t, None) lbound ubound state in
          let mzn = 
            if lbound != -1 then
              mzn ^ "constraint " ^ translateExpr expr false state ^ ";\n"
            else mzn
          in
          (mzn, state)
      | _ ->
          let mzn = mzn ^ "var " ^ translateType t state ^ ": " ^ vname ^ ";\n" in
          (mzn, state)
          (* constants *)
          (*let expr = getVarConstantExpr (Var var) state in
          let mzn = mzn ^ (if isVarConstant (Var var) state then "" else "var ") ^ translateType t state ^ ": " ^ vname ^ 
            (match expr with
            | Some expr -> " = " ^ translateExpr expr false state ^ ";\n"
            | None -> ";\n")
          in
          (mzn, state)*)
    in
    oldGlobalVar mzn var state
  
  and oldGlobalVar mzn var state =
    let (vname, t) = var in
    let mzn =
      match t with
      | T_Class _
      | T_Set (T_Class _, _, _) -> mzn  (* NOTE: constants :) *)
      | _ ->
        match prev with
        | None -> mzn
        | Some (globals, paths) ->
            try
              let value = StrMap.find vname globals in
              mzn ^ translateType t state ^ ": old_" ^ vname ^ " = " ^
                csonToMz value paths globals state ^ ";\n"
            with
            | Not_found ->
                output_string stderr ("Warning: missing solution variable: `" ^ vname ^ "`\n");
                mzn
    in
    (mzn, state)
  
  and translateGlobalParam (vname, t) state =
    output vname;
    match (params, paths) with
    | (Some params, Some paths) ->
      (match t with
      | T_Class _
      | T_Set (T_Class _, _, _) ->
          (* constant *)
          translateGlobalVar (vname, t) state
      | _ ->
          let mzn =
            try
              let value = StrMap.find vname params in
              translateType t state ^ ": old_" ^ vname ^ " = " ^
                csonToMz value paths params state ^ ";\n"
            with
            | Not_found ->
                output_string stderr ("Error: missing paramater: `" ^ vname ^ "`\n");
                exit 1
            in (mzn, state))
    | (_, _) ->
        failwith "this model uses paramaters but the -p flag was not set"
      
  (* translates a class-level constraint *)    
  and translateClassConstraint cls con state =
    let mzIds = intListToMz (getSubclassIds cls.name state) in
    match con with
    | C_Where expr ->
        let mzn =
          "\nconstraint\n  forall (this in " ^ mzIds ^ ") (\n    " ^ translateExpr expr false state ^ "\n  );\n"
        in (mzn, state)
		| C_Maximise expr | C_MinChange_Maximise expr ->
				let vname = "objective_" ^ string_of_int (!maximiseCount + 1) in
				(if noMinChangeConstraints
					then maximiseTermsIgnored := !maximiseTermsIgnored @ [vname]
					else maximiseTerms := !maximiseTerms @ [vname]);
				maximiseCount := !maximiseCount + 1;
        let mzn =
          "\nvar int: " ^ vname ^ ";\n" ^
          "constraint\n " ^ vname ^ " = sum (this in " ^ mzIds ^ ") (\n    " ^ translateExpr expr false state ^ "\n  );\n\n"
        in (mzn, state)
    
  (* translates a global constraint *)
  and translateGlobalConstraint con state =
    match con with
    | C_Where expr ->
        let mzn =
          (if hasComments then "\n% global" else "") ^
          "\nconstraint\n  " ^ translateExpr expr false state ^ ";\n"
        in (mzn, state)
    | C_Maximise expr | C_MinChange_Maximise expr ->
				let vname = "objective_" ^ string_of_int (!maximiseCount + 1) in
				(if noMinChangeConstraints
					then maximiseTermsIgnored := !maximiseTermsIgnored @ [vname]
					else maximiseTerms := !maximiseTerms @ [vname]);
				maximiseCount := !maximiseCount + 1;
        let mzn =
          (if hasComments then "\n% global" else "") ^
          "\nvar int: " ^ vname ^ ";\n" ^
					"constraint " ^ vname ^ " = " ^ translateExpr expr false state ^ ";\n\n"
        in (mzn, state)

  (* raise error on non-abstract inheritance *)
  and checkForAbstractInheritance cls state =
    match cls.super with
    | Some cname ->
        let super = (resolveClass cname state.scope) in
        if not super.isAbstract then
          raise (NotImplemented ("`" ^ cls.name ^ "` inherits non-abstract class `" ^ cname ^ "`"))
        else ()
    | None -> ()

  (* translates a class *)
  and translateClass cls state =
    if rawCount cls.name state = 0 then
      ("", state)
    else (
      checkForAbstractInheritance cls state;
      let state = { state with scope = pushScope (S_Class cls) state.scope } in
      let mzn = if hasComments then "\n%\n% class " ^ cls.name ^ "\n%\n" else ""
      in
      let (mzn, state) = 
        List.fold_left (fun (mzn, state) mbr ->
          let (mzn', state) =
            match mbr with
            | Var var -> translateMemberVar cls var state
            | Param var -> translateMemberParam cls var state
            | Constraint con -> translateClassConstraint cls con state
            | Enum _ | Class _ | Block _ -> raise UnexpectedError
          in (mzn  ^ mzn', state)
        ) (mzn, state) cls.members
      in
      (mzn, { state with scope = popScope state.scope })
    )

  and translateEnum enm state =
    ("", state)

  (* translates the entire model *)
  and translateModel state =
    List.fold_left (fun (mzn, state) decl ->
      let (mzn', state) =
        match decl with
        | Var var -> translateGlobalVar var state
        | Param var -> translateGlobalParam var state
        | Class cls -> translateClass cls state
        | Enum enm -> translateEnum enm state
        | Constraint con -> translateGlobalConstraint con state
        | Block _ -> raise UnexpectedError
      in (mzn  ^ mzn', state)
    ) ("", state) model.declarations
    
  (* entry point ********************************************************************)
  in
  
  (* init *)
  let scope = { parent = None; node = S_Global model } in
  let state = { counts = StrMap.empty; indexes = StrMap.empty;
                scope = scope; subclasses = StrMap.empty; } in
  (* 1st pass: count objects *)
  let state = countModel showCounting model state in
  if showCounting then
    (printCounts state;
    "")
  else
	  (* 2nd pass: translate to MiniZinc *)
		
		(* maximiseTerms *)
    let (mzn, state) = translateModel state in
    if List.length !maximiseTerms > 0 then
      output "total_objective"
    	else ();
    let mzn =
      mzn ^
      if List.length !maximiseTerms > 0 then
        "var int: total_objective;\n" ^
        "constraint total_objective = " ^
        List.fold_left (fun acc term ->
          acc ^ (if acc = "" then "" else " + ") ^ term
        ) "" !maximiseTerms
        ^ ";\n"
      else ""
    in
		
		(* maximiseTermsIgnored  *)
    if List.length !maximiseTermsIgnored > 0 then
      output "ignored_objective"
    	else ();
		let mzn =
      mzn ^
      if List.length !maximiseTermsIgnored > 0 then
        "var int: ignored_objective;\n" ^
        "constraint ignored_objective = " ^
        List.fold_left (fun acc term ->
          acc ^ (if acc = "" then "" else " + ") ^ term
        ) "" !maximiseTermsIgnored
        ^ ";\n"
      else ""
    in
		
		(* goal *)
    let solve =
      if List.length !maximiseTerms = 0 then
        "satisfy" 
      else
        "maximize total_objective"
    in
    mzn ^ "\nsolve " ^ solve ^ ";\n\n"
        ^ "output ["
        ^ (listToMz !mzn_output)
        ^ "];"