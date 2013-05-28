open Util

val toCSON :
  ConfSolve.model ->
    SznSolution.solution ->
    CsonSolution.solution option ->
    ConfSolve.varName StrIntMap.t option ->
    bool ->
    string

val toJSON :
  ConfSolve.model ->
    SznSolution.solution ->
    CsonSolution.solution option ->
    ConfSolve.varName StrIntMap.t option ->
    bool ->
    string
		
val buildNameMap : ConfSolve.model -> ConfSolve.varName StrIntMap.t
(*val csonPathValue : string -> CsonSolution.solution -> CsonSolution.value
val csonRefValue : CsonSolution.reference -> CsonSolution.solution -> CsonSolution.value
val csonRefId : CsonSolution.reference -> CsonSolution.solution -> ConfSolve.varName PathMap.t -> int*)