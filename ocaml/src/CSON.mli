module PathMap : Map.S with type key = string * int

val toCSON : ConfSolve.model -> SznSolution.solution -> bool -> string
val buildNameMap : ConfSolve.model -> ConfSolve.varName PathMap.t
(*val csonPathValue : string -> CsonSolution.solution -> CsonSolution.value
val csonRefValue : CsonSolution.reference -> CsonSolution.solution -> CsonSolution.value
val csonRefId : CsonSolution.reference -> CsonSolution.solution -> ConfSolve.varName PathMap.t -> int*)