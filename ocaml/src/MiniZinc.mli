val toMiniZinc:
  ConfSolve.model -> 
  CsonSolution.solution option ->
  ConfSolve.varName CSON.PathMap.t option ->
  bool ->
  bool ->
  string