open Util

val toMiniZinc:
  ConfSolve.model -> 
  CsonSolution.solution option ->
  CsonSolution.solution option ->
  ConfSolve.varName StrIntMap.t option ->
  bool ->
  bool ->
	bool ->
  string