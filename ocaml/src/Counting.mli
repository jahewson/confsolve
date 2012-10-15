open Util

val getSubclassIds : StrMap.key -> State.state -> int list
val countModel : bool -> State.state -> State.state
val hasCounts : State.state -> bool
val printCounts : State.state -> unit
val rawCount : StrMap.key -> State.state -> int
val count : ConfSolve.className -> State.state -> int
val newIndex : ConfSolve.className -> State.state -> string * State.state
val newIndices : ConfSolve.className -> int -> State.state -> string list * State.state