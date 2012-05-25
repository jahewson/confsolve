module StrMap : Map.S with type key = string

val seq : int -> int -> int list

val getSubclassIds : StrMap.key -> State.state -> int list
val countModel : State.state -> State.state
val printCounts : State.state -> unit
val rawCount : StrMap.key -> State.state -> int
val count : ConfSolve.className -> State.state -> int
val newIndex : ConfSolve.className -> State.state -> string * State.state
val newIndices : ConfSolve.className -> int -> State.state -> string list * State.state
