(* exceptions *********************************************************************)

exception UnexpectedError
exception NotImplemented of string

(* modules ************************************************************************)

(* map of string *)
module StrMap = Map.Make(String)

(* set of int *)
module IntSet = Set.Make( 
  struct
    let compare = Pervasives.compare
    type t = int
  end)

(* functions *********************************************************************)

(* check if an IntSet is contiguous *)
let isSetContiguous set =
  let (_,_,is) =
    IntSet.fold (fun elem (prev, idx, is) ->
      if idx = 0 then
        (elem, 1, true)
      else
        (elem, idx + 1, is && elem = prev + 1)
    ) set (0, 0, true)
  in is

(* check if a numeric list is contiguous *)
let isContiguous list =
  let (_,_,is) =
    List.fold_left (fun (prev, idx, is) elem ->
      if idx = 0 then
        (elem, 1, true)
      else
        (elem, idx + 1, is && elem = prev + 1)
    ) (0, 0, true) list
  in is

(* make an IntSet from a range *)
let makeSet min max =
  let rec next cur range =
    if cur = max then
      IntSet.add cur range
    else
      next (cur + 1) (IntSet.add cur range)
  in
  next min IntSet.empty

(* generates a list of integers min..max *)
let seq min max =
  let rec next cur range =
    if cur = max then
      range @ [cur]
    else
      next (cur + 1) (range @ [cur])
  in
  next min []
  


