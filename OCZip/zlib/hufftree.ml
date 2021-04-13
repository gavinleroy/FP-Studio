(****************************)
(*        Gavin Gray        *)
(*    University of Utah    *)
(*    Spring 21 -- OCZip    *)
(****************************)

open Core

(* let test = [] *)

(* val of_freqs: int * int list -> t *)

exception TreeFull

exception Fatal

type 'a t =
  | Empty
  | Leaf of 'a
  | Node of 'a t * 'a t

(* val build_at_depth: 'a tree option -> 'a -> int -> int -> 'a tree option *)
let rec build_at_depth nd v d ch =
  if ch > d then raise Fatal
  else
    match nd with
    | Leaf _ -> raise TreeFull
    | Empty ->
      if d = ch then Leaf v
      else Node ((build_at_depth Empty v d (ch + 1)), Empty)
    | Node (lo, ro) ->
      try Node (build_at_depth lo v d (ch + 1), ro)
      with TreeFull -> Node (lo, build_at_depth ro v d (ch + 1))

(* val of_lens: (int * int) list -> int tree *)
let of_lens ps =
  List.fold 
    (List.sort ps ~compare:(fun (a, b) (c, d) ->
         if b = d then compare a c
         else compare b d)) 
    ~init:Empty 
    ~f:(fun acc (v, l) -> build_at_depth acc v l 0)

let rec to_sexp t =
  match t with
  | Empty -> "()"
  | Leaf i -> "(Leaf " ^ (Int.to_string i) ^ ")"
  | Node (l, r) ->
    "(Node " ^ (to_sexp l) ^ " " ^ (to_sexp r) ^ ")"

