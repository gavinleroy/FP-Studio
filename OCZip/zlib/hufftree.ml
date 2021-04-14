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

type 'a tree =
  | Empty
  | Leaf of 'a
  | Node of 'a tree * 'a tree

type bitlist = bool list

type decoder = int tree

type encoder = (int, bitlist, Int.comparator_witness) Map.t

(**********************************)
(*  functions on decoders (trees) *)
(**********************************)

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

let of_lens ps =
  try List.fold 
    (List.sort ps ~compare:(fun (a, b) (c, d) ->
         if b = d then compare a c
         else compare b d)) 
    ~init:Empty 
    ~f:(fun acc (v, l) -> build_at_depth acc v l 0)
    |> Some
  with TreeFull -> None

let rec to_sexp t =
  match t with
  | Empty -> "()"
  | Leaf i -> "(Leaf " ^ (Int.to_string i) ^ ")"
  | Node (l, r) ->
    "(Node " ^ (to_sexp l) ^ " " ^ (to_sexp r) ^ ")"

(***************************)
(*  functions on encoders  *)
(***************************)

let encode e v =
  Map.find e v

(* val bit_list_of_int: int -> int -> bitlist *)
(* The function works as follows: 
 * given an integer `b` which represents a byte
 * we push `len` bits into a list.
 *
 * The numbers are processed as follows:
 * 0b001110010 ~> 7 ~> [ #t, #t, #t, #f, #f, #t, #f ]*)
let bit_list_of_int b len =
  let is_odd = 
    fun x -> (x land 1) = 1 in
  let rec process n bt acc =
    if n = 0 then acc
    else process (n - 1) (bt lsr 1) ((is_odd bt) :: acc) in
  process len b []

let end_of_stream_byte =
  List.(range 0 7 >>| fun _ -> false)

(************************************************)
(* Lit Value  Bits  Codes                       *)
(* ---------  ----  -----                       *)
(*   0 - 143   8    00110000 through 10111111   *)
(* 144 - 255   9    110010000 through 111111111 *)
(* 256 - 279   7    0000000 through 0010111     *)
(* 280 - 287   8    11000000 through 11000111   *)
(************************************************)
let fixed_encoder =
  List.fold
    List.(range 0 288 >>| 
          fun x -> x, 
                   if x < 144 then bit_list_of_int (x + 0b00110000) 8
                   else if x < 256 then bit_list_of_int ((x - 144) + 0b110010000) 9
                   else if x < 280 then bit_list_of_int ((x - 256) + 0b0000000) 7
                   else bit_list_of_int ((x - 280) + 0b11000000) 8)
    ~init:(Map.empty(module Int))
    ~f:(fun acc (k, v) ->
        Map.add_exn acc ~key:k ~data:v)

