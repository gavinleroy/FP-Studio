(****************************)
(*        Gavin Gray        *)
(*    University of Utah    *)
(*    Spring 21 -- OCZip    *)
(****************************)

type 'a tree

type encoder

(* val decode: 'a tree -> < some bit data structure > -> 'a *) 

val fixed_encoder: encoder

val end_of_stream_byte: bool list

val encode: encoder -> int -> bool list option

(* val bit_list_of_int: int -> int -> bool list *)

(* val of_freqs: int * int list -> t *)

val of_lens: (int * int) list -> int tree option

val to_sexp: int tree -> string
