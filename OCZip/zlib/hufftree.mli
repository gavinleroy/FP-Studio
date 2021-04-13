(****************************)
(*        Gavin Gray        *)
(*    University of Utah    *)
(*    Spring 21 -- OCZip    *)
(****************************)

type 'a t

(* val of_freqs: int * int list -> t *)

val of_lens: (int * int) list -> int t

val to_sexp: int t -> string

