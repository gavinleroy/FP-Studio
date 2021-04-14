(****************************)
(*        Gavin Gray        *)
(*    University of Utah    *)
(*    Spring 21 -- OCZip    *)
(****************************)

type t

val create: unit -> t

val has_byte: t -> bool 

val is_empty: t -> bool

val enqueue: bool -> t -> t

val enqueue_all: bool list -> t -> t

val dequeue: t -> bool option * t

(* val enqueue_byte: int -> t -> t *)

val dequeue_byte: t -> int option * t

val dequeue_byte_force: t -> int * t

