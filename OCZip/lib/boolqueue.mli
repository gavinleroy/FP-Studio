(****************************)
(*        Gavin Gray        *)
(*    University of Utah    *)
(*    Spring 21 -- OCZip    *)
(****************************)

type bqueue

val create: unit -> bqueue

val enqueue: bool -> bqueue -> bqueue

val dequeue: bqueue -> (bool option) * bqueue

val has_byte: bqueue -> bool 

val is_empty: bqueue -> bool

