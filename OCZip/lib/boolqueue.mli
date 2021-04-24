(****************************)
(*        Gavin Gray        *)
(*    University of Utah    *)
(*    Spring 21 -- OCZip    *)
(****************************)

module Boolqueue : sig

  type t

  (********* general helperss *********)

  val create: t

  val of_byte_list: int list -> t

  val has_byte: t -> bool 

  val is_empty: t -> bool

  val len_in_bytes: t -> int

  (********* enqueuing values *********)

  val enqueue: t -> bool -> t

  val enqueue_all: t -> bool list -> t

  val enqueue_byte: t -> int -> t

  val enqueue_all_byte_aligned: t -> bool list -> t

  val enqueue_from: t -> t -> t

  (********* dequeuing values *********)

  val dequeue: t -> bool option * t

  val dequeue_byte: t -> int option * t

  val dequeue_byte_force: t -> int * t

end

