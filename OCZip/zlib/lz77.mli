(****************************)
(*        Gavin Gray        *)
(*    University of Utah    *)
(*    Spring 21 -- OCZip    *)
(****************************)

module LZ77 : sig

  type t

  type result =
    | Empty
    | Literal of int
    | Pointer of int * int

  val create: t

  val find_match: t -> int option Stream.t -> result * t

end

