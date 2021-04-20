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
    (* | Pointer of int * int *)

  (* NOTE what does this care about?
   * - number of lookback bytes
   * - number of lookahead bytes
   * *)
  val create: back:int -> ahead:int -> str:int option Stream.t -> t

  val find_match: t -> int option Stream.t -> result * t

end

