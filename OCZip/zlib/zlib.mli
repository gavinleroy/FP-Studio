(****************************)
(*        Gavin Gray        *)
(*    University of Utah    *)
(*    Spring 21 -- OCZip    *)
(****************************)

(* module Huffman = Hufftree *)

(** given a stream of bytes compute the CRC32 *)
val crc32: int option Stream.t -> int

(** given a byte update the crc *)
val crc32_update: int -> int -> int

val deflate: int option Stream.t -> int * int option Stream.t

