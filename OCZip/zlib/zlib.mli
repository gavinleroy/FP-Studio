(****************************)
(*        Gavin Gray        *)
(*    University of Utah    *)
(*    Spring 21 -- OCZip    *)
(****************************)

(* module Huffman = Hufftree *)

exception ZlibExn of string

type nonrec compression_level =
  | Zero (** Store only, no compression done *)
  | One  (** Only encode with huffman codes. 
             Why include this? Educational purposes *)
  | Two  (** Compress with huffman codes and lz77 encodings *)

val crc32: int option Stream.t -> int

val crc32_update: int -> int -> int

val deflate: ?level:compression_level -> int option Stream.t -> int * int option Stream.t

