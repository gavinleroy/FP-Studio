(****************************)
(*        Gavin Gray        *)
(*    University of Utah    *)
(*    Spring 21 -- OCZip    *)
(****************************)

open Core
open Stdint
open Lib

module Huffman = Hufftree

exception ZlibExn of string

type nonrec compression_level =
  | Zero (** Store only, no compression done *)
  | One  (** Only encode with huffman codes, why include this? educational purposes *)
  | Two  (** Compress with huffman codes and lz77 encodings *)

(**************************)
(*         CRC32          *)
(**************************)

let crc32_update' crc byte = 
  let rec inner_loop n crc = 
    if n = 0 then crc
    else (Uint32.neg (Uint32.logand crc (Uint32.of_int 1)))
         |> Uint32.logand (Uint32.of_int 0xEDB88320)
         |> Uint32.logxor (Uint32.shift_right crc 1) 
         |> inner_loop (n - 1) in 
  Uint32.of_int byte
  |> Uint32.logxor crc
  |> inner_loop 8 

let crc32_update crc byte = 
  (Uint32.to_int 
     (Uint32.lognot 
        (crc32_update' 
           (Uint32.lognot (Uint32.of_int crc)) 
           byte)))

let crc32 stream = 
  let rec outer_loop crc = 
    match Stream.next stream with
    | None -> crc
    | Some b -> 
      crc32_update' crc b
      |> outer_loop  in
  Uint32.of_int 0xFFFFFFFF
  |> outer_loop 
  |> Uint32.lognot 
  |> Uint32.to_int 

(****************************************)
(*     Deflate Utils and Algorithm      *)
(****************************************)

(*----------------------------------------*)
(*----------------------------------------*)
(*                Steps                   *)
(*----------------------------------------*)
(*     1. write byte stream in ALL        *)
(*       uncompressed blocks              *)
(*----------------------------------------*)
(*----------------------------------------*)
(* XXX 2. only compress bytes with static *)
(*       huffman encoding                 *)
(*----------------------------------------*)
(*----------------------------------------*)
(*     3. get the lz77 algorithm working  *)
(*       with a sliding window            *) 
(*----------------------------------------*)
(*----------------------------------------*)

(* let max_fix_block_size = *) 
(*   Uint16.to_int Uint16.max_int *)

(* let deflate_store_only instr queue = *) 
(*   let rec eq_all qu = *)
(*     match Stream.next instr with *)
(*     | None -> qu *)
(*     | Some i -> eq_all (Boolqueue.enqueue_byte i qu) *)
(*   Boolqueue.enqueue_byte queue static_huffman_block_header *)
(*   |> eq_all *)
  (* let rec make_list q = *) 
  (*   match Boolqueue.dequeue_byte q with *)
  (*   | None, _ -> [None] *)
  (*   | Some ob, nq -> Some ob :: make_list nq in *)
  (* let written_bytes, fullq = enqueue_file instr (Boolqueue.create ()) in *)
  (* written_bytes, Stream.of_list (make_list fullq) *)

(** the header bits for a block using static huffman codes *)
let static_huffman_block_header =
  [true; true; false;]

(* val deflate_huffman_only buffer-of-bytes queue *)
let deflate_huffman_only instr queue =
  let rec eq_all qu =
    match Stream.next instr with
    | None -> Boolqueue.enqueue_all qu Huffman.end_of_stream_byte
    | Some i -> 
      match Huffman.encode Huffman.fixed_encoder i with
      | None -> raise (ZlibExn "invalid byte found in input stream")
      | Some b -> eq_all (Boolqueue.enqueue_all qu b) in 
  Boolqueue.enqueue_all queue static_huffman_block_header
  |> eq_all

(** main entry point for the deflate algorithm  *)
let deflate ?(level=One) instr =
  (* TODO : read in a buffer of bytes, doing some heuristics at the
   * same time. Switch on whether this block is better using the
   * static codes or the dynamic codes. *)
  let emptyq = Boolqueue.create () in
  let queue = match level with 
    (* each option should return the queue that is filled with bytes *)
    | Zero -> raise (ZlibExn "don't call this pls")
    | One -> deflate_huffman_only instr emptyq
    | Two -> raise (ZlibExn "LZ77 currently unsupported") in
  let rec remove_all qu =
    match Boolqueue.dequeue_byte qu with
    | Some b, qu' -> Some b :: remove_all qu'
    | None, qu' -> 
      [ Some (fst (Boolqueue.dequeue_byte_force qu')); None; ] in
  let len = Boolqueue.len_in_bytes queue in
  len, (remove_all queue |> Stream.of_list)












(* DECODING ALGORITHM *)
(* do *)
(*    read block header from input stream. *)
(*    if stored with no compression *)
(*       skip any remaining bits in current partially *)
(*          processed byte *)
(*       read LEN and NLEN (see next section) *)
(*       copy LEN bytes of data to output *)
(*    otherwise *)
(*       if compressed with dynamic Huffman codes *)
(*          read representation of code trees (see *)
(*             subsection below) *)
(*       loop (until end of block code recognized) *)
(*          decode literal/length value from input stream *)
(*          if value < 256 *)
(*             copy value (literal byte) to output stream *)
(*          otherwise *)
(*             if value = end of block (256) *)
(*                break from loop *)
(*******v THIS SHOULDN'T HAPPEN WITH MY CURRENT ENCODER v*********)
(*             otherwise (value = 257..285) *)
(*                decode distance from input stream *)
(*                move backwards distance bytes in the output *)
(*                stream, and copy length bytes from this *)
(*                position to the output stream. *)
(*       end loop *)
(* while not last block *)


