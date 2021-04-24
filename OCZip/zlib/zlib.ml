(****************************)
(*        Gavin Gray        *)
(*    University of Utah    *)
(*    Spring 21 -- OCZip    *)
(****************************)

open Core
open Stdint
open Lib
open Boolqueue
open Huffman
open Lz77

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

(* max stored block size is 
 * restricted to 2 bytes *)
let max_store_block_size =
  Uint16.to_int Uint16.max_int

(** NOTE when storing you can only do one block at a time. *)
let deflate_store_only instr queue = 
  let rec eq_all q =
    if Boolqueue.len_in_bytes q >= max_store_block_size
    then q
    else match Stream.next instr with
      | None -> q
      | Some i -> eq_all (Boolqueue.enqueue_byte q i) in
  let data = eq_all Boolqueue.create in
  let is_final = match Stream.peek instr with
    | None | Some None -> true
    | _ -> false in
  let eq2b = fun b qu -> Boolqueue.enqueue_byte 
      (Boolqueue.enqueue_byte qu b) (b lsr 8) in
  let len = Boolqueue.len_in_bytes data in
  let nlen = Uint16.(of_int len |> lognot |> to_int) in
  Boolqueue.enqueue_all_byte_aligned queue 
    [is_final; false; false;]
  |> eq2b len |> eq2b nlen 
  |> Boolqueue.enqueue_from data

(** the header bits for a block using static huffman codes *)
let static_huffman_block_header =
  [true; true; false;]

(* val deflate_huffman_only buffer-of-bytes queue *)
let deflate_huffman_only instr queue =
  let rec eq_all qu =
    match Stream.next instr with
    | None -> Boolqueue.enqueue_all qu Huffman.end_of_stream_byte
    | Some i -> 
      match Huffman.encode_lit_fixed i with
      | None -> raise (ZlibExn "invalid byte found in input stream")
      | Some b -> eq_all (Boolqueue.enqueue_all qu b) in 
  Boolqueue.enqueue_all queue static_huffman_block_header
  |> eq_all

(** deflate the input stream with static huffman codes and lz77 *)
let deflate_static instr queue =
  let rec eq_all lz77win qu =
    match LZ77.find_match lz77win instr with
    | LZ77.Empty, _ -> 
      Boolqueue.enqueue_all qu Huffman.end_of_stream_byte
    | LZ77.Literal i, win' ->
      (match Huffman.encode_lit_fixed i with
      | None -> raise (ZlibExn "invalid byte found in input stream")
      | Some b -> eq_all win' (Boolqueue.enqueue_all qu b))
    | LZ77.Pointer (l, d), win' -> 
      match Huffman.encode_len_fixed l, Huffman.encode_dist_fixed d with
      | None, _ -> raise (ZlibExn "encoding len invalid")
      | _, None -> raise (ZlibExn "encoding dist invalid")
      | Some ll, Some dd -> 
        eq_all win' 
          (Boolqueue.enqueue_all qu (List.append ll dd)) in
  Boolqueue.enqueue_all queue static_huffman_block_header
  |> eq_all LZ77.create

(** main entry point for the deflate algorithm  *)
let deflate ?(level=Two) instr =
  (* TODO : read in a buffer of bytes, doing some heuristics at the
   * same time. Switch on whether this block is better using the
   * static codes or the dynamic codes. *)

  (* add all of the bytes as bits to the queue *)
  let rec add_all qu = 
  let qu' = match level with 
    | Zero -> deflate_store_only instr qu
    | One -> deflate_huffman_only instr qu
    | Two -> deflate_static instr qu in
  match Stream.peek instr with
  | None | Some None -> qu'
  | _ -> add_all qu' in
  (* remove all of the bits in the queue as bytes *)
  let rec remove_all qu =
    match Boolqueue.dequeue_byte qu with
    | Some b, qu' -> 
      Some b :: remove_all qu'
    | None, qu' -> 
      let b = (fst (Boolqueue.dequeue_byte_force qu')) in
      [ Some b; None; ] in
  let queue =  add_all Boolqueue.create in
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
(*             otherwise (value = 257..285) *)
(*                decode distance from input stream *)
(*                move backwards distance bytes in the output *)
(*                stream, and copy length bytes from this *)
(*                position to the output stream. *)
(*       end loop *)
(* while not last block *)

