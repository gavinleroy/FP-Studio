(****************************)
(*        Gavin Gray        *)
(*    University of Utah    *)
(*    Spring 21 -- OCZip    *)
(****************************)

open Core
open Stdint
open Lib

module Hufftree = Hufftree

(**************************)
(*         CRC32          *)
(**************************)

let crc32_update' crc byte = 
  let rec inner_loop n crc = 
    if n = 0 then crc
    else
      inner_loop (n - 1) 
        (Uint32.logxor 
           (Uint32.shift_right crc 1) 
           (Uint32.logand 
              (Uint32.of_int 0xEDB88320)
              (Uint32.neg (Uint32.logand crc (Uint32.of_int 1))))) in 
  (Uint32.logxor crc (Uint32.of_int byte))
  |> inner_loop 8 

(** provide an incremental way to update the crc with a byte *)
let crc32_update crc byte = 
  (Uint32.to_int 
     (Uint32.lognot 
        (crc32_update' 
           (Uint32.lognot (Uint32.of_int crc)) 
           byte)))

(** compute the crc32 of the given byte stream *)
let crc32 stream = 
  let rec outer_loop crc = 
    match Stream.next stream with
    | None -> crc
    | Some b -> 
      outer_loop (crc32_update' crc b) in
  (Uint32.to_int (Uint32.lognot 
                    (outer_loop (Uint32.of_int 0xFFFFFFFF))))

(****************************************)
(*     Deflate Utils and Algorithm      *)
(****************************************)

(*--------------------------------------*)
(*--------------------------------------*)
(*               Steps                  *)
(*--------------------------------------*)
(*   1. write byte stream in ALL        *)
(*     uncompressed blocks              *)
(*--------------------------------------*)
(*--------------------------------------*)
(*   2. only compress bytes with static *)
(*     huffman encoding                 *)
(*--------------------------------------*)
(*--------------------------------------*)
(*   3. get the lz77 algorithm working  *)
(*     with a sliding window            *) 
(*--------------------------------------*)
(*--------------------------------------*)

let max_fix_block_size = 
  Uint16.to_int Uint16.max_int

let enqueue_block instr q = 
  print_endline "entered enqueue_block";
  let rec loop_search n q = 
    Printf.printf "N is %d as compared to %d\n" n max_fix_block_size;
    if n = max_fix_block_size
    then n, q
    else match Stream.next instr with
      | Some i -> loop_search (n + 1) (Boolqueue.enqueue_byte i q)
      | None -> n, q in
  let rec loop_swap src tgt = 
    match Boolqueue.dequeue src with
    | Some b, srcq -> loop_swap srcq (Boolqueue.enqueue b tgt)
    | None, _ -> tgt in
  let wrote, qtemp = loop_search 0 (Boolqueue.create ()) in
  let is_final_fix = match Stream.peek instr with | None | Some None -> 0x01 | _ -> 0x00 in
  let q' = Boolqueue.enqueue_byte is_final_fix q in
  let q'' = Boolqueue.enqueue_byte (wrote lsr 8) 
      (Boolqueue.enqueue_byte wrote q') in
  let wroten = lnot wrote in
  let q''' = Boolqueue.enqueue_byte (wroten lsr 8) 
      (Boolqueue.enqueue_byte wroten q'') in
  wrote + 5, loop_swap qtemp q'''

let enqueue_file instr q = 
  print_endline "entered enqueue_file";
  let rec do_block n q' = 
    Printf.printf "enqueued %d bytes so far\n" n;
    match Stream.peek instr with
    | None | Some None -> n, q'
    | _ -> 
      let n', q'' = enqueue_block instr q' in
      do_block (n + n') q'' in
  do_block 0 q

let deflate instr = 
  print_endline "deflate started";
  let rec make_list q = 
    match Boolqueue.dequeue_byte q with
    | None, _ -> [None]
    | Some ob, nq -> Some ob :: make_list nq in
  let written_bytes, fullq = enqueue_file instr (Boolqueue.create ()) in
  print_endline "deflate done";
  written_bytes, Stream.of_list (make_list fullq)

(************************************************)
(* Lit Value  Bits  Codes                       *)
(* ---------  ----  -----                       *)
(*   0 - 143   8    00110000 through 10111111   *)
(* 144 - 255   9    110010000 through 111111111 *)
(* 256 - 279   7    0000000 through 0010111     *)
(* 280 - 287   8    11000000 through 11000111   *)
(************************************************)

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


