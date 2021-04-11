(****************************)
(*        Gavin Gray        *)
(*    University of Utah    *)
(*    Spring 21 -- OCZip    *)
(****************************)

open Core
open Stdint
open Lib

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

(** represents the symbol types for data in the deflate algorithm  *)
(* type nonrec symbol = *) 
(*  | Literal of Uint8.t (** literal byte values in the range (0.. 255) *) *)
  (** | Pointer of Uint8.t * int  <length, backward distance> pairs,
                                 where the length is drawn from (3..258) 
                                 and the distance is drawn from (1..32,768) *)

(*--------------------------------------*)
(*               Steps                  *)
(*--------------------------------------*)
(*   1. write byte stream in ALL        *)
(*     uncompressed blocks              *)
    (* what do we need for this? *)
    (* * block data structure *)
(*--------------------------------------*)
(*--------------------------------------*)
(*   2. only compress bytes with static *)
(*     huffman encoding                 *)
(*--------------------------------------*)
(*   3. get the lz77 algorithm working  *)
(*     with a sliding window            *) 
(*--------------------------------------*)

type nonrec btype = | N (* | FH | DH | E *)

let max_fix_block_size = 
  Uint16.to_int Uint16.max_int

(* type nonrec block = *) 
(*   { bfinal : bool; *)
(*     b_type : btype; *)
(*     len    : Uint16.t } *)

let is_even b = 
  (b land 1) = 0

let enqueue_btype bt q = 
  match bt with
  | N  -> Boolqueue.enqueue false (Boolqueue.enqueue false q) (* 00 *)
  (* | FH -> Boolqueue.enqueue false (Boolqueue.enqueue true  q) (1* 01 *1) *)
  (* | DH -> Boolqueue.enqueue true  (Boolqueue.enqueue false q) (1* 10 *1) *)
  (* | E  -> Boolqueue.enqueue true  (Boolqueue.enqueue true  q) (1* 11 *1) *)

let enqueue_byte b q =
  let rec eq n b q =
    if n = 0 then q
    else 
      eq (n - 1) (b lsr 1)
      (Boolqueue.enqueue (is_even b) q) in
  eq 8 b q

let dequeue_byte q =
  let rec dq n b q = 
    if n = 0 then Some b, q
    else
      let v, q' = Boolqueue.dequeue q in
      let v' = match v with | Some true -> 1 | Some false -> 0 | None -> 0 in
      let v'' = (b lsl 1) land v' in
      dq (n - 1) v'' q' in
  if Boolqueue.has_byte q 
  then dq 8 0 q
  else None, q

let enqueue_block instr q = 
  let rec loop_search n q = 
    if n = max_fix_block_size
    then n, q
    else match Stream.next instr with
      | Some i -> loop_search (n + 1) (enqueue_byte i q)
      | None -> n, q in
  let rec loop_swap src tgt = 
    match Boolqueue.dequeue src with
    | Some b, srcq -> loop_swap srcq (Boolqueue.enqueue b tgt)
    | None, _ -> tgt in
  let wrote, qtemp = loop_search 0 (Boolqueue.create ()) in
  let is_final = match Stream.peek instr with | None | Some None -> true | _ -> false in
  Printf.printf "is final? %b\n" is_final;
  let q' = Boolqueue.enqueue is_final q in
  let q'' = enqueue_byte (wrote lsr 4) (enqueue_byte wrote q') in
  let wrote = lnot wrote in
  let q''' = enqueue_byte (wrote lsr 4) (enqueue_byte wrote q'') in
  let q'''' = (enqueue_btype N q''') in
  wrote + 5, loop_swap qtemp q''''

let enqueue_file instr q = 
  let rec do_block n q = 
    Printf.printf "enqueued %d bytes so far\n" n;
    match Stream.peek instr with
    | None | Some None -> n, q
    | _ -> 
      let n', q' = enqueue_block instr q in
      do_block (n + n') q' in
  do_block 0 q

let deflate instr = 
  print_endline "deflate started";
  let rec make_list q = 
    match dequeue_byte q with
    | None, _ -> [None]
    | ob, nq -> ob :: make_list nq in
  let written_bytes, fullq = enqueue_file instr (Boolqueue.create ()) in
  (* NOTE the stream should be int option Stream.t *)
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


