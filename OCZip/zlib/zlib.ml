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
    match n with
    | 0 -> crc
    | _ -> 
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

(* type nonrec btype = | None | FixHuff | DynHuff | Error *)

(* type nonrec block = *) 
(*   { bfinal : bool; *)
(*     bty    : btype; } *)

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

let _deflate _instr = 
  let _dataq = Boolqueue.create () in
  Stream.from 
    (fun _ ->
      Some 0)






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


