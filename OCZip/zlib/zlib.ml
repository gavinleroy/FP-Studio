(****************************)
(*        Gavin Gray        *)
(*    University of Utah    *)
(*    Spring 21 -- OCZip    *)
(****************************)

open Core
open Stdint

let crc32 stream = 
  let rec outer_loop crc = 
    let rec inner_loop n crc = 
      match n with
      | 0 -> crc
      | _ -> inner_loop (n - 1) 
               (Uint32.logxor 
                  (Uint32.shift_right crc 1) 
                  (Uint32.logand 
                     (Uint32.of_int 0xEDB88320)
                     (Uint32.neg (Uint32.logand crc (Uint32.of_int 1))))) 
    in 
    match Stream.next stream with
    | None -> crc
    | Some b -> outer_loop (inner_loop 8 (Uint32.logxor crc (Uint32.of_int b)))
  in
  (Uint32.to_int (Uint32.lognot (outer_loop (Uint32.of_int 0xFFFFFFFF))))

