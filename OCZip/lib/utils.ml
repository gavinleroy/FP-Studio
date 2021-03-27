(****************************)
(*        Gavin Gray        *)
(*    University of Utah    *)
(*    Spring 21 -- OCZip    *)
(****************************)

open Core
open Defs

exception ImpE of string

let write_1byte ochnl n = 
  Out_channel.output_byte ochnl n

let write_2byte_le ochnl n = 
  (* output the lower byte *)
  write_1byte ochnl n; 
  (* output the upper byte *)
  write_1byte ochnl (n lsr 8)

let write_4byte_le ochnl n = 
  (* write the lower 2 bytes *)
  write_2byte_le ochnl n;
  (* write the upper two bytes *)
  write_2byte_le ochnl (n lsr 16)

(* get the time in MS-DOS fmt from unixtime *)
let getdosfmt_localtime unixtime = 
  let t = Unix.localtime unixtime in
    ((t.Unix.tm_hour lsl 11) lor
    (t.Unix.tm_min lsl 5)) lor
    (* divide seconds by two *)
    (t.Unix.tm_sec lsr 1)

(* get the date in MS-DOS fmt from unixtime *)
let getdosfmt_date unixtime =
  let t = Unix.localtime unixtime in
    (* tm_year = Year - 1900 NOTE we want year since 1980 not 1900*)
    (((t.Unix.tm_year - 80) lsr 9) lor
    (* 0 <= tm_mon < 12 *)
    ((t.Unix.tm_mon + 1) lsr 5)) lor
    (* tm_mday --> Month date *)
    t.Unix.tm_mday

let get_compress_method_code cmthd = 
  match cmthd with
    | Store -> 0
    | Deflate -> 8

let int64_to_int i64 = 
  match Int64.to_int i64 with
    | Some a -> a
    | None -> raise (ImpE "64 can't convert to int ...")

