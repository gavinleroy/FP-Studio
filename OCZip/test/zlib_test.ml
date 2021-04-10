(****************************)
(*        Gavin Gray        *)
(*    University of Utah    *)
(*    Spring 21 -- OCZip    *)
(****************************)

open OUnit2
open Lib

let rel_path = 
  "../test/test-files/"

(************************************************)
(*             CRC 32 UPDATE TESTS              *)
(* NOTE the update tests compare output against *)
(* the ouput of the crc32 algorithm. If a bug   *)
(* in the crc32 computation is found the update *)
(* tests are also erroneous.                    *)
(************************************************)

let crc_update_test4 _ =
  let str1 = Utils.fn_to_byte_stream (rel_path ^ "crc32-test4.jpeg") in
  let str2 = Utils.fn_to_byte_stream (rel_path ^ "crc32-test4.jpeg") in
  let rec loop crc ss =
    match Stream.next ss with
    | None -> crc
    | (Some b) ->
      loop (Zlib.crc32_update crc b) ss in
  assert_equal 
    ~msg:"crc32_update test4"
    ~printer:string_of_int
    (Zlib.crc32 str1) (loop 0 str2)

(* TODO include tests 2,3 for crc update *)

let crc_update_test1 _ =
  let str1 = Utils.fn_to_byte_stream (rel_path ^ "crc32-test1.txt") in
  let str2 = Utils.fn_to_byte_stream (rel_path ^ "crc32-test1.txt") in
  let rec loop crc ss =
    match Stream.next ss with
    | None -> crc
    | (Some b) ->
      loop (Zlib.crc32_update crc b) ss in
  assert_equal 
    ~msg:"crc32_update test1"
    ~printer:string_of_int
    (Zlib.crc32 str1) (loop 0 str2)

(************************************************)
(*                 CRC 32 TESTS                 *)
(************************************************)

let crc_test4 _ = 
  let str = Utils.fn_to_byte_stream (rel_path ^ "crc32-test4.jpeg") in
  assert_equal 
    ~msg:"crc32 test4"
    ~printer:string_of_int
    0x031b4302 (Zlib.crc32 str)

let crc_test3 _ = 
  let str = Utils.fn_to_byte_stream (rel_path ^ "crc32-test3.png") in
  assert_equal 
    ~msg:"crc32 test3"
    ~printer:string_of_int
    0x04fca310 (Zlib.crc32 str)

let crc_test2 _ = 
  let str = Utils.fn_to_byte_stream (rel_path ^ "crc32-test2.txt") in
  assert_equal 
    ~msg:"crc32 test2"
    ~printer:string_of_int
    0x0d278dbc (Zlib.crc32 str)

let crc_test1 _ = 
  let str = Utils.fn_to_byte_stream (rel_path ^ "crc32-test1.txt") in
  assert_equal 
    ~msg:"crc32 test1"
    ~printer:string_of_int
    0x26bd706a (Zlib.crc32 str)

let suite = 
  "TestZLib" >::: [
    "crc32-test1" >:: crc_test1;
    "crc32-test2" >:: crc_test2;
    "crc32-test3" >:: crc_test3;
    "crc32-test4" >:: crc_test4;
    "crc32-update-test1" >:: crc_update_test1;
    "crc32-update-test4" >:: crc_update_test4
  ]

let () =
  run_test_tt_main suite

