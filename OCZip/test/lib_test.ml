(****************************)
(*        Gavin Gray        *)
(*    University of Utah    *)
(*    Spring 21 -- OCZip    *)
(****************************)

open OUnit2
open Lib
open Boolqueue

(*****************************************)
(*             BOOLEAN QUEUE             *)
(*****************************************)

let obp v = 
  match v with
  | None -> "none"
  | Some b -> Bool.to_string b

let obp' v = 
  match v with
  | None -> "none"
  | Some b -> Printf.sprintf "%x" b

(*  queue: ~BACK  1 0 1 1  FRONT~> *)
let setup () = 
  let q = (Boolqueue.create ()) in
  Boolqueue.enqueue_all 
    q [ true; true; false; true; ]

let queue_test5 _ =
  let ctx = (Boolqueue.create ()) in
  let v = 0b01100101 in
  let q = Boolqueue.enqueue_byte ctx v in
  let v1, _ = Boolqueue.dequeue_byte q in
  assert_equal
    ~msg:"enqueueing byte"
    ~printer:obp'
    (Some v) v1

let queue_test4 _ = 
  let q = (Boolqueue.create ()) in
  (* ~back~ 0110 1011 ~front~> *)
  let q = Boolqueue.enqueue_all 
      q [ true; true; false; true; false; true; true; false; ] in
  assert_equal ~msg:"len in bytes"
    1 (Boolqueue.len_in_bytes q);
  let v1, _ = Boolqueue.dequeue_byte q in
  assert_equal
    ~msg:"dequeueing byte"
    ~printer:obp'
    (Some 0b01101011) v1

let queue_test3 _ = 
  let v = 0xff in
  let q = Boolqueue.enqueue_byte 
      (Boolqueue.create ()) v in
  let v', _ = Boolqueue.dequeue_byte q in
  assert_equal
    ~msg:"dequeueing byte"
    ~printer:obp'
    (Some v) v'

let queue_test2 q = 
  let (v, q2) = (Boolqueue.dequeue q) in
  assert_equal ~msg:"queue_test2" ~printer:obp (Some true) v; 
  let (v, q3) = (Boolqueue.dequeue q2) in
  assert_equal ~msg:"queue_test2" ~printer:obp (Some true) v; 
  let (v, q4) = (Boolqueue.dequeue q3) in
  assert_equal ~msg:"queue_test2" ~printer:obp (Some false) v; 
  let (v, _) = (Boolqueue.dequeue q4) in
  assert_equal ~msg:"queue_test2" ~printer:obp (Some true) v

let queue_test1 q = 
  assert_equal 
    ~msg:"queue_test1"
    ~printer:obp 
    (Some true)
    (fst (Boolqueue.dequeue q))

let suite = 
  "TestLib" >::: [
    "queue_test1" >:: (fun _ -> queue_test1 (setup ()));
    "queue_test2" >:: (fun _ -> queue_test2 (setup ()));
    "queue_test3" >:: queue_test3;
    "queue_test4" >:: queue_test4;
    "queue_test5" >:: queue_test5;
  ]

let () =
  run_test_tt_main suite

