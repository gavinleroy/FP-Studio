(****************************)
(*        Gavin Gray        *)
(*    University of Utah    *)
(*    Spring 21 -- OCZip    *)
(****************************)

open OUnit2
open Lib

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
  let q = Boolqueue.create () in
  Boolqueue.enqueue true
    (Boolqueue.enqueue false 
       (Boolqueue.enqueue true
          (Boolqueue.enqueue true q)))

let queue_test5 _ = 
  let s1, s2 = 0x2c, 0xe7 in
  let q = Boolqueue.create () in
  let q' = Boolqueue.enqueue_byte s1 q in
  let q'' = Boolqueue.enqueue_byte s2 q' in
  let v1, q' = Boolqueue.dequeue_byte q'' in
  let v2, _ = Boolqueue.dequeue_byte q' in
  assert_equal
    ~msg:"dequeueing byte"
    ~printer:obp'
    (Some s1) v1;
  assert_equal
    ~msg:"dequeueing byte"
    ~printer:obp'
    (Some s2) v2

let queue_test4 _ = 
  let q = Boolqueue.create () in
  let q' = Boolqueue.enqueue_byte 0x63 q in
  let v1, _ = Boolqueue.dequeue_byte q' in
  assert_equal
    ~msg:"dequeueing byte"
    ~printer:obp'
    (Some 0x63) v1

(* test3 requires an empty queue *)
let queue_test3 q = 
  let start_num = 200000 in
  let ft = (fun x -> (x land 1)=0) in
  let rec fill n q' =
    match n with
    | 0 -> q'
    | _ -> fill (n - 1) (Boolqueue.enqueue (ft n) q') in
  let rec ass n q' = 
    match n with
    | 0 -> ()
    | _ -> 
      let (v, q'') = Boolqueue.dequeue q' in
      assert_equal ~msg:"queue_test3" ~printer:obp (Some (ft n)) v; 
      ass (n - 1) q'' in
  let q = fill start_num q in
  ass start_num q

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
    "queue_test3" >:: (fun _ -> queue_test3 (Boolqueue.create ()));
    "queue_test4" >:: queue_test4;
    "queue_test5" >:: queue_test5;
  ]

let () =
  run_test_tt_main suite

