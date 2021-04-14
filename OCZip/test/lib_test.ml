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
  Boolqueue.enqueue_all 
    [ true; true; false; true; ] q

let queue_test4 ctx = 
  (* ~back~ 0110 1011 ~front~> *)
  let q = Boolqueue.enqueue_all 
      [ false; true; true; false; ] ctx in
  let v1, _ = Boolqueue.dequeue_byte q in
  assert_equal
    ~msg:"dequeueing byte"
    ~printer:obp'
    (Some 0b01101011) v1

(* test3 requires an empty queue *)
let queue_test3 _ = 
  let q = Boolqueue.create () in
  let start_num = 1000000 in
  let ft = (fun x -> (x land 1) = 1) in
  let rec fill n q' =
    match n with
    | 0 -> q'
    | _ -> 
      fill (n - 1) (Boolqueue.enqueue (ft n) q') in
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
    "queue_test3" >:: queue_test3;
    "queue_test4" >:: (fun _ -> queue_test4 (setup ()));
  ]

let () =
  run_test_tt_main suite

