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

(*  queue: ~BACK  1 0 1 1  FRONT~> *)
let setup () = 
  let q = Boolqueue.create () in
  Boolqueue.enqueue true
    (Boolqueue.enqueue false 
       (Boolqueue.enqueue true
          (Boolqueue.enqueue true q)))

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
    "queue_test3" >:: (fun _ -> queue_test3 (Boolqueue.create ()))
  ]

let () =
  run_test_tt_main suite

