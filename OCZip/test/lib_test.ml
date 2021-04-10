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

(*  queue: ~BACK  1 0 1 1  FRONT~> *)
let setup () = 
  let q = Boolqueue.create () in
  Boolqueue.enqueue true
    (Boolqueue.enqueue false 
      (Boolqueue.enqueue true
        (Boolqueue.enqueue true q)))

let queue_test1 q = 
  assert_equal 
    ~msg:"queue_test1"
    (Some true)
    (fst (Boolqueue.dequeue q))

let suite = 
  "TestLib" >::: [
    "queue_test1" >:: (fun _ -> queue_test1 (setup ()))
  ]

let () =
  run_test_tt_main suite

