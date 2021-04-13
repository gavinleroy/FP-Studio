(****************************)
(*        Gavin Gray        *)
(*    University of Utah    *)
(*    Spring 21 -- OCZip    *)
(****************************)

open Core

exception EmptyQ of string

type t = bool Queue.t

(* type nonrec t = *) 
(*   { data : Bigint.t; *)
(*     size : int } *)

(* let create () = *) 
(*   { data = Bigint.zero; *)
(*     size = 0 } *)
let create () = 
  Queue.create ()

(* let has_byte q = *) 
(*   q.size >= 8 *)
let has_byte q = 
  Queue.length q >= 8

(* let is_empty q = *) 
(*   q.size = 0 *)
let is_empty q = 
  Queue.is_empty q

(* let enqueue b { data; size; } = *)
(*   let new_data = *) 
(*     match b with *)
(*     | true -> *) 
(*       Bigint.bit_or (Bigint.shift_left data 1) Bigint.one *)
(*     | false -> *)
(*       Bigint.shift_left data 1 in *)
(*   { data = new_data; size = size + 1 } *)
let enqueue b q = 
  Queue.enqueue q b;
  q

(* let dequeue q = *)
(*   let new_size = q.size - 1 in *)
(*   match q.size with *)
(*   | 0 -> (None, q) *)
(*   | _ -> *)  
(*     (Some (Bigint.(>) *)
(*              (Bigint.bit_and (Bigint.shift_right q.data new_size) Bigint.one) *)
(*              Bigint.zero) *)
(*     , { q with size = new_size }) *)
let dequeue q =
 Queue.dequeue q, q

let enqueue_byte b q =
  let rec eq n b q =
    let is_even b = 
      (b land 1) = 1 in
    if n < 0 then q
    else 
      let v = (is_even (b lsr n)) in
      eq (n - 1) b (enqueue v q) in
  eq 7 b q

let dequeue_byte q =
  let rec dq n b q' = 
    if n < 0 then Some b, q'
    else
      let v, q'' = dequeue q' in
      let v' = 
        match v with 
        | Some true -> 1 
        | Some false -> 0 
        | None -> raise (EmptyQ "unreachable!") in
      let v'' = ((b lsl 1) lor v') in
      dq (n - 1) v'' q'' in
  if has_byte q 
  then dq 7 0 q
  else None, q

