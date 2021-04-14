(****************************)
(*        Gavin Gray        *)
(*    University of Utah    *)
(*    Spring 21 -- OCZip    *)
(****************************)

open Core

exception EmptyQ

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

let enqueue_all v q =
  Queue.enqueue_all q v;
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

(* let enqueue_byte b q = *)
(*   let rec eq n b q = *)
(*     let is_even b = *) 
(*       (b land 1) = 1 in *)
(*     if n < 0 then q *)
(*     else *) 
(*       let v = (is_even (b lsr n)) in *)
(*       eq (n - 1) b (enqueue v q) in *)
(*   eq 7 b q *)

let rec build_byte q acc n ~success:f ~failure:esc =
  let b2i = fun b -> 
    match b with | true -> 1 | false -> 0 in
  if n = 8 then f acc, q
  else match dequeue q with
    | Some bo, q' -> 
      let bi = b2i bo in
      build_byte q' 
        (acc lor (bi lsl n)) 
        (n + 1) 
        ~success:f
        ~failure:esc
    | None, q' -> esc acc q'

(* if the queue has ~FRONT~ 1 1 0 0 ~BACK~ 
 * 0011 *)
let dequeue_byte q =
  if Queue.length q < 8 
  then None, q
  else build_byte q 0 0 
    ~success:(fun a -> Some a)
    ~failure:(fun _ _ -> raise EmptyQ)

let dequeue_byte_force q =
  build_byte q 0 0 
    ~success:ident
    ~failure:(fun a b -> a, b)

