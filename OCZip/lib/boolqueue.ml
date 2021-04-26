(****************************)
(*        Gavin Gray        *)
(*    University of Utah    *)
(*    Spring 21 -- OCZip    *)
(****************************)

module Boolqueue = struct

  open Core

  exception EmptyQ

  type t = bool Queue.t

  let create () = 
    Queue.create ()

  let has_byte q = 
    Queue.length q >= 8

  let is_empty q = 
    Queue.is_empty q

  let enqueue q b = 
    Queue.enqueue q b;
    q

  let enqueue_all q vs =
    Queue.enqueue_all q vs;
    q

  (* NOTE requires that bytes have only been dequeued in bytes *)
  let enqueue_all_byte_aligned q vs =
    let rec align q' = 
      if (Queue.length q' % 8) = 0 then q'
      else align (enqueue q' false) in
    enqueue_all q vs |> align

  let dequeue q =
    Queue.dequeue q, q

  let rec enqueue_from s t =
    match dequeue s with
    | Some b, s' -> enqueue_from s' (enqueue t b)  
    | None, _ -> t

  (* given the num 0b0011 
   * the queue is ~FRONT~ 1 1 0 0 ~BACK~ *)
  let enqueue_byte q b =
    let is_high x = (x land 1) = 1 in
    let rec eq n b q =
      if n = 0 then q
      else 
        eq (n - 1) (b lsr 1) (enqueue q (is_high b)) in
    eq 8 b q

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

  let len_in_bytes q =
    let len = Queue.length q in
    if len % 8 = 0 then len / 8
    else 1 + (len / 8)

  let of_byte_list bs =
    List.fold_left bs ~init:(create ()) ~f:enqueue_byte

end (* MODULE BOOLQUEUE*)

