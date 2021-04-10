(****************************)
(*        Gavin Gray        *)
(*    University of Utah    *)
(*    Spring 21 -- OCZip    *)
(****************************)

open Core

type nonrec bqueue = 
  { data : Bigint.t;
    size : int }

let create () = 
  { data = Bigint.zero;
    size = 0 }

let enqueue b { data; size; } =
  let new_data = 
    match b with
    | true -> 
      Bigint.bit_or (Bigint.shift_left data 1) Bigint.one
    | false ->
      Bigint.shift_left data 1 in
  { data = new_data; size = size + 1 }

let dequeue q =
  let new_size = q.size - 1 in
  match q.size with
  | 0 -> (None, q)
  | _ ->  
    (Some (Bigint.(>)
             (Bigint.bit_and (Bigint.shift_right q.data new_size) Bigint.one)
             Bigint.zero)
    , { q with size = new_size })

