(****************************)
(*        Gavin Gray        *)
(*    University of Utah    *)
(*    Spring 21 -- OCZip    *)
(****************************)

module Huffman = struct

  open Core

  exception TreeFull

  exception Fatal

  type 'a tree =
    | Empty
    | Leaf of 'a
    | Node of 'a tree * 'a tree

  type bitlist = bool list

  type decoder = int tree

  type encoder = (int, bitlist, Int.comparator_witness) Map.t

  (**********************************)
  (*  functions on decoders (trees) *)
  (**********************************)

  let rec build_at_depth nd v d ch =
    if ch > d then raise Fatal
    else
      match nd with
      | Leaf _ -> raise TreeFull
      | Empty ->
        if d = ch then Leaf v
        else Node ((build_at_depth Empty v d (ch + 1)), Empty)
      | Node (lo, ro) ->
        try Node (build_at_depth lo v d (ch + 1), ro)
        with TreeFull -> Node (lo, build_at_depth ro v d (ch + 1))

  let of_lens ps =
    try List.fold 
      (List.sort ps ~compare:(fun (a, b) (c, d) ->
           if b = d then compare a c
           else compare b d)) 
      ~init:Empty 
      ~f:(fun acc (v, l) -> build_at_depth acc v l 0)
      |> Some
    with TreeFull -> None

  let rec to_sexp t =
    match t with
    | Empty -> "()"
    | Leaf i -> "(Leaf " ^ (Int.to_string i) ^ ")"
    | Node (l, r) ->
      "(Node " ^ (to_sexp l) ^ " " ^ (to_sexp r) ^ ")"

  (***************************)
  (*  functions on encoders  *)
  (***************************)

  let encode e v =
    Map.find e v

  let encode_exn e v =
    Map.find_exn e v

  (* val bit_list_of_int: int -> int -> bitlist *)
  (* The function works as follows: 
   * given an integer `b` which represents a byte
   * we push `len` bits into a list.
   *
   * The numbers are processed as follows:
   * 0b001110010 ~> 7 ~> [ #t, #t, #t, #f, #f, #t, #f ]*)
  let bit_list_of_int b len =
    let is_odd = 
      fun x -> (x land 1) = 1 in
    let rec process n bt acc =
      if n = 0 then acc
      else process (n - 1) (bt lsr 1) ((is_odd bt) :: acc) in
    process len b []

  let end_of_stream_byte =
    List.(range 0 7 >>| fun _ -> false)

  (************************************************)
  (* Lit Value  Bits  Codes                       *)
  (* ---------  ----  -----                       *)
  (*   0 - 143   8    00110000 through 10111111   *)
  (* 144 - 255   9    110010000 through 111111111 *)
  (* 256 - 279   7    0000000 through 0010111     *)
  (* 280 - 287   8    11000000 through 11000111   *)
  (************************************************)
  let fixed_encoder =
    List.fold
      List.(range 0 288 >>| 
            fun x -> x, 
                     if x < 144 then bit_list_of_int (x + 0b00110000) 8
                     else if x < 256 then bit_list_of_int ((x - 144) + 0b110010000) 9
                     else if x < 280 then bit_list_of_int ((x - 256) + 0b0000000) 7
                     else bit_list_of_int ((x - 280) + 0b11000000) 8)
      ~init:(Map.empty(module Int))
      ~f:(fun acc (k, v) ->
          Map.add_exn acc ~key:k ~data:v)

  let encode_lit_fixed l = 
    encode fixed_encoder l

  (* code  bits  len     code  bits len      code bits len    *)
  (* -------------------------------------------------------- *)
  (* 257   0     3       267   1   15,16     277   4   67-82  *)
  (* 258   0     4       268   1   17,18     278   4   83-98  *)
  (* 259   0     5       269   2   19-22     279   4   99-114 *)
  (* 260   0     6       270   2   23-26     280   4  115-130 *)
  (* 261   0     7       271   2   27-30     281   5  131-162 *)
  (* 262   0     8       272   2   31-34     282   5  163-194 *)
  (* 263   0     9       273   3   35-42     283   5  195-226 *)
  (* 264   0    10       274   3   43-50     284   5  227-257 *)
  (* 265   1  11,12      275   3   51-58     285   0    258 *)
  (* 266   1  13,14      276   3   59-66 *)
  let encode_len_fixed l =
    (* NOTE bit_list_of_int b len *)
    try let l1, l2 = if l < 3 then raise Fatal
          else if l < 11 then 
            encode_exn fixed_encoder (257 + l - 3), []
          else if l < 13 then
            (encode_exn fixed_encoder 265), (bit_list_of_int (l - 11) 1)
          else if l < 15 then
            (encode_exn fixed_encoder 266), (bit_list_of_int (l - 13) 1)
          else if l < 17 then
            (encode_exn fixed_encoder 267), (bit_list_of_int (l - 15) 1)
          else if l < 19 then
            (encode_exn fixed_encoder 268), (bit_list_of_int (l - 17) 1)
          else if l < 23 then
            (encode_exn fixed_encoder 269), (bit_list_of_int (l - 19) 2)
          else if l < 27 then
            (encode_exn fixed_encoder 270), (bit_list_of_int (l - 23) 2)
          else if l < 31 then
            (encode_exn fixed_encoder 271), (bit_list_of_int (l - 27) 2)
          else if l < 35 then
            (encode_exn fixed_encoder 272), (bit_list_of_int (l - 31) 2)
          else if l < 43 then
            (encode_exn fixed_encoder 273), (bit_list_of_int (l - 35) 3)
          else if l < 51 then
            (encode_exn fixed_encoder 274), (bit_list_of_int (l - 43) 3)
          else if l < 59 then
            (encode_exn fixed_encoder 275), (bit_list_of_int (l - 51) 3)
          else if l < 67 then
            (encode_exn fixed_encoder 276), (bit_list_of_int (l - 59) 3)
          else if l < 83 then
            (encode_exn fixed_encoder 277), (bit_list_of_int (l - 67) 4)
          else if l < 99 then
            (encode_exn fixed_encoder 278), (bit_list_of_int (l - 83) 4)
          else if l < 115 then
            (encode_exn fixed_encoder 279), (bit_list_of_int (l - 99) 4)
          else if l < 131 then
            (encode_exn fixed_encoder 280), (bit_list_of_int (l - 115) 4)
          else if l < 163 then
            (encode_exn fixed_encoder 281), (bit_list_of_int (l - 131) 5)
          else if l < 195 then 
            (encode_exn fixed_encoder 282), (bit_list_of_int (l - 163) 5)
          else if l < 227 then 
            (encode_exn fixed_encoder 283), (bit_list_of_int (l - 195) 5)
          else if l < 258 then
            (encode_exn fixed_encoder 284), (bit_list_of_int (l - 227) 5)
          else raise Fatal in
      List.append l1 (List.rev l2) |> Some
    with Fatal -> None

  (* cd  bs  dist   cd   bs    dist    cd    bs     dist    *)
  (* ------------------------------------------------------ *)
  (* 0   0    1     10   4     33-48    20    9   1025-1536 *)
  (* 1   0    2     11   4     49-64    21    9   1537-2048 *)
  (* 2   0    3     12   5     65-96    22   10   2049-3072 *)
  (* 3   0    4     13   5     97-128   23   10   3073-4096 *)
  (* 4   1   5,6    14   6    129-192   24   11   4097-6144 *)
  (* 5   1   7,8    15   6    193-256   25   11   6145-8192 *)
  (* 6   2   9-12   16   7    257-384   26   12  8193-12288 *)
  (* 7   2  13-16   17   7    385-512   27   12 12289-16384 *)
  (* 8   3  17-24   18   8    513-768   28   13 16385-24576 *)
  (* 9   3  25-32   19   8   769-1024   29   13 24577-32768 *)
  (*  NOTE code for distances is a fixed 5 bit code [0, 31] *)
  let encode_dist_fixed d =
    try let d1, d2 = if d <= 0 then raise Fatal
          else if d < 5 then 
            (bit_list_of_int (d - 1) 5), []
          else if d < 7 then 
            (bit_list_of_int 4 5), (bit_list_of_int (d - 5) 1)
          else if d < 9 then 
            (bit_list_of_int 5 5), (bit_list_of_int (d - 7) 1)
          else if d < 13 then 
            (bit_list_of_int 6 5), (bit_list_of_int (d - 9) 2)
          else if d < 17 then 
            (bit_list_of_int 7 5), (bit_list_of_int (d - 13) 2)
          else if d < 25 then 
            (bit_list_of_int 8 5), (bit_list_of_int (d - 17) 3)
          else if d < 33 then 
            (bit_list_of_int 9 5), (bit_list_of_int (d - 25) 3)
          else if d < 49 then 
            (bit_list_of_int 10 5), (bit_list_of_int (d - 33) 4)
          else if d < 65 then 
            (bit_list_of_int 11 5), (bit_list_of_int (d - 49) 4)
          else if d < 97 then 
            (bit_list_of_int 12 5), (bit_list_of_int (d - 65) 5)
          else if d < 129 then 
            (bit_list_of_int 13 5), (bit_list_of_int (d - 97) 5)
          else if d < 193 then 
            (bit_list_of_int 14 5), (bit_list_of_int (d - 129) 6)
          else if d < 257 then 
            (bit_list_of_int 15 5), (bit_list_of_int (d - 193) 6)
          else if d < 385 then 
            (bit_list_of_int 16 5), (bit_list_of_int (d - 257) 7)
          else if d < 513 then 
            (bit_list_of_int 17 5), (bit_list_of_int (d - 385) 7)
          else if d < 769 then 
            (bit_list_of_int 18 5), (bit_list_of_int (d - 513) 8)
          else if d < 1025 then 
            (bit_list_of_int 19 5), (bit_list_of_int (d - 769) 8)
          else if d < 1537 then 
            (bit_list_of_int 20 5), (bit_list_of_int (d - 1025) 9)
          else if d < 2049 then 
            (bit_list_of_int 21 5), (bit_list_of_int (d - 1537) 9)
          else if d < 3073 then 
            (bit_list_of_int 22 5), (bit_list_of_int (d - 2049) 10)
          else if d < 4097 then 
            (bit_list_of_int 23 5), (bit_list_of_int (d - 3073) 10)
          else if d < 6145 then 
            (bit_list_of_int 24 5), (bit_list_of_int (d - 4097) 11)
          else if d < 8193 then 
            (bit_list_of_int 25 5), (bit_list_of_int (d - 6145) 11)
          else if d < 12289 then 
            (bit_list_of_int 26 5), (bit_list_of_int (d - 8193) 12)
          else if d < 16385 then 
            (bit_list_of_int 27 5), (bit_list_of_int (d - 12289) 12)
          else if d < 24577 then 
            (bit_list_of_int 28 5), (bit_list_of_int (d - 16385) 13)
          else if d < 32769 then 
            (bit_list_of_int 29 5), (bit_list_of_int (d - 24577) 13)
          else raise Fatal in
      List.append d1 (List.rev d2) |> Some
    with Fatal -> None

end (* END OF MODULE *)

