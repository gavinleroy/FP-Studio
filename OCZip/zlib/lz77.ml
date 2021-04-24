(****************************)
(*        Gavin Gray        *)
(*    University of Utah    *)
(*    Spring 21 -- OCZip    *)
(****************************)

module LZ77 = struct

  open Core

  exception Fatal of string

  let max_match_len = 257

  let min_match_len = 3

  let max_look =
    32 * 1024

  type t = 
    { max_lookback  : int;
      max_lookahead : int;
      min_match     : int; (** NOTE currently min_match must be 3 *)
      map           : (int, int list, Int.comparator_witness) Map.t;
      behind        : int BatVect.t; }

  type result =
    | Empty
    | Literal of int
    | Pointer of int * int (** length, backwards distance*)

  let create = 
    { max_lookback  = max_look;
      max_lookahead = max_look;
      min_match     = min_match_len;
      behind        = BatVect.empty;
      map           = Map.empty(module Int); }

  let hash a b c =
    (a lsl 16) lor (b lsl 8) lor c

  let exp_map v ns =
    let rec cmp_trp_hshs vs =
      match vs with
      | a :: b :: c :: _ -> 
        hash a b c :: cmp_trp_hshs (List.tl_exn vs)
      | _ -> [] in
    let bvns = BatVect.concat v.behind (BatVect.of_list ns) in
    if BatVect.length v.behind < v.min_match 
    then { v with behind = bvns }
    else let p = (BatVect.length v.behind) - 2 in
      let xs = 
        BatVect.get v.behind (p) :: 
        BatVect.get v.behind (p + 1) :: ns
        |> cmp_trp_hshs
        |> (fun ys -> List.mapi ys ~f:(fun i v -> (i+p), v)) in
      (* insert them into the dictionary (or update dict list) *)
      let nmap = List.fold_left xs ~init:v.map 
          ~f:(fun mac (i, h) -> Map.add_multi mac ~key:h ~data:i) in
      (* update the map *)
      let bvns = BatVect.concat v.behind (BatVect.of_list ns) in
      { v with map = nmap; behind = bvns }

  (* str := stream, v := t, n := len, rs := remaining idxs, xs := popped stream vals *)
  (* needs to return length, xs, ridxs*) 
  let rec match_from str v n rs xs =
    let ret_o = n, (List.rev xs), rs in
    if n = max_match_len then ret_o
    else match Stream.peek str with
    | Some None | None -> ret_o
    | Some Some i ->
      match List.filter_map rs ~f:(fun idx ->
          (* can't go past the end of the vec*)
          if BatVect.length v.behind >  idx
            (* can't go too far back *)
            && (BatVect.length v.behind) - v.max_lookback < idx
            (* values must be equal *)
            && (BatVect.get v.behind idx) = i 
          then Some (idx + 1)
          else None)  with
      | [] -> ret_o
      | rs' -> Stream.junk str;
        match_from str v (n+1) rs' (i :: xs)

  let match_lngst v str hsh =
    match Map.find v.map hsh with
    | None -> Empty, []
    | Some is ->
      match match_from str v 0 is [] with
      (* this could happen if all idx are too far back *)
      | 0, _, _ -> Empty, []
      | _, _, [] -> Empty, []
      | len, xs, rs ->
        let mx = Option.value_exn 
            (List.max_elt rs ~compare:Int.compare) in
        let idx = (BatVect.length v.behind) -  mx + len in
        Pointer (len, idx), xs

  let find_match v str =
    let ret_lit = (fun a -> Stream.junk str;
        Literal a, (exp_map v [a])) in
    match Stream.npeek v.min_match str with
    | Some a :: Some b :: Some c :: _ -> 
      (match hash a b c |> match_lngst v str with
       | Empty, _ -> 
         ret_lit a 
       | Literal _, _ -> raise (Fatal "unreachable")
       | Pointer (l, d), ns -> 
         Pointer (l, d), (exp_map v ns))
    | Some a :: _ -> ret_lit a
    | _ -> Empty, v

end (* END OF MODULE *)

