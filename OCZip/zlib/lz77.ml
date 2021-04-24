(****************************)
(*        Gavin Gray        *)
(*    University of Utah    *)
(*    Spring 21 -- OCZip    *)
(****************************)

module LZ77 = struct

  open Core

  (*type t =*) 
  (*  { max_lookback  : int;*)
  (*    max_lookahead : int;*)
  (*    behind        : int list;*)
  (*    ahead         : int list; }*)

  type t = 
    { max_lookback : int;
      min_match    : int; (** NOTE currently min_match must be 3 *)
      map          : (int, int list, Int.comparator_witness) Map.t;
      behind       : int BatVect.t; }

  type result =
    | Empty
    | Literal of int
    | Pointer of int * int (** length, backwards distance*)

  let rec stream_take str n =
    if n = 0 then []
    else match Stream.next str with
      | Some i -> i :: stream_take str (n-1)
      | None -> []

  (*let create ~back ~ahead ~str =*) 
  (*  { max_lookback = back;*)
  (*    max_lookahead = ahead;*)
  (*    behind = [];*)
  (*    ahead = stream_take str ahead; }*)

  (*(** take `n` from the input buffer and put them into the*)
  (* * dictionary. refill the input buffer from the stream. *)*)
  (*let balance v n instr =*)
  (*  let blen = List.length v.behind in*)
  (*  let nahead = List.take v.ahead n in*)
  (*  let nb = List.append (List.rev nahead)*)
  (*      (if blen + n > v.max_lookback*)
  (*       then  (List.take v.behind (v.max_lookback - n))*)
  (*       else  v.behind) in*)
  (*  let na = List.append (List.drop v.ahead n) (stream_take instr n) in*)
  (*  { v with ahead = na; behind = nb; }*)

  (*(** find the longest match of two lists *)*)
  (*let match2 l1 l2 =*)
  (*  let rec mn2 l1 l2 n =*)
  (*    match l1, l2 with*)
  (*    | [], _ -> n*)
  (*    | _, [] -> n*)
  (*    | a :: l1', b :: l2' ->*)
  (*      if a = b then mn2 l1' l2' (n+1)*)
  (*      else n in*)
  (*  mn2 l1 l2 0*)

  (*let max_match_len =*) 
  (*  257*)

  (*let match_lngst  ~back  ~ahead =*)
  (*  (* Printf.printf "ahead_len: %d back_len: %d\n%s -- %s\n" *)*) 
  (*    (* (List.length ahead) (List.length back) *)*) 
  (*    (* (List.to_string ~f:Int.to_string (List.rev back)) *)*) 
  (*    (* (List.to_string ~f:Int.to_string ahead); *)*)
  (*  let rec domatch ll =*)
  (*    match ll with*)
  (*    | [] -> []*)
  (*    | _ :: tl -> match2 ll ahead :: domatch tl in*)
  (*  (* Printf.printf "before domatch\n"; *)*)
  (*  let lens = List.rev back |> domatch |> List.rev in*)
  (*  (* Printf.printf "before fold\n"; *)*)
  (*  let (_, d, ol) = List.fold lens ~init:(0, 0, None)*)
  (*    ~f:(fun (acc, i, mx) x ->*) 
  (*         let ni = acc + 1 in*)
  (*         if x > max_match_len then ni, i, mx*)
  (*         else if is_none mx then ni, ni, Some x*) 
  (*         else if Option.value_exn mx <= x then ni, ni, Some x*)
  (*         else ni, i, mx) in*)
  (*  (* Printf.printf "before return d: %d\n" d; *)*)
  (*  let l = Option.value ol ~default:0 in*)
  (*  if l <= 0 then Empty*)
  (*  else if l < 3 then Literal (List.hd_exn ahead)*)
  (*  else  Pointer (l, d)*)

  (*let find_match v str =*)
  (*  match match_lngst ~back:v.behind ~ahead:v.ahead with*)
  (*  | Literal i -> Literal i, balance v 1 str*)
  (*  | Pointer (l, d) -> Pointer (l, d), balance v l str*)
  (*  | Empty ->*) 
  (*    match List.hd v.ahead with*)
  (*    | None -> Empty, v*)
  (*    | Some i -> Literal i, balance v 1 str*)

  let hash a b c =
    (a lsl 16) lor (b lsl 8) lor c

  let exp_map v str n =
    let p = (BatVect.length v.behind) - 2 in
    let rec cmp_trp_hshs vs =
      match vs with
      | a :: b :: c :: _ -> 
        hash a b c :: cmp_trp_hshs (List.tl_exn vs)
      | _ -> [] in
    let xs = BatVect.get v.behind (p) :: 
             BatVect.get v.behind (p+1) :: 
             stream_take str n in
    (* compute the hashes of all triples *)
    let xs = cmp_trp_hshs xs in
    (* compute the indicies *)
    let xs = List.mapi xs ~f:(fun i v -> (i+p), v) in
    (* insert them into the dictionary (or update dict list) *)
    let nmap = List.fold_left xs ~init:v.map 
      ~f:(fun mac (i, h) -> Map.add_multi mac ~key:h ~data:i) in
    (* update the map *)
    {v with map = nmap}

  (* TODO *)
  (* let rec match_from str v n rs = *)
  (*   match Stream.next str with *)
  (*   | None -> n, rs *)
  (*   | Some i -> *)
  (*     match List.filter_map rs ~f:(fun idx -> *)
  (*         if BatVect.get v i = i then Some (idx+1) *)
  (*         else None)  with *)
  (*     | [] -> n, rs *)
  (*     | rs' -> match_until str v (n+1) rs' *)


  (* TODO *)
  let match_lngst v _str hsh =
    match Map.find v.map hsh with
    | None -> None, []
    | Some _is -> None, []
      (* match match_from str v with *)
      (* | _ -> raise Fatal *)  

  let find_match v str =
    match Stream.npeek v.min_match str with
    | Some (Some a) :: Some (Some b) :: Some (Some c) :: _ -> 
      (match hash a b c |> match_lngst v str with
       | Some _, _ -> Empty
       | None, _ -> Empty)
      (* take len from str and add to dict *)
      (* TODO Balance dict *)
    | Some (Some a) :: _ -> 
      (* TODO Balance dict *)
      (* next the input str and add to dict *)
      Literal a
    | _ -> Empty

end (* END OF MODULE *)

