(****************************)
(*        Gavin Gray        *)
(*    University of Utah    *)
(*    Spring 21 -- OCZip    *)
(****************************)

module LZ77 = struct

  open Core

  type t = 
    { max_lookback  : int;
      max_lookahead : int;
      behind        : int list;
      ahead         : int list; }

  type result =
    | Empty
    | Literal of int
    (* | Pointer of int * int *)

  let rec stream_take str n =
    if n = 0 then []
    else match Stream.next str with
      | Some i -> i :: stream_take str (n-1)
      | None -> []

  let create ~back ~ahead ~str = 
    { max_lookback = back;
      max_lookahead = ahead;
      behind = [];
      ahead = stream_take str ahead; }

  let match_lngst (* ~back *) ~ahead =
    match List.hd ahead with
    | Some x -> Literal x
    | None -> Empty

  let balance v n instr =
    let blen = List.length v.behind in
    let nahead = List.take v.ahead n in
    let nb = List.append nahead (if blen + n > v.max_lookback
      then  (List.take v.behind (v.max_lookback - n))
      else  v.behind) in
    let na = List.append (List.drop v.ahead n) (stream_take instr n) in
    { v with ahead = na; behind = nb; }

  let find_match v str =
    match match_lngst (* ~back:v.behind *) ~ahead:v.ahead with
    | Empty -> Empty, v
    | Literal i -> Literal i, balance v 1 str

end

