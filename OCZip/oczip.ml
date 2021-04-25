(****************************)
(*        Gavin Gray        *)
(*    University of Utah    *)
(*    Spring 21 -- OCZip    *)
(****************************)

open Core
open Lib.Utils
open Lib.Defs
open Async.Deferred.Let_syntax

(*********************)
(* writing utilities *)
(*********************)

(* the constant file signature for local headers *)
let local_file_signature = 0x04034b50

let write_local_file_header ochnl fh = 
  write_4byte_le ochnl local_file_signature;
  (* required version *)
  let ver = match fh.c_method with | Store -> 10 | Deflate -> 20 in
  write_2byte_le ochnl ver;
  (* bit flags *)
  write_2byte_le ochnl 0; (* TODO this shouldn't be hard coded *)
  (* compression method *)
  write_2byte_le ochnl (get_compress_method_code fh.c_method);
  (* last modification time *)
  write_2byte_le ochnl fh.mtime;
  (* last modification date *)
  write_2byte_le ochnl fh.mdate;
  (* CRC 32 TODO not sure how to get this*)
  write_4byte_le ochnl fh.crc;
  (* compressed size *)
  write_4byte_le ochnl fh.csize;
  (* uncompressed size *)
  write_4byte_le ochnl fh.ucsize;
  (* file name size *)
  write_2byte_le ochnl (String.length fh.filename);
  (* XXX TODO extra comment size *)
  write_2byte_le ochnl 0;
  (* file name *)
  Out_channel.output_string ochnl fh.filename
(* extra comment *)
(* XXX TODO print out something 'empty string' *) 

(* the constant file signature for directory headers *)
let dir_file_signature = 0x02014b50

(* write the file header in central directory *)
let write_dir_file_header ochnl dh = 
  write_4byte_le ochnl dir_file_signature;
  (* version made by *)
  let ver = match dh.c_method with | Store -> 10 | Deflate -> 20 in
  write_2byte_le ochnl ver;
  (* required version *)
  write_2byte_le ochnl ver;
  (* bit flags *)
  write_2byte_le ochnl 0; (* TODO this shouldn't be hard coded *)
  (* compression method *)
  write_2byte_le ochnl (get_compress_method_code dh.c_method);
  (* last modification time *)
  write_2byte_le ochnl dh.mtime;
  (* last modification date *)
  write_2byte_le ochnl dh.mdate;
  (* CRC 32 this could be done when trying to deflate *)
  write_4byte_le ochnl (Zlib.crc32 (fn_to_byte_stream dh.filename));
  (* compressed size *)
  write_4byte_le ochnl  dh.csize;
  (* uncompressed size *)
  write_4byte_le ochnl dh.ucsize;
  (* file name length *)
  write_2byte_le ochnl (String.length dh.filename);
  (* extra comment length *)
  write_2byte_le ochnl 0;
  (* file comment length *)
  write_2byte_le ochnl 0;
  (* TODO the number of the disk on which this file exists *)
  write_2byte_le ochnl 0;
  (* Internal file attributes: *)
  write_2byte_le ochnl 0;
  (* External attr.	External file attributes: *)
  write_4byte_le ochnl 0;
  (* offset of local file header *)
  write_4byte_le ochnl (dh.offset);
  (* file name *)
  Out_channel.output_string ochnl dh.filename
(* extra comment *)
(* XXX what to write? *) 
(* file comment *)
(* XXX what to write? *) 

let end_dir_signature = 0x06054b50

let write_end_dir ochnl ndirs dirsize cdoff = 
  write_4byte_le ochnl end_dir_signature;
  (* disk number *)
  write_2byte_le ochnl 0;
  (* disk number on which the central directory starts *)
  write_2byte_le ochnl 0; 
  (* number of central dir entries on this disk *)
  write_2byte_le ochnl ndirs;
  (* total number of central dir entries *)
  write_2byte_le ochnl ndirs;
  (* central directory size *)
  write_4byte_le ochnl (int64_to_int dirsize);
  (* central directory disk offset *)
  write_4byte_le ochnl (int64_to_int cdoff);
  (* comment length *)
  write_2byte_le ochnl 0
(* optional comment *)
(* XXX what do I put here? *)

(* possibly return bytes written ?? *)
let rec output_stream ochnl dstr = 
  match Stream.next dstr with
  | Some b -> Out_channel.output_byte ochnl b;
    output_stream ochnl dstr
  | None -> ()

let open_file_with_stats fn = 
  let fstats = Unix.stat fn in
  { filename = fn;
    mtime    = getdosfmt_localtime fstats.st_mtime;
    mdate    = getdosfmt_date fstats.st_mtime;
    size     = fstats.st_size; }

let fn_to_header fn off comp_size =
  let ifile = open_file_with_stats fn in
  { c_method  = Deflate;
    mtime     = ifile.mtime;
    mdate     = ifile.mdate;
    crc       = Zlib.crc32 (fn_to_byte_stream ifile.filename);
    csize     = comp_size;
    ucsize    = (int64_to_int ifile.size);
    filename  = ifile.filename;
    offset    = off; }

(* compress each file concurrently and produce the new data stream 
 * if the 'compressed data' is larger than the original simply 
 * make a new stream to the original file.
 * NOTE currently it is very likely that the compressed data becomes
 * larger because the deflate algorithm only uses static huffman codes *)
let cons_files fns = 
  Async.Deferred.List.map fns ~f:(fun fn -> 
      let compressed_size, outstr = 
        fn_to_byte_stream fn |> Zlib.deflate in 
      let lfh = fn_to_header fn 0 compressed_size in
      let lfh, outstr = if lfh.csize > lfh.ucsize
        then { lfh with 
               csize = lfh.ucsize;
               c_method = Store;
               } , (fn_to_byte_stream fn)
        else lfh, outstr in
      let pct = 
        compute_ratio_int lfh.csize lfh.ucsize in
      let s = Printf.sprintf 
          "    adding: \"%s\" (in=%d) (out=%d) deflated ~> %.2f%%" 
          lfh.filename lfh.ucsize lfh.csize pct in
       Async.Deferred.return (lfh, outstr, s))

(* function to write all local headers 
 * data and central directory *)
let write_archive ochnl fns =
  let write_dir fs = 
    List.fold_left fs ~init:0
      ~f:(fun acc a -> write_dir_file_header ochnl a; acc + 1) in
  let%bind lfhs = cons_files fns in
  let lfhs = List.map lfhs ~f:(fun (lfh, ostr, s) ->
      let lfh = { 
        lfh with (* update the offset now that it is known *)
        offset = (Out_channel.pos ochnl |> int64_to_int) } in
      write_local_file_header ochnl lfh; 
      output_stream ochnl ostr; 
      print_endline s; lfh) in
  let s_pos   = Out_channel.pos ochnl in
  let ndirs   = write_dir lfhs in
  let e_pos   = Out_channel.pos ochnl in
  let dirsize = Int64.(-) e_pos s_pos in
  Async.Deferred.return (write_end_dir ochnl ndirs dirsize s_pos)

let compress ~srcs:infns ~tgt:outfn =
  Printf.printf "creating zip archive: \"%s\" ~\n" outfn;
  let p = (fun typ msg -> 
    Printf.printf (* don't touch the indenting here (: *)
      "~~ a %s error within oczip occured ~~
* msg: %s
* please report to yorelnivag@gmail.com\n" typ msg) in
  try let uniq_cons x xs = 
        if List.mem xs x ~equal:String.equal 
        then xs else x :: xs in
    let remove_from_right xs = 
      List.fold_right xs ~f:uniq_cons ~init:[] in
    let infns = remove_from_right infns in
    let ochnl = Out_channel.create ~binary:true outfn in
    write_archive ochnl infns 
    >>| fun () -> Out_channel.close ochnl
  with 
  | Zlib.ZlibExn s -> Async.Deferred.return (p "ZLIB" s)
  | _ -> Async.Deferred.return (p "FATAL" "no additional info available")

(******************)
(* main interface *)
(******************)

let command =
  Async.Command.async
    ~summary:"OCZIP : the OCaml file compression tool"
    ~readme:(fun () -> "TODO")
    Command.Let_syntax.(
      let%map_open 
        tgt = anon ("tgt" %: string) 
      and 
        srcs = anon (sequence ("filename" %: Filename.arg_type))
      in fun () -> compress ~srcs:srcs ~tgt:tgt)

let () =
  Command.run ~version:"0.1" ~build_info:"RWO" command

