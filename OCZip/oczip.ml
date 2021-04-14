(****************************)
(*        Gavin Gray        *)
(*    University of Utah    *)
(*    Spring 21 -- OCZip    *)
(****************************)

open Core
open Lib.Utils
open Lib.Defs

(*********************)
(* writing utilities *)
(*********************)

(* the constant file signature for local headers *)
let local_file_signature = 0x04034b50

let write_local_file_header ochnl fh = 
  write_4byte_le ochnl local_file_signature;
  (* required version *)
  let ver = match fh.c_method with | Store -> 10 | Deflate -> 20 in
  write_2byte_le ochnl ver; (* TODO this shouldn't be hardcoded *)
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
  write_4byte_le ochnl fh.compressed_size;
  (* uncompressed size *)
  write_4byte_le ochnl fh.uncompressed_size;
  (* file name size *)
  write_2byte_le ochnl (String.length fh.filename);
  (* extra comment size *)
  write_2byte_le ochnl 0;
  (* file name *)
  Out_channel.output_string ochnl fh.filename
(* extra comment *)
(* XXX what to write? *) 

(* the constant file signature for directory headers *)
let dir_file_signature = 0x02014b50

(* write the file header in central directory *)
let write_dir_file_header ochnl dh = 
  write_4byte_le ochnl dir_file_signature;
  (* version made by *)
  let ver = match dh.c_method with | Store -> 10 | Deflate -> 20 in
  write_2byte_le ochnl ver; (* TODO this shouldn't be hardcoded *)
  (* required version *)
  write_2byte_le ochnl ver; (* TODO this shouldn't be hardcoded *)
  (* bit flags *)
  write_2byte_le ochnl 0; (* TODO this shouldn't be hard coded *)
  (* compression method *)
  write_2byte_le ochnl (get_compress_method_code dh.c_method);
  (* last modification time *)
  write_2byte_le ochnl dh.mtime;
  (* last modification date *)
  write_2byte_le ochnl dh.mdate;
  (* CRC 32 not sure how to get this*)
  write_4byte_le ochnl (Zlib.crc32 (fn_to_byte_stream dh.filename));
  (* compressed size *)
  write_4byte_le ochnl  dh.compressed_size;
  (* uncompressed size *)
  write_4byte_le ochnl dh.uncompressed_size;
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
  write_4byte_le ochnl (dh.file_offset);
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

(* possibly return bytes written TODO *)
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
  (* { c_method           = Store; *)
  { c_method           = Deflate;
    mtime              = ifile.mtime;
    mdate              = ifile.mdate;
    crc                = Zlib.crc32 (fn_to_byte_stream ifile.filename);
    compressed_size    = comp_size;
    uncompressed_size  = (int64_to_int ifile.size);
    filename           = ifile.filename;
    file_offset        = off; }

(* function to write all local headers 
 * data and central directory *)
let write_archive ochnl fns =
  let rec write_files fns = 
    match fns with
    | fn :: fs' -> 
      let compressed_size, outstr = 
        (Zlib.deflate (fn_to_byte_stream fn)) in 
      let lfh = fn_to_header fn 
          (int64_to_int (Out_channel.pos ochnl)) 
          compressed_size in
      let pct = 
        (Int.to_float compressed_size) 
        /. (Int.to_float lfh.uncompressed_size) 
        *. 100. in
      (* probably shouldn't print in whichever function *)
      Printf.printf "~ ADDING \"%s\" ~~ DEFLATED ~> %.2f%%\n" lfh.filename pct;
      write_local_file_header ochnl lfh;
      output_stream ochnl outstr;
      lfh :: write_files fs'
    | [] -> [] in
  let rec write_dir fs = 
    match fs with
    | lfh :: fs' ->
      write_dir_file_header ochnl lfh;
      1 + write_dir fs'
    | [] -> 0 in
  let lfhs    = write_files fns in
  let s_pos   = Out_channel.pos ochnl in
  let ndirs   = write_dir lfhs in
  let e_pos   = Out_channel.pos ochnl in
  let dirsize = Int64.(-) e_pos s_pos in
  write_end_dir ochnl ndirs dirsize s_pos

let create_zip ~srcs:infns ~tgt:outfn =
  let ochnl = Out_channel.create ~binary:true outfn in
  write_archive ochnl infns; 
  Out_channel.close ochnl

(******************)
(* main interface *)
(******************)

let command =
  Command.basic
    ~summary:"OCZIP : the OCaml file compression tool"
    ~readme:(fun () -> "TODO")
    Command.Let_syntax.(
      let%map_open 
        tgt = anon ("tgt" %: string) 
      and 
        srcs = anon (sequence ("filename" %: Filename.arg_type))
      in fun () -> 
        try create_zip ~srcs:srcs ~tgt:tgt
        with _ -> print_endline "** a fatal error occured **")

let () =
  Command.run ~version:"0.1" ~build_info:"RWO" command

