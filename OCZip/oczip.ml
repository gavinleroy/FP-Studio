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
  write_2byte_le ochnl 10; (* TODO this shouldn't be hardcoded *)
  (* bit flags *)
  write_2byte_le ochnl 0; (* TODO this shouldn't be hard coded *)
  (* compression method *)
  write_2byte_le ochnl (get_compress_method_code fh.c_method);
  (* last modification time *)
  write_2byte_le ochnl fh.mtime;
  (* last modification date *)
  write_2byte_le ochnl fh.mdate;
  (* CRC 32 TODO not sure how to get this*)
  write_4byte_le ochnl 0;
  (* compressed size *)
  write_4byte_le ochnl (int64_to_int fh.uncompressed_size);
  (* uncompressed size *)
  write_4byte_le ochnl (int64_to_int fh.compressed_size);
  (* file name size *)
  write_2byte_le ochnl (String.length fh.file_name);
  (* extra comment size *)
  write_2byte_le ochnl 0;
  (* file name *)
  Out_channel.output_string ochnl fh.file_name
  (* extra comment *)
  (* XXX what to write? *) 

(* the constant file signature for directory headers *)
let dir_file_signature = 0x02014b50

(* write the file header in central directory *)
let write_dir_file_header ochnl dh = 
  write_4byte_le ochnl dir_file_signature;
  (* version made by *)
  write_2byte_le ochnl 10; (* TODO this shouldn't be hardcoded *)
  (* required version *)
  write_2byte_le ochnl 10; (* TODO this shouldn't be hardcoded *)
  (* bit flags *)
  write_2byte_le ochnl 0; (* TODO this shouldn't be hard coded *)
  (* compression method *)
  write_2byte_le ochnl (get_compress_method_code dh.c_method);
  (* last modification time *)
  write_2byte_le ochnl dh.mtime;
  (* last modification date *)
  write_2byte_le ochnl dh.mdate;
  (* CRC 32 TODO not sure how to get this*)
  write_4byte_le ochnl 0;
  (* compressed size *)
  write_4byte_le ochnl (int64_to_int dh.uncompressed_size);
  (* uncompressed size *)
  write_4byte_le ochnl (int64_to_int dh.compressed_size);
  (* file name length *)
  write_2byte_le ochnl (String.length dh.file_name);
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
  Out_channel.output_string ochnl dh.file_name
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
    | None   -> ()

(* function to write all local headers 
 * data and 
 *central directory *)
let write_archive tgt =
  let rec write_file fs = 
    match fs with
      | lfh :: fs' ->
        write_local_file_header tgt.ochnl lfh;
        output_stream tgt.ochnl lfh.data;
        (* return data for central dir *)
        write_file fs'
      | [] -> ()
  in
  let rec write_dir fs = 
    match fs with
      | lfh :: fs' ->
        write_dir_file_header tgt.ochnl lfh;
        1 + write_dir fs'
      | [] -> 0
  in
  let _ : unit = write_file tgt.files in
  let s_pos = Out_channel.pos tgt.ochnl in
  let ndirs = write_dir tgt.files in
  let e_pos = Out_channel.pos tgt.ochnl in
  let dirsize = Int64.(-) e_pos s_pos in
  write_end_dir tgt.ochnl ndirs dirsize s_pos

(* takes a file name and returns a 
 * stream of bytes option represented in decimal *)
let fn_to_byte_stream fn = 
  let inc = In_channel.create ~binary:true fn in
    Stream.from
      (fun _ ->
        try Some (In_channel.input_byte inc) with End_of_file -> None)

let open_file_with_stats fn = 
  let fstats = Unix.stat fn in
  let iarch = 
    { filename = fn;
      ichnl    = fn_to_byte_stream fn;
      mtime    = getdosfmt_localtime fstats.st_mtime;
      mdate    = getdosfmt_date fstats.st_mtime;
      size     = fstats.st_size; } in 
  iarch

let fn_to_header fn =
  let ifile = open_file_with_stats fn in
  (* TODO we need to compress/encrypt the byte stream and get the new count  *)
  { c_method           = Store;
    mtime              = ifile.mtime;
    mdate              = ifile.mdate;
    crc                = 0; (* compute the crc?? TODO  *)
    compressed_size    = ifile.size;
    uncompressed_size  = ifile.size; (* this needs to get ocmputed by zlib TODO  *)
    file_name          = ifile.filename;
    file_offset        = 0; 
    data               = ifile.ichnl; }

let compress_input tgt infn = 
  let fe = fn_to_header infn in
    { tgt with files = fe :: tgt.files }

let compress ~src:infn ~tgt:outfn =
  let tgt = 
    { filename = outfn;
      ochnl    = Out_channel.create ~binary:true outfn;
      files    = [];
    } in
  (* write_end_dir tgt.ochnl 0 (Int64.of_int 0); *)
  write_archive (compress_input tgt infn);
  Out_channel.close tgt.ochnl
  

(******************)
(* main interface *)
(******************)

let command =
  Command.basic
    ~summary:"TODO"
    ~readme:(fun () -> "TODO")
    Command.Param.(
      map (both
            (anon ("tgt" %: string))
            (anon ("src" %: string)))
       ~f:(fun (tgt,src) ->
            (fun () -> compress ~src:src ~tgt:tgt)))

let () =
  Command.run ~version:"0.1" ~build_info:"RWO" command

