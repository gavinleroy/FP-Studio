(****************************)
(*        Gavin Gray        *)
(*    University of Utah    *)
(*    Spring 21 -- OCZip    *)
(****************************)
 
open Core

type nonrec compress_method = 
  | Store   (** No compression *) 
  | Deflate (** DEFLATE compression ~ https://tools.ietf.org/html/rfc1951#section-3 *)
    (** Compression used for file data storage *)

(* off | bytes | description ~ https://docs.fileformat.com/compression/zip/ 
 * 	0     4	Local file header signature # 0x04034b50 (read as a little-endian number)
 *  4	    2	Version needed to extract (minimum)
 *  6	    2	General purpose bit flag
 *  8	    2	Compression method
 *  10	  2	File last modification time
 *  12	  2	File last modification date
 *  14	  4	CRC-32
 *  18	  4	Compressed size
 *  22	  4	Uncompressed size
 *  26	  2	File name length (n)
 *  28	  2	Extra field length (m)
 *  30	  n	File Name
 *  30+n	m	Extra Field 
 *)

type local_file_header =
  { min_version: float; (** minimum version needed to read the file *)
    flags: int32; (** flags *)
    c_method: compress_method; (** method of compression *)
    mtime: int; (** time of last modification *)
    mdate: int; (** date of last modification *)
    crc: int32; (** crc32 checksum *)
    compressed_size: int; (** size of compressed file *)
    uncompressed_size: int; (** size of uncompressed file *)
    file_name_length: int32; (** length of the file name *)
    extra_field_length: int32; (** extra field length *)
    file_name: string; (** file name as a string *)
    extra: string; (** extra information *)
    file_offset: int64 (** file offset in the zip *)
  } (**  Contents describing the local file header *)

let local_file_header_signature = ()

(* takes a file name and returns a 
 * stream of bytes option represented in decimal *)
let fn_2_byte_stream fn = 
  let inc = In_channel.create ~binary:true fn in
    Stream.from
      (fun _ ->
        try Some (In_channel.input_byte inc) with End_of_file -> None)

let get_bytes fn =
    let inc = In_channel.create ~binary:true fn in
    let rec go sofar =
        match In_channel.input_char inc with
        | Some b -> go (Char.to_int b :: sofar)
        | None -> List.rev sofar
        | exception End_of_file -> List.rev sofar
    in
    let res = go [] in
    In_channel.close inc;
    res

let put_bytes _ ints =
    (* let outc = Out_channel.create fn in *)
    List.iter ints ~f: (fun b -> Out_channel.output_char stdout (Char.unsafe_of_int b))
    (* close_out outc *)

let compress infn outfn =
    put_bytes outfn (get_bytes infn)

(*********************)
(* writing utilities *)
(*********************)

let write_1byte ochnl n = 
  Out_channel.output_byte ochnl n

let write_2byte_le ochnl n = 
  (* output the lower byte *)
  write_1byte ochnl n; 
  (* output the upper byte *)
  write_1byte ochnl (n lsr 8)

let write_4byte_le ochnl n = 
  (* write the lower 2 bytes *)
  write_2byte_le ochnl n;
  (* write the upper two bytes *)
  write_2byte_le ochnl (n lsr 16)

(* TODO *)
let getdosfmt_of_localtime = ()

let get_compress_method_code cmthd = 
  match cmthd with
    | Store -> 0
    | Deflate -> 8

(* the constant file signature for headers *)
let file_signature = 0x04034b50

let write_directory_header = ()

let write_local_file_header ochnl fh = 
  write_4byte_le ochnl file_signature;
  (* required version *)
  write_2byte_le ochnl 0; (* TODO this shouldn't be hardcoded *)
  (* bit flags *)
  write_2byte_le ochnl 0; (* TODO this shouldn't be hard coded *)
  (* compression method *)
  write_2byte_le ochnl (get_compress_method_code fh.c_method);
  (* last modification time TODO *) (* NOTE these need to be in dos standard *)
  write_4byte_le ochnl 0;
  (* last modification date TODO *) (* NOTE these need to be in dos standard *)
  write_4byte_le ochnl 0;
  (* CRC 32 TODO not sure how to get this*)
  write_4byte_le ochnl 0;
  (* compressed size *)
  write_4byte_le ochnl fh.uncompressed_size;
  (* uncompressed size *)
  write_4byte_le ochnl fh.compressed_size;
  (* file name size *)
  write_4byte_le ochnl (String.length fh.file_name);
  (* extra comment size *)
  write_2byte_le ochnl 0;
  (* file name *)
  Out_channel.output_string ochnl fh.file_name
  (* extra comment *)
  (* XXX what to write? *) 
  

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
            (fun () -> compress src tgt)))

let () =
  Command.run ~version:"0.1" ~build_info:"RWO" command

