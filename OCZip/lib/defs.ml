(****************************)
(*        Gavin Gray        *)
(*    University of Utah    *)
(*    Spring 21 -- OCZip    *)
(****************************)
 
type nonrec compress_method = 
  | Store   (** No compression *) 
  | Deflate (** DEFLATE compression ~ https://tools.ietf.org/html/rfc1951#section-3 *)
    (** Compression used for file data storage *)

type nonrec local_file_header =
  { (* flags              : int32;           (** flags *) *)
    c_method           : compress_method; (** method of compression *)
    mtime              : int;             (** time of last modification *)
    mdate              : int;             (** date of last modification *)
    crc                : int;           (** crc32 checksum *)
    compressed_size    : int64;             (** size of compressed file *)
    uncompressed_size  : int64;             (** size of uncompressed file *)
    (* file_name_length   : int32;             (** length of the file name *) *)
    (* extra_field_length : int32;             (** extra field length *) *)
    filename          : string;          (** file name as a string *)
    (* extra              : string;           (** extra information *) *)
    file_offset        : int;             (** file offset in the zip *)
  } (**  Contents describing the local file header *)

type nonrec inp_archive = 
  { filename : string;
    mtime    : int;
    mdate    : int;
    size     : int64;
  } (** input file archive for reading *)

