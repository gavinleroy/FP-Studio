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
  { c_method : compress_method; (** method of compression *)
    mtime    : int;             (** time of last modification *)
    mdate    : int;             (** date of last modification *)
    crc      : int;             (** crc32 checksum *)
    csize    : int;             (** size of compressed file *)
    ucsize   : int;             (** size of uncompressed file *)
    filename : string;          (** file name as a string *)
    offset   : int;             (** file offset in the zip *)
  } (**  Contents describing the local file header *)

type nonrec inp_archive = 
  { filename : string;
    mtime    : int;
    mdate    : int;
    size     : int64;
  } (** input file archive for reading *)

