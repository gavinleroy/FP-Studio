(****************************)
(*        Gavin Gray        *)
(*    University of Utah    *)
(*    Spring 21 -- OCZip    *)
(****************************)

val write_1byte: Core.Out_channel.t -> int -> unit

val write_2byte_le: Core.Out_channel.t -> int -> unit

val write_4byte_le: Core.Out_channel.t -> int -> unit

val getdosfmt_localtime: float -> int

val getdosfmt_date: float -> int

val get_compress_method_code: Defs.compress_method -> int

val int64_to_int: Int64.t -> int

