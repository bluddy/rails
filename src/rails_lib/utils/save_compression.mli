(** Compression utilities for save games using camlzip (Zlib). *)

val compress_string : ?level:int -> string -> string
(** [compress_string ?level str] compresses [str] using zlib format.
    Default compression level is 6. *)

val decompress_string : string -> (string, string) result
(** [decompress_string str] decompresses a zlib-compressed string.
    Returns [Ok uncompressed] or [Error err_msg]. *)

val read_save_data : string -> (string, string) result
(** [read_save_data raw_data] transparently handles both compressed and
    legacy uncompressed save data. *)

val read_save_file : string -> (string, string) result
(** [read_save_file path] reads a save file from disk and returns the
    uncompressed string content. *)

val write_save_file : ?level:int -> string -> string -> (unit, string) result
(** [write_save_file ?level path data] compresses [data] and atomically
    writes it to [path] via a temporary file. *)
