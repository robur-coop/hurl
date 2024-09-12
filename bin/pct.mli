(* Percent-encoding for the query part and normalize the path part. *)

val encode : (string * string list) list -> string
(** [encode kvs] generates the query part of an url from a key-values list. *)

val path : string -> string
(** [path str] normalizes the given path and escape non-allowed characters. *)
