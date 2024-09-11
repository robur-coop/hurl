type value = [ `Null | `Bool of bool | `String of string | `Float of float ]
type t = [ value | `A of t list | `O of (string * t) list ]
type 'a or_error = ('a, [ `Msg of string ]) result

val decode :
     ?size_chunk:int
  -> input:(bytes -> int -> int -> int)
  -> (t -> 'a or_error)
  -> 'a or_error

val of_string : ?size_chunk:int -> string -> t or_error
val of_location : ?size_chunk:int -> Fpath.t -> t or_error
val validate_location : ?size_chunk:int -> Fpath.t -> unit or_error

val input_to_lexemes :
     ?size_chunk:int
  -> ?finally:(unit -> unit)
  -> input:(bytes -> int -> int -> int)
  -> Jsonm.lexeme Seq.t

val seq_of_string_to_lexemes :
  ?size_chunk:int -> string Seq.t -> Jsonm.lexeme Seq.t

val location_to_lexemes : ?size_chunk:int -> Fpath.t -> Jsonm.lexeme Seq.t

val lexemes_to_seq_of_bytes :
  ?size_chunk:int -> ?minify:bool -> Jsonm.lexeme Seq.t -> string Seq.t

val to_lexemes : t -> Jsonm.lexeme Seq.t

(** Pretty-printers. *)

val pp : t Fmt.t
val pp_as_ocaml_string : ?minify:bool -> t Fmt.t
val pp_as_hurl_string : ?minify:bool -> t Fmt.t
