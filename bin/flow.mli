type ('a, 'b) t = { flow: 'r. ('b, 'r) Sink.t -> ('a, 'r) Sink.t } [@@unboxed]

val ( % ) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
val identity : ('a, 'a) t
val gzip : ?len:int -> unit -> (De.bigstring, De.bigstring) t
val deflate : ?len:int -> unit -> (De.bigstring, De.bigstring) t
val to_bigstring : ?len:int -> unit -> (string, De.bigstring) t
val to_string : (De.bigstring, string) t

val jsonm :
     ?encoding:Jsonm.encoding
  -> ?size_chunk:int
  -> unit
  -> (string, Jsonm.lexeme) t
