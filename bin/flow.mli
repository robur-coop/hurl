(* Copyright (c) 2020 Rizo I. <rizo@odis.io>
   Copyright (c) 2022 Romain Calascibetta <romain.calascibetta@gmail.com>

   Permission to use, copy, modify, and distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*)

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
