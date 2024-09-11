type ('a, 'r) t =
  | Sink : {
        init: unit -> 's
      ; push: 's -> 'a -> 's
      ; full: 's -> bool
      ; stop: 's -> 'r
    }
      -> ('a, 'r) t

val hex : Hxd.cfg -> Format.formatter -> (string, unit) t

val jsonm :
  ?minify:bool -> ?len:int -> Format.formatter -> (Jsonm.lexeme, unit) t

val to_formatter : Format.formatter -> (string, unit) t
