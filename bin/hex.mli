type n = |

type 'a command =
  | Rd : bytes * int * int -> (int, n) result command
  | Wr : int -> (int, n) result command
  | Bind : 'a command * ('a -> 'b command) -> 'b command
  | Return : 'a -> 'a command

val generate : Hxd.cfg -> Format.formatter -> (unit, n) result command
val consume : string -> 'a command -> 'a command
val finalize : 'a command -> 'a
