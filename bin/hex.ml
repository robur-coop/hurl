type n = |

module X = struct
  type 'a t =
    | Rd : bytes * int * int -> (int, n) result t
    | Wr : int -> (int, n) result t
    | Bind : 'a t * ('a -> 'b t) -> 'b t
    | Return : 'a -> 'a t
end

include Hxd.Make (X)

type 'a command = 'a X.t =
  | Rd : bytes * int * int -> (int, n) result command
  | Wr : int -> (int, n) result command
  | Bind : 'a command * ('a -> 'b command) -> 'b command
  | Return : 'a -> 'a command

let ( $ ) f g x = f (g x)

let scheduler =
  let bind x fn = inj (Bind (prj x, prj $ fn)) and return x = inj (Return x) in
  { Hxd.bind; return }

let seek = { Hxd.lseek= (fun _ _ _ -> assert false) }
let recv () buf ~off ~len = inj (Rd (buf, off, len))
let send () _str ~off:_ ~len = inj (Wr len)

let generate cfg ppf =
  Hxd.generate cfg scheduler recv send () () seek (`Relative 0) ppf |> prj

type 'a process =
  | Consumed of int * 'a
  | Continue_with of 'a
  | Apply : 'x process * ('x -> 'a command) -> 'a process

let rec expand : type a. string * int * int -> a command -> a process =
 fun (str, off, len) -> function
  | Return v -> Continue_with v
  | Rd (buf, off', len') ->
      let max = min len len' in
      Bytes.blit_string str off buf off' max;
      Consumed (max, Ok max)
  | Wr len -> Continue_with (Ok len)
  | Bind (x, fn) -> Apply (expand (str, off, len) x, fn)

let rec unroll : type a. a process -> int option = function
  | Apply (x, _) -> unroll x
  | Consumed (len, _) -> Some len
  | Continue_with _ -> None

let rec execute : type a. a process -> a command = function
  | Consumed (_, v) -> Return v
  | Continue_with v -> Return v
  | Apply (x, fn) -> (
      match execute x with Return v -> fn v | x -> Bind (x, fn))

let consume str command =
  let off = ref 0 and len = ref (String.length str) in
  let command = ref command in
  while !len > 0 do
    let process = expand (str, !off, !len) !command in
    begin
      match unroll process with
      | Some shift ->
          off := !off + shift;
          len := !len - shift
      | None -> ()
    end;
    command := execute process
  done;
  !command

let rec finalize : type a. a command -> a = function
  | Rd _ -> Ok 0
  | Wr len -> Ok len
  | Return v -> v
  | Bind (x, fn) -> finalize (fn (finalize x))
