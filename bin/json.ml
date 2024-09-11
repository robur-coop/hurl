type value = [ `Null | `Bool of bool | `String of string | `Float of float ]
type t = [ value | `A of t list | `O of (string * t) list ]
type 'a or_error = ('a, [ `Msg of string ]) result
type await = [ `Await ]
type error = [ `Error of Jsonm.error ]
type eoi = [ `End ]

let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

let rec pp ppf = function
  | `Null -> Fmt.const Fmt.string "()" ppf ()
  | `Bool v -> Fmt.bool ppf v
  | `Float v -> Fmt.float ppf v
  | `Name v -> Fmt.string ppf v
  | `String v -> Fmt.string ppf v
  | `O lst -> Fmt.Dump.list (Fmt.Dump.pair Fmt.string pp) ppf lst
  | `A lst -> Fmt.Dump.list pp ppf lst

let validate ?(size_chunk = 0x800) ~input k =
  let decoder = Jsonm.decoder `Manual in
  let buf = Bytes.create size_chunk in
  let error (`Error err) =
    error_msgf "Invalid JSON input: %a" Jsonm.pp_error err
  in
  let rec go stack = function
    | #eoi -> k ()
    | #error as v -> error v
    | `Lexeme `Os -> go (`Object :: stack) (Jsonm.decode decoder)
    | `Lexeme `As -> go (`Array :: stack) (Jsonm.decode decoder)
    | `Lexeme ((`Oe | `Ae) as lexeme) -> begin
        match (stack, lexeme) with
        | `Object :: stack, `Oe -> go stack (Jsonm.decode decoder)
        | `Array :: stack, `Ae -> go stack (Jsonm.decode decoder)
        | _ -> error_msgf "Malformed JSON input"
      end
    | `Lexeme _ -> go stack (Jsonm.decode decoder)
    | #await -> (
        try
          let len = input buf 0 (Bytes.length buf) in
          if len = 0 then
            match stack with
            | [] -> k ()
            | _ -> error_msgf "Unterminated JSON object"
          else begin
            Jsonm.Manual.src decoder buf 0 len;
            go stack (Jsonm.decode decoder)
          end
        with End_of_file -> error_msgf "Partial JSON input")
  in
  go [] `Await

let decode ?(size_chunk = 0x800) ~input k =
  let decoder = Jsonm.decoder `Manual in
  let buf = Bytes.create size_chunk in
  let rec await k `Await =
    match input buf 0 size_chunk with
    | len ->
        Jsonm.Manual.src decoder buf 0 len;
        k ()
    | exception End_of_file -> error_msgf "Partial JSON input"
  and error (`Error err) =
    error_msgf "Invalid JSON input: %a" Jsonm.pp_error err
  and end_of_input `End = error_msgf "Unexpected end of input"
  and arr acc k =
    match Jsonm.decode decoder with
    | #await as v -> await (fun () -> arr acc k) v
    | #error as v -> error v
    | #eoi as v -> end_of_input v
    | `Lexeme `Ae -> k (`A (List.rev acc))
    | `Lexeme v -> core (fun v -> arr (v :: acc) k) v
  and name n k =
    match Jsonm.decode decoder with
    | #await as v -> await (fun () -> name n k) v
    | #error as v -> error v
    | #eoi as v -> end_of_input v
    | `Lexeme v -> core (fun v -> k (n, v)) v
  and obj acc k =
    match Jsonm.decode decoder with
    | #await as v -> await (fun () -> obj acc k) v
    | #error as v -> error v
    | #eoi as v -> end_of_input v
    | `Lexeme `Oe -> k (`O (List.rev acc))
    | `Lexeme (`Name n) -> name n (fun v -> obj (v :: acc) k)
    | `Lexeme v ->
        error_msgf "Unexpected lexeme: %a (expected key)" Jsonm.pp_lexeme v
  and core k = function
    | #value as v -> k v
    | `Os -> obj [] k
    | `As -> arr [] k
    | `Ae | `Oe -> error_msgf "Retrieve invalid end of JSON array/object"
    | `Name _ -> error_msgf "Retrieve invalid JSON key value"
  and top () =
    match Jsonm.decode decoder with
    | #await as v -> await top v
    | #error as v -> error v
    | #eoi -> k `Null
    | `Lexeme (#Jsonm.lexeme as lexeme) -> core k lexeme
  in
  top ()

module Stack = struct
  type stack =
    | In_array of t list * stack
    | In_object of (string * t) list * stack
    | Empty
end

let encode ?minify ?(size_chunk = 0x800) ~output t =
  let encoder = Jsonm.encoder ?minify `Manual in
  let buf = Bytes.create size_chunk in
  let rec encode k stack value =
    match Jsonm.encode encoder value with
    | `Ok -> k stack
    | `Partial ->
        let len = Bytes.length buf - Jsonm.Manual.dst_rem encoder in
        output (Bytes.sub_string buf 0 len);
        Jsonm.Manual.dst encoder buf 0 (Bytes.length buf);
        encode k stack `Await
  and value k v stack =
    match v with
    | #value as v -> encode (continue k) stack (`Lexeme v)
    | `O ms -> encode (obj k ms) stack (`Lexeme `Os)
    | `A vs -> encode (arr k vs) stack (`Lexeme `As)
  and obj k ms stack =
    match ms with
    | (n, v) :: ms ->
        let stack = Stack.In_object (ms, stack) in
        encode (value k v) stack (`Lexeme (`Name n))
    | [] -> encode (continue k) stack (`Lexeme `Oe)
  and arr k vs stack =
    match vs with
    | v :: vs ->
        let stack = Stack.In_array (vs, stack) in
        value k v stack
    | [] -> encode (continue k) stack (`Lexeme `Ae)
  and continue k = function
    | Stack.In_array (vs, stack) -> arr k vs stack
    | Stack.In_object (ms, stack) -> obj k ms stack
    | Stack.Empty as stack -> encode k stack `End
  in
  Jsonm.Manual.dst encoder buf 0 (Bytes.length buf);
  value (Fun.const ()) t Stack.Empty

let ( ++ ) = Seq.cons

let rec to_lexemes seq = function
  | #value as v -> Seq.cons (v :> Jsonm.lexeme) seq
  | `O ms ->
      let seq =
        List.fold_left
          begin
            fun seq (k, v) -> `Name k ++ to_lexemes seq v
          end
          (`Oe ++ seq) (List.rev ms)
      in
      `Os ++ seq
  | `A vs ->
      let seq =
        List.fold_left
          begin
            fun seq v -> to_lexemes seq v
          end
          (`Ae ++ seq) (List.rev vs)
      in
      `As ++ seq

let to_lexemes t = to_lexemes Seq.empty t
let _max_young_size = Sys.word_size / 8 * 256

let of_string ?size_chunk str =
  let size_chunk =
    match size_chunk with
    | None when String.length str <= _max_young_size -> String.length str
    | None -> 0x800
    | Some len -> len
  in
  let pos = ref 0 in
  let input buf off len =
    let len = max len (String.length str - !pos) in
    Bytes.blit_string str !pos buf off len;
    pos := !pos + len;
    len
  in
  decode ~size_chunk ~input Result.ok

let of_location ?size_chunk location =
  let ic = open_in (Fpath.to_string location) in
  let ln = in_channel_length ic in
  let size_chunk =
    match size_chunk with
    | None when ln < _max_young_size -> ln
    | None -> 0x800
    | Some len -> len
  in
  let finally () = close_in ic in
  Fun.protect ~finally @@ fun () ->
  decode ~size_chunk ~input:(input ic) Result.ok

let validate_location ?size_chunk location =
  let ic = open_in (Fpath.to_string location) in
  let ln = in_channel_length ic in
  let size_chunk =
    match size_chunk with
    | None when ln < _max_young_size -> ln
    | None -> 0x800
    | Some len -> len
  in
  let finally () = close_in ic in
  Fun.protect ~finally @@ fun () ->
  validate ~size_chunk ~input:(input ic) Result.ok

let pp_as_ocaml_string ?minify ppf v =
  let buf = Buffer.create 0x800 in
  let output str = Buffer.add_string buf str in
  encode ?minify ~output v;
  Fmt.pf ppf "%S" (Buffer.contents buf)

let pp_as_hurl_string ?minify ppf v =
  let buf = Buffer.create 0x800 in
  let output str = Buffer.add_string buf str in
  encode ?minify ~output v;
  Fmt.pf ppf "'%s'" (Str.hurl_escape (Buffer.contents buf))

let input_to_lexemes ?(size_chunk = 0x800) ?(finally = Fun.const ()) ~input =
  let decoder = Jsonm.decoder `Manual in
  let buf = Bytes.create size_chunk in
  let rec await k `Await =
    match input buf 0 size_chunk with
    | len ->
        Jsonm.Manual.src decoder buf 0 len;
        k ()
    | exception End_of_file ->
        begin
          try finally () with exn -> raise (Fun.Finally_raised exn)
        end;
        Fmt.failwith "Partial JSON input"
  and error (`Error err) =
    begin
      try finally () with exn -> raise (Fun.Finally_raised exn)
    end;
    Fmt.failwith "Invalid JSON input: %a" Jsonm.pp_error err
  and top () =
    match Jsonm.decode decoder with
    | #await as v -> await top v
    | #error as v -> error v
    | #eoi ->
        begin
          try finally () with exn -> raise (Fun.Finally_raised exn)
        end;
        None
    | `Lexeme (#Jsonm.lexeme as lexeme) -> Some lexeme
  in
  Seq.of_dispenser top

let location_to_lexemes ?size_chunk location =
  let ic = open_in (Fpath.to_string location) in
  let ln = in_channel_length ic in
  let size_chunk =
    match size_chunk with
    | None when ln <= _max_young_size -> ln
    | None -> 0x800
    | Some len -> len
  in
  let finally () = close_in ic in
  input_to_lexemes ~size_chunk ~finally ~input:(input ic)

let seq_of_string_to_lexemes ?size_chunk (seq : string Seq.t) =
  let seq = ref seq in
  let str = ref "" and len = ref 0 in
  let input buf off len' =
    if !len = 0 then
      match Seq.uncons !seq with
      | None -> 0
      | Some (str', seq') ->
          let () = seq := seq' in
          let () = str := str' in
          let () = len := String.length str' in
          let max = min len' !len in
          let () = Bytes.blit_string str' 0 buf off max in
          let () = len := !len - max in
          max
    else
      let max = min len' !len in
      let () = Bytes.blit_string !str (String.length !str - !len) buf off max in
      let () = len := !len - max in
      max
  in
  input_to_lexemes ?size_chunk ?finally:None ~input

let lexemes_to_seq_of_bytes ?(size_chunk = 0x800) ?(minify = false)
    (seq : Jsonm.lexeme Seq.t) =
  let encoder = Jsonm.encoder ~minify `Manual in
  let buf = Bytes.create size_chunk in
  let rec partial seq = function
    | `Ok -> seq
    | `Partial ->
        let len = Bytes.length buf - Jsonm.Manual.dst_rem encoder in
        let str = Bytes.sub_string buf 0 len in
        Jsonm.Manual.dst encoder buf 0 (Bytes.length buf);
        partial Seq.(append seq (return str)) (Jsonm.encode encoder `Await)
  and fn action = partial Seq.empty (Jsonm.encode encoder action) in
  Jsonm.Manual.dst encoder buf 0 (Bytes.length buf);
  Seq.flat_map fn Seq.(append (map (fun l -> `Lexeme l) seq) (return `End))

(*
module Prettier = struct
  type t =
    { decoder : Jsonm.decoder
    ; stack : [ `Object | `Array ] list
    ; stdout : Format.formatter }

  let decode t str =
    let rec until_await to_align stack decoder = match Jsonm.decode decoder, stack with
      | `Await, _ -> { decoder; stack; stdout= t.stdout }
      | `Lexeme `Os ->
          Fmt.pf t.stdout "%a{@\n%!" (pp_align ~to_align) stack;
          until_await true (succ stack) decoder
      | `Lexeme `As ->
          Fmt.pf t.stdout "%a[@\n%!" (pp_align ~to_align) stack;
          until_await true (succ stack) decoder
      | `Lexeme (`Name str) ->
          Fmt.pf t.stdout "%a%S:@ " pp_align stack;
          until_await false (succ stack) decoder
      | `Lexeme (`Bool v) ->
          Fmt.pf t.stdout "%a%b" (pp_align ~to_align) stack v;
          until_await true (succ stack) decoder

end
*)
