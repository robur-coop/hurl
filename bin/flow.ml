let src = Logs.Src.create "hurl.flow"

module Log = (val Logs.src_log src : Logs.LOG)

type ('a, 'b) t = { flow: 'r. ('b, 'r) Sink.t -> ('a, 'r) Sink.t } [@@unboxed]

let ( % ) { flow= f } { flow= g } = { flow= (fun sink -> f (g sink)) }
let identity = { flow= Fun.id }

let rec inflate_gzip_until_await ~push ~acc decoder o =
  match Gz.Inf.decode decoder with
  | `Await decoder -> `Continue (decoder, o, acc)
  | `Flush decoder ->
      let len = Bigarray.Array1.dim o - Gz.Inf.dst_rem decoder in
      Log.debug (fun m -> m "%d byte(s) inflated" len);
      let acc = push acc (Bigarray.Array1.sub o 0 len) in
      inflate_gzip_until_await ~push ~acc (Gz.Inf.flush decoder) o
  | `End decoder ->
      let len = Bigarray.Array1.dim o - Gz.Inf.dst_rem decoder in
      let acc = push acc (Bigarray.Array1.sub o 0 len) in
      `Stop (decoder, o, acc)
  | `Malformed err -> failwith err

let rec inflate_gzip_until_end ~push ~acc decoder o =
  match Gz.Inf.decode decoder with
  | `Await _decoder -> `Partial
  | `Flush decoder ->
      let len = Bigarray.Array1.dim o - Gz.Inf.dst_rem decoder in
      let acc = push acc (Bigarray.Array1.sub o 0 len) in
      inflate_gzip_until_end ~push ~acc (Gz.Inf.flush decoder) o
  | `End decoder ->
      let len = Bigarray.Array1.dim o - Gz.Inf.dst_rem decoder in
      let acc = push acc (Bigarray.Array1.sub o 0 len) in
      `Ok acc
  | `Malformed err -> failwith err

let gzip ?(len = De.io_buffer_size) () =
  let flow (Sink.Sink k) =
    let init () =
      let o = De.bigstring_create len in
      let decoder = Gz.Inf.decoder `Manual ~o in
      let[@warning "-8"] (`Await decoder
                           : [ `End of _
                             | `Flush of _
                             | `Malformed of _
                             | `Await of _ ]) =
        Gz.Inf.decode decoder
      in
      let acc = k.init () in
      (decoder, o, false, acc)
    in
    let push (decoder, o, full, acc) i =
      Log.debug (fun m -> m "inflate %d byte(s)" (Bigarray.Array1.dim i));
      if Bigarray.Array1.dim i = 0 then (decoder, o, full, acc)
      else if not full then
        let gzip = Gz.Inf.src decoder i 0 (Bigarray.Array1.dim i) in
        match inflate_gzip_until_await ~push:k.push ~acc gzip o with
        | `Continue (decoder, o, acc) -> (decoder, o, false, acc)
        | `Stop (decoder, o, acc) -> (decoder, o, true, acc)
      else (decoder, o, true, acc)
    in
    let full (_, _, full, acc) = full || k.full acc in
    let stop (encoder, o, full, acc) =
      if full then k.stop acc
      else
        match inflate_gzip_until_end ~push:k.push ~acc encoder o with
        | `Partial -> Fmt.failwith "Partial GZip content"
        | `Ok acc -> k.stop acc
    in
    Sink.Sink { init; stop; full; push }
  in
  { flow }

let rec push_until_free ~push acc (o, free) (str, off, len) =
  if len > 0 then begin
    let max = min free len in
    let dst_off = Bigstringaf.length o - free in
    Log.debug (fun m -> m "copy %d byte(s) (free %d byte(s))" max free);
    Bigstringaf.blit_from_string str ~src_off:off o ~dst_off ~len:max;
    if free - max <= 0 then
      push_until_free ~push (push acc o) (o, 0) (str, off + max, len - max)
    else push_until_free ~push acc (o, free - max) (str, off + max, len - max)
  end
  else (o, free, acc)

let to_bigstring ?(len = De.io_buffer_size) () =
  let flow (Sink.Sink k) =
    let init () =
      let o = De.bigstring_create len in
      (o, len, k.init ())
    and push (o, free, acc) str =
      push_until_free ~push:k.push acc (o, free) (str, 0, String.length str)
    and full (_, _, acc) = k.full acc
    and stop (o, free, acc) =
      let acc =
        if free < Bigstringaf.length o then
          k.push acc Bigstringaf.(sub o ~off:0 ~len:(length o - free))
        else acc
      in
      k.stop acc
    in
    Sink.Sink { init; stop; full; push }
  in
  { flow }

let to_string =
  let flow (Sink.Sink k) =
    let init () = k.init ()
    and push acc bstr =
      let str = Bigstringaf.to_string bstr in
      k.push acc str
    and full = k.full
    and stop = k.stop in
    Sink.Sink { init; stop; full; push }
  in
  { flow }

let rec until_all_consumed ~push acc buf decoder (str, off, len) =
  if len = 0 then (acc, false)
  else
    match Jsonm.decode decoder with
    | `Await ->
        let max = min len (Bytes.length buf) in
        Bytes.blit_string str off buf 0 max;
        Jsonm.Manual.src decoder buf 0 max;
        until_all_consumed ~push acc buf decoder (str, off + max, len - max)
    | `End -> (acc, true)
    | `Lexeme lexeme ->
        until_all_consumed ~push (push acc lexeme) buf decoder (str, off, len)
    | `Error err -> Fmt.invalid_arg "Invalid JSON input: %a" Jsonm.pp_error err

let rec until_end ~push acc decoder =
  match Jsonm.decode decoder with
  | `Await ->
      Jsonm.Manual.src decoder Bytes.empty 0 0;
      until_end ~push acc decoder
  | `Lexeme lexeme -> until_end ~push (push acc lexeme) decoder
  | `End -> acc
  | `Error err -> Fmt.invalid_arg "Invalid JSON input: %a" Jsonm.pp_error err

let jsonm ?encoding ?(size_chunk = 0x800) () =
  let flow (Sink.Sink k) =
    let init () =
      let decoder = Jsonm.decoder ?encoding `Manual in
      let buf = Bytes.create size_chunk in
      (decoder, buf, k.init (), false)
    in
    let push (decoder, buf, acc, closed) str =
      if closed then (decoder, buf, acc, closed)
      else
        let acc, closed =
          until_all_consumed ~push:k.push acc buf decoder
            (str, 0, String.length str)
        in
        (decoder, buf, acc, closed)
    in
    let full (_, _, acc, closed) = closed || k.full acc in
    let stop (decoder, _, acc, closed) =
      if closed then k.stop acc
      else
        let acc = until_end ~push:k.push acc decoder in
        k.stop acc
    in
    Sink.Sink { init; stop; full; push }
  in
  { flow }
