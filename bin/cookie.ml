let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

let is_ftext = function
  | '\033' .. '\057' | '\059' .. '\126' -> true
  | _ -> false

let is_wsp = function ' ' | '\t' -> true | _ -> false

let parser =
  let open Angstrom in
  take_while1 is_ftext >>= fun _field_name ->
  let buf = Bytes.create 0x7ff in
  skip_while is_wsp *> char ':' *> Unstrctrd_parser.unstrctrd buf
  >>= fun value -> return value

let parser =
  let open Angstrom in
  many parser

let of_filename filename =
  let parser = Angstrom.Unbuffered.parse parser in
  let ic = open_in filename in
  let finally () = close_in ic in
  Fun.protect ~finally @@ fun () ->
  let bstr = Bstr.create 0x7ff in
  let rec go len = function
    | Angstrom.Unbuffered.Done (_, lst) ->
        let lst = List.map Unstrctrd.without_comments lst in
        let lst = List.map Result.get_ok lst in
        let lst = List.map Unstrctrd.fold_fws lst in
        let lst = List.map Unstrctrd.to_utf_8_string lst in
        let lst = List.map String.trim lst in
        Ok lst
    | Fail _ -> error_msgf "Invalid Cookie file"
    | Partial { committed= src_off; continue } -> (
        Bstr.blit bstr ~src_off bstr ~dst_off:0 ~len:(len - src_off);
        let rem = len - src_off in
        match input_line ic with
        | exception End_of_file -> go 0 (continue bstr ~off:0 ~len:rem Complete)
        | line ->
            let line = line ^ "\r\n" in
            let bstr =
              if String.length line > Bstr.length bstr - len then
                Bstr.extend bstr 0 (String.length line)
              else bstr
            in
            Bstr.blit_from_string line ~src_off:0 bstr ~dst_off:rem
              ~len:(String.length line);
            let len = len + String.length line in
            go len (continue bstr ~off:0 ~len Incomplete))
  in
  go 0 parser

let to_filename headers =
  let f name value acc =
    let unstrctrd = Unstrctrd.of_string (value ^ "\r\n\r\n") in
    match (String.lowercase_ascii name, unstrctrd) with
    | "set-cookie", Ok (_, value) -> value :: acc
    | _ -> acc
  in
  Httpcats.Headers.fold ~f ~init:[] headers

let pp =
  let open Fmt in
  Dump.iter List.iter (any "cookie") string

let to_headers =
  let fn value =
    (* NOTE(dinosaure): delete attributes. *)
    match String.split_on_char ';' value with
    | [ value ] | value :: _ -> ("Cookie", Fmt.str "%s" value)
    | [] -> assert false
  in
  List.map fn

let setup = function
  | None -> `Ok []
  | Some filename -> (
      match of_filename (Fpath.to_string filename) with
      | Ok cookie ->
          Logs.debug (fun m -> m "Cookie: @[<hov>%a@]" pp cookie);
          `Ok cookie
      | Error _ ->
          `Error (true, Fmt.str "Invalid Cookie file %a" Fpath.pp filename))
