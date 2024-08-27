let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

open Cmdliner

let verbosity =
  let env = Cmd.Env.info "HURL_LOGS" in
  Logs_cli.level ~env ()

let renderer =
  let env = Cmd.Env.info "HURL_FMT" in
  Fmt_cli.style_renderer ~env ()

let utf_8 =
  let doc = "Allow us to emit UTF-8 characters." in
  let env = Cmd.Env.info "HURL_UTF_8" in
  Arg.(value & opt bool true & info [ "with-utf-8" ] ~doc ~env)

let app_style = `Cyan
let err_style = `Red
let warn_style = `Yellow
let info_style = `Blue
let debug_style = `Green

let pp_header ~pp_h ppf (l, h) =
  match l with
  | Logs.Error ->
      pp_h ppf err_style (match h with None -> "ERROR" | Some h -> h)
  | Logs.Warning ->
      pp_h ppf warn_style (match h with None -> "WARN" | Some h -> h)
  | Logs.Info ->
      pp_h ppf info_style (match h with None -> "INFO" | Some h -> h)
  | Logs.Debug ->
      pp_h ppf debug_style (match h with None -> "DEBUG" | Some h -> h)
  | Logs.App -> (
      match h with
      | Some h -> Fmt.pf ppf "[%a] " Fmt.(styled app_style (fmt "%10s")) h
      | None -> ())

let pp_header =
  let pp_h ppf style h = Fmt.pf ppf "[%a]" Fmt.(styled style (fmt "%10s")) h in
  pp_header ~pp_h

let reporter ppf =
  let pid = Unix.getpid () in
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let with_metadata header _tags k ppf fmt =
      Fmt.kpf k ppf
        ("[%a:%04d]%a[%a]: @[<hov>" ^^ fmt ^^ "@]\n%!")
        Fmt.(styled `Cyan (fmt "%6d"))
        pid
        (Stdlib.Domain.self () :> int)
        pp_header (level, header)
        Fmt.(styled `Magenta (fmt "%20s"))
        (Logs.Src.name src)
    in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt
  in
  { Logs.report }

let colorscheme =
  let x = Array.make 256 `None in
  for i = 0 to 31 do
    x.(i) <- `Style (`Fg, `bit24 (0xaf, 0xd7, 0xff))
  done;
  for i = 48 to 57 do
    x.(i) <- `Style (`Fg, `bit24 (0xaf, 0xdf, 0x77))
  done;
  for i = 65 to 90 do
    x.(i) <- `Style (`Fg, `bit24 (0xff, 0xaf, 0x5f))
  done;
  for i = 97 to 122 do
    x.(i) <- `Style (`Fg, `bit24 (0xff, 0xaf, 0xd7))
  done;
  Hxd.colorscheme_of_array x

let setup_logs utf_8 style_renderer level =
  let stdout = Format.make_formatter (output_substring stdout) (fun () -> flush stdout) in
  Fmt_tty.setup_std_outputs ~utf_8 ?style_renderer ();
  let style_renderer =
    match style_renderer with
    | Some `Ansi_tty -> `Ansi
    | Some `None -> `None
    | None ->
        let dumb =
          match Sys.getenv_opt "TERM" with
          | Some "dumb" | Some "" | None -> true
          | _ -> false
        in
        let isatty =
          try Unix.(isatty (descr_of_out_channel Stdlib.stdout))
          with Unix.Unix_error _ -> false
        in
        if (not dumb) && isatty then `Ansi else `None
  in
  Hxd.Fmt.set_style_renderer stdout style_renderer;
  let reporter = reporter Fmt.stderr in
  Logs.set_reporter reporter; Logs.set_level level;
  (Option.is_none level, stdout)

let setup_logs = Term.(const setup_logs $ utf_8 $ renderer $ verbosity)

let hex =
  let doc = "Displays the response content in hexadecimal format." in
  Arg.(value & flag & info [ "h"; "hex" ] ~doc)

let output =
  let doc = "The destination to save the response content." in
  let parser str = match Fpath.of_string str with
    | Ok v when Sys.file_exists str -> error_msgf "%a already exist" Fpath.pp v
    | Ok _ as v -> v
    | Error _ as err -> err in
  let output = Arg.conv (parser, Fpath.pp) in
  Arg.(value & opt (some output) None & info [ "o"; "output" ] ~doc)

let cols =
  let doc = "Format <cols> octets per line. Default 16. Max 256." in
  let parser str =
    match int_of_string str with
    | n when n < 1 || n > 256 ->
        error_msgf "Invalid <cols> value (must <= 256 && > 0): %d" n
    | n -> Ok n
    | exception _ -> error_msgf "Invalid <cols> value: %S" str
  in
  let cols = Arg.conv (parser, Fmt.int) in
  Arg.(value & opt (some cols) None & info [ "c"; "cols" ] ~doc ~docv:"<cols>")

let groupsize =
  let doc =
    "Separate the output of every <bytes> bytes (two hex characters) by a \
     whitespace. Specify -g 0 to supress grouping. <bytes> defaults to 2."
  in
  Arg.(
    value & opt (some int) None & info [ "g"; "groupsize" ] ~doc ~docv:"<bytes>")

let len =
  let doc = "Stop after writing <len> octets." in
  Arg.(value & opt (some int) None & info [ "l"; "len" ] ~doc ~docv:"<len>")

let uppercase =
  let doc = "Use upper case hex letters. Default is lower case." in
  Arg.(value & flag & info [ "u" ] ~doc)

let setup_hxd cols groupsize len uppercase =
  Hxd.xxd ?cols ?groupsize ?long:len ~uppercase colorscheme

let setup_hxd = Term.(const setup_hxd $ cols $ groupsize $ len $ uppercase)

let authenticator =
  let doc = "The TLS authenticator used to verify TLS certificates." in
  let parser str =
    match X509.Authenticator.of_string str with
    | Ok authenticator -> Ok (authenticator, str)
    | Error _ as err -> err
  in
  let pp ppf (_, str) = Fmt.string ppf str in
  let authenticator = Arg.conv (parser, pp) in
  Arg.(
    value
    & opt (some authenticator) None
    & info [ "a"; "auth"; "authenticator" ] ~doc)

let tls_version =
  let ( let* ) = Result.bind in
  let doc = "Use a specific TLS version (TLS 1.0, 1.1, 1.2 & 1.3)." in
  let version_of_string = function
    | "v1.0" | "1.0" -> Ok `TLS_1_0
    | "v1.1" | "1.1" -> Ok `TLS_1_1
    | "v1.2" | "1.2" -> Ok `TLS_1_2
    | "v1.3" | "1.3" -> Ok `TLS_1_3
    | str -> error_msgf "Invalid TLS version: %S" str
  in
  let version_compare a b =
    match (a, b) with
    | `TLS_1_0, `TLS_1_0 -> 0
    | `TLS_1_0, _ -> -1
    | `TLS_1_1, `TLS_1_1 -> 0
    | `TLS_1_1, `TLS_1_2 -> -1
    | `TLS_1_1, `TLS_1_3 -> -1
    | `TLS_1_1, _ -> 1
    | `TLS_1_2, `TLS_1_2 -> 0
    | `TLS_1_2, `TLS_1_3 -> -1
    | `TLS_1_2, _ -> 1
    | `TLS_1_3, `TLS_1_3 -> 0
    | `TLS_1_3, _ -> 1
  in
  let pp_version ppf = function
    | `TLS_1_0 -> Fmt.string ppf "v1.0"
    | `TLS_1_1 -> Fmt.string ppf "v1.1"
    | `TLS_1_2 -> Fmt.string ppf "v1.2"
    | `TLS_1_3 -> Fmt.string ppf "v1.3"
  in
  let parser str =
    match String.split_on_char ',' str with
    | [ version ] ->
        let* version = version_of_string version in
        Ok (`Only version)
    | [ min; max ] ->
        let* min = version_of_string min in
        let* max = version_of_string max in
        if version_compare min max <= 0 then Ok (`Range (min, max))
        else
          error_msgf "Invalid range of TLS versions: %a >= %a" pp_version min
            pp_version max
    | _ -> error_msgf "Invalid TLS version: %S" str
  in
  let pp ppf = function
    | `Only version -> pp_version ppf version
    | `Range (min, max) -> Fmt.pf ppf "%a,%a" pp_version min pp_version max
  in
  let tls_version = Arg.conv (parser, pp) in
  Arg.(
    value
    & opt (some tls_version) None
    & info [ "tls-version" ] ~doc ~docv:"<tls-version>")

let http_version =
  let doc = "Enforce a specific HTTP version (HTTP/1.1 or H2) for the given request." in
  let parser str = match String.lowercase_ascii str with
    | "http/1.1" | "h1" -> Ok `HTTP_1_1
    | "h2" -> Ok `H2
    | _ -> error_msgf "Invalid HTTP version: %S" str in
  let pp ppf = function
    | `HTTP_1_1 -> Fmt.string ppf "http/1.1"
    | `H2 -> Fmt.string ppf "h2" in
  let http_version = Arg.conv (parser, pp) in
  Arg.(value & opt (some http_version) None & info [ "http-version" ] ~doc)

let now () = Some (Ptime_clock.now ())

let setup_tls authenticator version http_version =
  let version =
    match version with
    | Some (`Only v) -> Some (v, v)
    | Some (`Range (a, b)) -> Some (a, b)
    | None -> None
  in
  let authenticator =
    match authenticator with
    | Some (v, _) -> v now
    | None -> (
        match Ca_certs.authenticator () with
        | Ok v -> v
        | Error (`Msg msg) ->
            Logs.warn (fun m ->
                m
                  "Impossible to load the CA store from the native operating \
                   system: %s"
                  msg);
            fun ?ip:_ ~host:_ _ -> Ok None)
  in
  let alpn_protocols = match http_version with
    | Some `HTTP_1_1 -> [ "http/1.1" ]
    | Some `H2 -> [ "h2" ]
    | None -> [ "h2"; "http/1.1" ] in
  match Tls.Config.client ~authenticator ~alpn_protocols ?version () with
  | Ok tls_config -> Some tls_config, http_version
  | Error (`Msg msg) ->
      Logs.warn (fun m -> m "Impossible to build a TLS configuration: %s" msg);
      None, http_version

let setup_tls = Term.(const setup_tls $ authenticator $ tls_version $ http_version)

let follow_redirect =
  let doc = "If the server reports that the requested page has moved to a different location \
            (indicated with $(b,Location:) header and a $(b,3XX) response code), this option \
            makes $(tname) redo the request on the new place." in
  Arg.(value & flag & info [ "L"; "location"; "follow-redirect" ] ~doc)

let meth =
  let doc =
    "The HTTP method to be used for the request (GET, POST, PUT, DELETE, ...)."
  in
  let parser str =
    match String.lowercase_ascii str with
    | "connect" -> Ok `CONNECT
    | "delete" -> Ok `DELETE
    | "get" -> Ok `GET
    | "head" -> Ok `HEAD
    | "options" -> Ok `OPTIONS
    | "post" -> Ok `POST
    | "put" -> Ok `PUT
    | "trace" -> Ok `TRACE
    | _ -> error_msgf "Invalid method: %S" str
  in
  let pp ppf = function
    | `CONNECT -> Fmt.string ppf "CONNECT"
    | `DELETE -> Fmt.string ppf "DELETE"
    | `GET -> Fmt.string ppf "GET"
    | `HEAD -> Fmt.string ppf "HEAD"
    | `OPTIONS -> Fmt.string ppf "OPTIONS"
    | `POST -> Fmt.string ppf "POST"
    | `PUT -> Fmt.string ppf "PUT"
    | `TRACE -> Fmt.string ppf "TRACE"
    | `Other str -> Fmt.string ppf str
  in
  let meth = Arg.conv (parser, pp) in
  Arg.(
    value
    & opt (some meth) None
    & info [ "m"; "meth"; "method" ] ~doc ~docv:"METHOD")

let uri =
  let doc = "The request URL." in
  let parser str =
    match Httpcats.decode_uri str with Ok _ -> Ok str | Error _ as err -> err
  in
  let uri = Arg.conv (parser, Fmt.string) in
  Arg.(required & pos 0 (some uri) None & info [] ~doc ~docv:"URL")

type request_item =
  | Header of string * string
  | Url_parameter of string * string
  | Json of string * Json.t
  | String of string * string
  | Json_from_location of string * Fpath.t
  | String_from_location of string * Fpath.t
  | Part of string * Fpath.t

let pp_request_item ppf = function
  | Header (k, v) -> Fmt.pf ppf "%s:%s" k v
  | Url_parameter (k, v) -> Fmt.pf ppf "%s==%s" k v
  | Json (k, v) ->
      Fmt.pf ppf "%s:=%a" k (Json.pp_as_hurl_string ~minify:false) v
  | String (k, v) -> Fmt.pf ppf "%s=%S" k v
  | Json_from_location (k, v) -> Fmt.pf ppf "%s:=@%a" k Fpath.pp v
  | String_from_location (k, v) -> Fmt.pf ppf "%s=@%a" k Fpath.pp v
  | Part (k, v) -> Fmt.pf ppf "%s@%a" k Fpath.pp v

let chop ?(len = 1) str =
  if len >= String.length str then
    invalid_arg "Number of characters to discard higher than the given string";
  String.(sub str len (length str - len))

let parser_key str =
  let valid_header_chr = function
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '0' .. '9'
    | '!' | '"' | '#' | '$' | '%' | '&' | '\'' | '(' | ')' | '*' | '+' | ','
    | '-' | '.' | '/' | ';' | '<' | '=' | '>' | '?' | '@' | '[' | '\\' | ']'
    | '^' | '_' | '`' | '{' | '|' | '}' | '~' ->
        true
    | _ -> false
  in
  let valid_key_chr chr = valid_header_chr chr || chr = ':' in
  let valid_to_escape = function
    | ':' | '=' | '@' | '\\' -> true
    | _ -> false
  in
  let idx = ref 0 and stop = ref false and to_escape = ref false in
  let buf = Buffer.create (String.length str) in
  while !idx < String.length str && not !stop do
    begin
      match (str.[!idx], !to_escape) with
      | _, true ->
          Buffer.add_char buf str.[!idx];
          incr idx
      | (':' | '=' | '@'), _ -> stop := true
      | '\\', _ ->
          if valid_to_escape str.[!idx] then to_escape := true
          else Buffer.add_char buf '\\'
      | chr, false ->
          if valid_key_chr chr then Buffer.add_char buf chr
          else Fmt.invalid_arg "Invalid key: %S" str
    end;
    incr idx
  done;
  let key = Buffer.contents buf in
  ( String.for_all valid_header_chr key
  , key
  , String.(sub str (!idx - 1) (length str - !idx + 1)) )

let parse_json_as_hurl_string = function
  | "" -> Ok `Null
  | str when str.[0] = '\'' ->
      let ( let* ) = Result.bind in
      let buf = Buffer.create (String.length str) in
      let lexbuf = Lexing.from_string (chop str) in
      let* str =
        try Ok (Str.hurl_string buf lexbuf)
        with Str.Lexical_error (msg, _, _, _) -> Error (`Msg msg)
      in
      Json.of_string str
  | str -> Json.of_string str

let parse_ocaml_string str =
  if str = "" then Ok str
  else if str.[0] = '"' then
    let buf = Buffer.create (String.length str) in
    let lexbuf = Lexing.from_string str in
    try Ok (Str.ocaml_string buf lexbuf)
    with Str.Lexical_error (msg, _, _, _) -> Error (`Msg msg)
  else Ok str

let parser_request_item str =
  let ( let* ) = Result.bind in
  let* valid_as_header, key, rest =
    try Ok (parser_key str) with Invalid_argument msg -> Error (`Msg msg)
  in
  let for_header = String.starts_with ~prefix:":" rest
  and for_url = String.starts_with ~prefix:"==" rest
  and for_json_string = String.starts_with ~prefix:"=" rest
  and for_json_value = String.starts_with ~prefix:":=" rest
  and for_part = String.starts_with ~prefix:"@" rest in
  if for_url then Ok (Url_parameter (key, chop ~len:2 rest))
  else if for_part then
    let* location = Fpath.of_string (chop rest) in
    if
      Sys.file_exists (Fpath.to_string location)
      && Sys.is_directory (Fpath.to_string location) = false
    then Ok (Part (key, location))
    else error_msgf "%a is not a regular file" Fpath.pp location
  else if for_json_string then
    if String.starts_with ~prefix:"=@" rest then
      let* location = Fpath.of_string (chop ~len:2 rest) in
      if
        Sys.file_exists (Fpath.to_string location)
        && Sys.is_directory (Fpath.to_string location) = false
      then Ok (String_from_location (key, location))
      else error_msgf "%a is not a regular file" Fpath.pp location
    else
      let* value = parse_ocaml_string (chop rest) in
      Ok (String (key, value))
  else if for_json_value then
    if String.starts_with ~prefix:":=@" rest then
      let* location = Fpath.of_string (chop ~len:3 rest) in
      if
        Sys.file_exists (Fpath.to_string location)
        && Sys.is_directory (Fpath.to_string location) = false
      then
        let* () = Json.validate_location location in
        Ok (Json_from_location (key, location))
      else error_msgf "%a is not a regular file" Fpath.pp location
    else
      let* value = parse_json_as_hurl_string (chop ~len:2 rest) in
      Ok (Json (key, value))
  else if for_header then
    if valid_as_header then Ok (Header (key, chop rest))
    else error_msgf "Invalid header field: %S" key
  else error_msgf "Missing value on: %S" str

let request_items =
  let doc = "Optional key-value pairs to be included in the request." in
  let request_item = Arg.conv (parser_request_item, pp_request_item) in
  Arg.(value & pos_right 0 request_item [] & info [] ~doc ~docv:"REQUEST_ITEM")