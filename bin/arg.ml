let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let error_clif ?(man = false) fmt = Fmt.kstr (fun msg -> `Error (man, msg)) fmt

open Cmdliner

let docs_output = "OUTPUT"
let docs_tls = "TRANSPORT LAYER SECURITY"
let docs_hexdump = "HEX OUTPUT"

let verbosity =
  let env = Cmd.Env.info "HURL_LOGS" in
  Logs_cli.level ~env ~docs:docs_output ()

let renderer =
  let env = Cmd.Env.info "HURL_FMT" in
  Fmt_cli.style_renderer ~env ~docs:docs_output ()

let utf_8 =
  let doc = "Allow us to emit UTF-8 characters." in
  let env = Cmd.Env.info "HURL_UTF_8" in
  let open Arg in
  value
  & opt bool true
  & info [ "with-utf-8" ] ~doc ~docv:"BOOL" ~docs:docs_output ~env

let app_style = `Cyan
let err_style = `Red
let warn_style = `Yellow
let info_style = `Blue
let debug_style = `Green

let pp_header ~pp_h ppf (l, h) =
  match l with
  | Logs.Error ->
      let h = Option.value ~default:"ERROR" h in
      pp_h ppf err_style h
  | Logs.Warning ->
      let h = Option.value ~default:"WARN" h in
      pp_h ppf warn_style h
  | Logs.Info ->
      let h = Option.value ~default:"INFO" h in
      pp_h ppf info_style h
  | Logs.Debug ->
      let h = Option.value ~default:"DEBUG" h in
      pp_h ppf debug_style h
  | Logs.App ->
      Fun.flip Option.iter h @@ fun h ->
      Fmt.pf ppf "[%a] " Fmt.(styled app_style (fmt "%10s")) h

let pp_header =
  let pp_h ppf style h = Fmt.pf ppf "[%a]" Fmt.(styled style (fmt "%10s")) h in
  pp_header ~pp_h

let reporter ppf =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let with_metadata header _tags k ppf fmt =
      Fmt.kpf k ppf
        ("[%02d]%a[%a]: @[<hov>" ^^ fmt ^^ "@]\n%!")
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
  let stdout =
    Format.make_formatter (output_substring stdout) (fun () -> flush stdout)
  in
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
  Logs.set_reporter reporter;
  Logs.set_level level;
  (Option.is_none level, stdout)

let setup_logs = Term.(const setup_logs $ utf_8 $ renderer $ verbosity)
let () = Logs_threaded.enable ()

let output =
  let doc = "The destination to save the response content." in
  let parser str =
    match Fpath.of_string str with
    | Ok v when Sys.file_exists str -> error_msgf "%a already exist" Fpath.pp v
    | Ok _ as v -> v
    | Error _ as err -> err
  in
  let open Arg in
  let output = Arg.conv (parser, Fpath.pp) in
  value
  & opt (some output) None
  & info [ "o"; "output" ] ~doc ~docv:"FILE" ~docs:docs_output

let cols =
  let doc = "Format $(i,COLS) octets per line. Default 16. Max 256." in
  let parser str =
    match int_of_string str with
    | n when n < 1 || n > 256 ->
        error_msgf "Invalid COLS value (must <= 256 && > 0): %d" n
    | n -> Ok n
    | exception _ -> error_msgf "Invalid COLS value: %S" str
  in
  let open Arg in
  let cols = conv (parser, Fmt.int) in
  value
  & opt (some cols) None
  & info [ "c"; "cols" ] ~doc ~docv:"COLS" ~docs:docs_hexdump

let groupsize =
  let doc =
    "Separate the output of every $(i,bytes) bytes (two hex characters) by a \
     whitespace. Specify -g 0 to supress grouping. $(i,bytes) defaults to 2."
  in
  let open Arg in
  value
  & opt (some int) None
  & info [ "g"; "groupsize" ] ~doc ~docv:"BYTES" ~docs:docs_hexdump

let len =
  let doc = "Stop after writing $(i,LEN) octets." in
  let open Arg in
  value
  & opt (some int) None
  & info [ "l"; "len" ] ~doc ~docv:"LEN" ~docs:docs_hexdump

let uppercase =
  let doc = "Use upper case hex letters. Default is lower case." in
  let open Arg in
  value & flag & info [ "u" ] ~doc ~docs:docs_hexdump

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
  let open Arg in
  value
  & opt (some authenticator) None
  & info
      [ "a"; "auth"; "authenticator" ]
      ~doc ~docs:docs_tls ~docv:"AUTHENTICATOR"

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
  let open Arg in
  let tls_version = Arg.conv (parser, pp) in
  value
  & opt (some tls_version) None
  & info [ "tls-version" ] ~doc ~docv:"TLS-VERSION" ~docs:docs_tls

let http_version =
  let doc =
    "Enforce a specific HTTP version (HTTP/1.1 or H2) for the given request. \
     By default, ALPN negotiation prioritises $(b,h2) and proposes \
     $(b,http/1.1). The user can force the use of one of the two protocols \
     during the TLS handshake if TLS is used. If TLS is not used, only \
     $(b,http/1.1) is used. If the protocol requested by the user is not \
     available, the protocol proposed by the server is used."
  in
  let parser str =
    match String.lowercase_ascii str with
    | "http/1.1" | "h1" -> Ok `HTTP_1_1
    | "h2" -> Ok `H2
    | _ -> error_msgf "Invalid HTTP version: %S" str
  in
  let pp ppf = function
    | `HTTP_1_1 -> Fmt.string ppf "http/1.1"
    | `H2 -> Fmt.string ppf "h2"
  in
  let open Arg in
  let http_version = conv (parser, pp) in
  value
  & opt (some http_version) None
  & info [ "http-version" ] ~doc ~docv:"PROTOCOL" ~docs:docs_tls

let already_alerted = ref false

let possibly_malformed_path =
  "the url path contains characters that have just been escaped. If you are \
   trying to specify parameters in the url, you should do so via the \
   command-line (rather than directly in the url)."

let uri =
  let pp_port ppf = function
    | Some port -> Fmt.pf ppf ":%d" port
    | None -> ()
  in
  let pp_user_pass ppf = function
    | Some (user, Some pass) -> Fmt.pf ppf "%s:%s@" user pass
    | Some (user, None) -> Fmt.pf ppf "%s@" user
    | None -> ()
  in
  let doc = "The request URL." in
  let parser str =
    match Httpcats.decode_uri str with
    | Ok (_tls, scheme, user_pass, host, port, path) ->
        let path' = Pct.path path in
        if path <> path' && not !already_alerted then begin
          Logs.warn (fun m -> m "%s" possibly_malformed_path);
          already_alerted := true
        end;
        let uri =
          Fmt.str "%s://%a%s%a%s" scheme pp_user_pass user_pass host pp_port
            port path'
        in
        Ok uri
    | Error _ as err -> err
  in
  let uri = Arg.conv (parser, Fmt.string) in
  Arg.(required & pos 0 (some uri) None & info [] ~doc ~docv:"URL")

let now () = Some (Ptime_clock.now ())

let setup_tls uri authenticator version http_version =
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
        Logs.debug (fun m -> m "Load certificates from the system");
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
  let alpn_protocols =
    match http_version with
    | Some `HTTP_1_1 -> [ "http/1.1" ]
    | Some `H2 -> [ "h2" ]
    | None -> [ "h2"; "http/1.1" ]
  in
  let domain_name =
    let ( let* ) = Result.bind in
    let* _, _, _, host, _, _ = Httpcats.decode_uri uri in
    let* domain_name = Domain_name.of_string host in
    Domain_name.host domain_name
  in
  match
    (domain_name, Tls.Config.client ~authenticator ~alpn_protocols ?version ())
  with
  | Ok host, Ok tls_config ->
      Logs.debug (fun m ->
          m "Add a peer to our TLS configuration: %a" Domain_name.pp host);
      (Some (Tls.Config.peer tls_config host), http_version)
  | _, Ok tls_config -> (Some tls_config, http_version)
  | _, Error (`Msg msg) ->
      Logs.warn (fun m -> m "Impossible to build a TLS configuration: %s" msg);
      (None, http_version)

let setup_tls =
  Term.(const setup_tls $ uri $ authenticator $ tls_version $ http_version)

let docs_dns = "DOMAIN NAME SERVICE"

let timeout =
  let is_digit = function '0' .. '9' -> true | _ -> false in
  let parser str =
    let len =
      let len = ref 0 in
      while !len < String.length str && is_digit str.[!len] do
        incr len
      done;
      !len
    in
    let meter = String.sub str len (String.length str - len) in
    let value = String.sub str 0 len in
    match meter with
    | "ns" -> Ok (Int64.of_string value)
    | "us" -> Ok (Duration.of_us (int_of_string value))
    | "ms" -> Ok (Duration.of_ms (int_of_string value))
    | "sec" | "s" -> Ok (Duration.of_sec (int_of_string value))
    | "min" | "m" -> Ok (Duration.of_min (int_of_string value))
    | "hour" | "h" -> Ok (Duration.of_hour (int_of_string value))
    | _ -> error_msgf "Invalid time: %S" str
  in
  Arg.conv ~docv:"TIME" (parser, Duration.pp)

let aaaa_timeout =
  let doc = "The timeout applied to the IPv6 resolution." in
  let open Arg in
  value
  & opt (some timeout) None
  & info [ "aaaa-timeout" ] ~doc ~docv:"TIME" ~docs:docs_dns

let connect_delay =
  let doc =
    "Time to repeat another connection attempt if the others don't respond."
  in
  let open Arg in
  value
  & opt (some timeout) None
  & info [ "connect-delay" ] ~doc ~docv:"TIME" ~docs:docs_dns

let connect_timeout =
  let doc = "The timeout applied to $(b,connect())." in
  let open Arg in
  value
  & opt (some timeout) None
  & info [ "connect-timeout" ] ~doc ~docv:"TIME" ~docs:docs_dns

let resolve_timeout =
  let doc = "The timeout applied to the domain-name resolution." in
  let open Arg in
  value
  & opt (some timeout) None
  & info [ "resolve-timeout" ] ~doc ~docv:"TIME" ~docs:docs_dns

let resolve_retries =
  let doc = "The number $(i,N) of attempts to make a connection." in
  let open Arg in
  value
  & opt (some int) None
  & info [ "resolve-retries" ] ~doc ~docv:"N" ~docs:docs_dns

type happy_eyeballs = {
    aaaa_timeout: int64 option
  ; connect_delay: int64 option
  ; connect_timeout: int64 option
  ; resolve_timeout: int64 option
  ; resolve_retries: int option
}

let setup_happy_eyeballs aaaa_timeout connect_delay connect_timeout
    resolve_timeout resolve_retries = function
  | false ->
      Some
        {
          aaaa_timeout
        ; connect_delay
        ; connect_timeout
        ; resolve_timeout
        ; resolve_retries
        }
  | _ -> None

let without_happy_eyeballs =
  let doc = "Don't use the happy-eyeballs algorithm (RFC8305)." in
  let open Arg in
  value & flag & info [ "without-happy-eyeballs" ] ~doc ~docs:docs_dns

let setup_happy_eyeballs =
  Term.(
    const setup_happy_eyeballs
    $ aaaa_timeout
    $ connect_delay
    $ connect_timeout
    $ resolve_timeout
    $ resolve_retries
    $ without_happy_eyeballs)

let nameserver_of_string str =
  let ( let* ) = Result.bind in
  match String.split_on_char ':' str with
  | "tls" :: rest -> (
      let str = String.concat ":" rest in
      match String.split_on_char '!' str with
      | [ nameserver ] ->
          let* ipaddr, port =
            Ipaddr.with_port_of_string ~default:853 nameserver
          in
          let* authenticator = Ca_certs.authenticator () in
          let* tls = Tls.Config.client ~authenticator () in
          Ok (`Tcp, `Tls (tls, ipaddr, port))
      | nameserver :: authenticator ->
          let* ipaddr, port =
            Ipaddr.with_port_of_string ~default:853 nameserver
          in
          let authenticator = String.concat "!" authenticator in
          let* authenticator = X509.Authenticator.of_string authenticator in
          let time () = Some (Ptime.v (Ptime_clock.now_d_ps ())) in
          let authenticator = authenticator time in
          let* tls = Tls.Config.client ~authenticator () in
          Ok (`Tcp, `Tls (tls, ipaddr, port))
      | [] -> assert false)
  | "tcp" :: nameserver ->
      let str = String.concat ":" nameserver in
      let* ipaddr, port = Ipaddr.with_port_of_string ~default:53 str in
      Ok (`Tcp, `Plaintext (ipaddr, port))
  | "udp" :: nameserver | nameserver ->
      let str = String.concat ":" nameserver in
      let* ipaddr, port = Ipaddr.with_port_of_string ~default:53 str in
      Ok (`Udp, `Plaintext (ipaddr, port))

type nameserver =
  [ `Plaintext of Ipaddr.t * int | `Tls of Tls.Config.client * Ipaddr.t * int ]

let nameserver =
  let parser = nameserver_of_string in
  let pp ppf = function
    | _, `Tls (_, ipaddr, 853) ->
        Fmt.pf ppf "tls:%a!<authenticator>" Ipaddr.pp ipaddr
    | _, `Tls (_, ipaddr, port) ->
        Fmt.pf ppf "tls:%a:%d!<authenticator>" Ipaddr.pp ipaddr port
    | `Tcp, `Plaintext (ipaddr, 53) -> Fmt.pf ppf "tcp:%a" Ipaddr.pp ipaddr
    | `Udp, `Plaintext (ipaddr, 53) -> Fmt.pf ppf "%a" Ipaddr.pp ipaddr
    | `Tcp, `Plaintext (ipaddr, port) ->
        Fmt.pf ppf "tcp:%a:%d" Ipaddr.pp ipaddr port
    | `Udp, `Plaintext (ipaddr, port) ->
        Fmt.pf ppf "%a:%d" Ipaddr.pp ipaddr port
  in
  Arg.conv (parser, pp) ~docv:"NAMESERVER"

let nameservers =
  let doc = "The $(i,NAMESERVER) used to resolve domain-names." in
  let google_com = (`Udp, `Plaintext (Ipaddr.of_string_exn "8.8.8.8", 53)) in
  let open Arg in
  value
  & opt_all nameserver [ google_com ]
  & info [ "n"; "nameserver" ] ~doc ~docv:"NAMESERVER" ~docs:docs_dns

let setup_nameservers nameservers =
  let tcp, udp =
    List.partition_map
      begin
        function `Udp, v -> Either.Right v | `Tcp, v -> Either.Left v
      end
      nameservers
  in
  match (tcp, udp) with
  | [], nameservers -> `Ok (`Udp, nameservers)
  | nameservers, [] -> `Ok (`Tcp, nameservers)
  | _ -> `Error (true, "Impossible to use TCP & UDP protocols for nameservers")

let setup_nameservers = Term.(ret (const setup_nameservers $ nameservers))

let dns =
  let open Arg in
  let system =
    let doc =
      "Domain name resolution is done by the system (usually 127.0.0.1:53)."
    in
    info [ "system" ] ~doc ~docs:docs_dns
  in
  let ocaml =
    let doc =
      "Domain name resolution is handled by our OCaml implementation (see \
       $(b,ocaml-dns))."
    in
    info [ "ocaml" ] ~doc ~docs:docs_dns
  in
  value & vflag `System [ (`System, system); (`OCaml, ocaml) ]

let follow_redirect =
  let doc =
    "If the server reports that the requested page has moved to a different \
     location (indicated with $(b,Location:) header and a $(b,3XX) response \
     code), this option makes $(tname) redo the request on the new place."
  in
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

let max_redirect =
  let doc = "The number of allowed redirection (when you use $(b,-L))." in
  Arg.(value & opt int 15 & info [ "redirect" ] ~doc)

type request_item =
  | Header of string * string
  | Url_parameter of string * string
  | Json of string * Json.t
  | String of string * string
  | Json_from_location of string * Fpath.t
  | String_from_location of string * Fpath.t
  | Part of string * Fpath.t

let not_header_and_query = function
  | Header _ | Url_parameter _ -> false
  | _ -> true

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

let fields_filter =
  let is_valid = function
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '0' .. '9'
    | '!' | '"' | '#' | '$' | '%' | '&' | '\'' | '(' | ')' | '*' | '+' | ','
    | '-' | '.' | '/' | ';' | '<' | '=' | '>' | '?' | '@' | '[' | '\\' | ']'
    | '^' | '_' | '`' | '{' | '|' | '}' | '~' ->
        true
    | _ -> false
  in
  let parser str =
    if str = "" then error_msgf "Empty field filter"
    else
      match str.[0] with
      | '-' when String.(for_all is_valid (sub str 1 (length str - 1))) ->
          Ok (`Exclude, String.sub str 1 (String.length str - 1))
      | '+' when String.(for_all is_valid (sub str 1 (length str - 1))) ->
          Ok (`Include, String.sub str 1 (String.length str - 1))
      | _ when String.for_all is_valid str -> Ok (`Include, str)
      | _ -> error_msgf "Invalid field filter: %S" str
  in
  let pp ppf = function
    | `Exclude, str -> Fmt.pf ppf "-%s" str
    | `Include, str -> Fmt.pf ppf "+%s" str
  in
  let filter = Arg.conv (parser, pp) in
  let doc = "Filtering displayed field" in
  Arg.(value & opt_all filter [] & info [ "field-filter" ] ~doc)

let setup_fields_filter fields =
  let incl, excl =
    List.partition_map
      (function
        | `Include, field -> Either.Left field
        | `Exclude, field -> Either.Right field)
      fields
  in
  let module Set = Set.Make (String) in
  let incl = Set.of_list incl and excl = Set.of_list excl in
  Set.(to_list (diff excl incl))

let setup_fields_filter = Term.(const setup_fields_filter $ fields_filter)

type printer =
  [ `DNS
  | `IP
  | `TLS
  | `Request
  | `Response
  | `Headers_request
  | `Headers_response
  | `Body_request
  | `Body_response ]

let printers =
  let doc = "String specifying what the output should contain." in
  let parser str =
    let show = [] in
    let show = if String.contains str 'd' then `DNS :: show else show in
    let show = if String.contains str 'i' then `IP :: show else show in
    let show = if String.contains str 's' then `TLS :: show else show in
    let show = if String.contains str 'r' then `Response :: show else show in
    let show = if String.contains str 'R' then `Request :: show else show in
    let show =
      if String.contains str 'H' then `Headers_request :: show else show
    in
    let show =
      if String.contains str 'h' then `Headers_response :: show else show
    in
    let show =
      if String.contains str 'B' then `Body_request :: show else show
    in
    let show =
      if String.contains str 'b' then `Body_response :: show else show
    in
    Ok show
  in
  let pp ppf lst =
    let pp_elt ppf = function
      | `DNS -> Fmt.string ppf "d"
      | `IP -> Fmt.string ppf "i"
      | `TLS -> Fmt.string ppf "s"
      | `Response -> Fmt.string ppf "r"
      | `Request -> Fmt.string ppf "R"
      | `Headers_request -> Fmt.string ppf "H"
      | `Headers_response -> Fmt.string ppf "h"
      | `Body_request -> Fmt.string ppf "B"
      | `Body_response -> Fmt.string ppf "b"
    in
    Fmt.pf ppf "%a" Fmt.(list ~sep:nop pp_elt) lst
  in
  let printers = Arg.conv (parser, pp) in
  Arg.(
    value
    & opt printers [ `Response; `Headers_response; `Body_response ]
    & info [ "p"; "printers" ] ~doc)

let format_of_output =
  let open Arg in
  let hex =
    let doc = "Displaying the HTTP response in the hexdump format." in
    info [ "hex" ] ~doc
  in
  let json =
    let doc =
      "Displaying the HTTP response in the JSON format (if the Content-Type is \
       application/json, otherwise, we use the hexdump format)."
    in
    info [ "j"; "json-output" ] ~doc
  in
  let raw =
    let doc =
      "Displaying the HTTP response as is (if the Content-Type is recognized \
       as a text)."
    in
    info [ "raw" ] ~doc
  in
  value & vflag `None [ (`Hex, hex); (`Json, json); (`Raw, raw) ]

let format_of_input =
  let open Arg in
  let form =
    let doc =
      "Data items from the coommand line are serialized as form fields. The \
       Content-Type is set to application/x-www-form-urlencoded (if not \
       specified). The presence of any file fields results in a \
       multipart/form-data request."
    in
    info [ "f"; "form" ] ~doc
  in
  let multipart =
    let doc =
      "Similar to $(b,--form), but always sends a multipart/form-data request \
       (i.e., even without files)."
    in
    info [ "multipart" ] ~doc
  in
  let json =
    let doc =
      "Data items from the command line are serialized as a JSON object. The \
       Content-Type and Accept headers are set to application/json (if not \
       specified)."
    in
    info [ "json-input" ] ~doc
  in
  value & vflag `Json [ (`Form, form); (`Multipart, multipart); (`Json, json) ]

type request = {
    headers: (string * string) list
  ; query: string
  ; body: string Seq.t option
}

let headers_of_request_items =
  Fun.flip List.fold_left [] @@ fun acc -> function
  | Header (k, v) -> (k, v) :: acc | _ -> acc

let query_of_request_items request_items =
  let parameters =
    Fun.flip List.fold_left [] @@ fun acc -> function
    | Url_parameter (k, v) -> (k, [ v ]) :: acc | _ -> acc
  in
  Pct.encode (parameters request_items)

type item =
  | Json of Json.t
  | Json_from_location of Fpath.t
  | String_from_location of Fpath.t

let location_to_string location =
  Miou.Lazy.from_fun @@ fun () ->
  let ic = open_in (Fpath.to_string location) in
  let ln = in_channel_length ic in
  let rs = Bytes.create ln in
  really_input ic rs 0 ln; close_in ic; Bytes.unsafe_to_string rs

let json_of_request_items request_items =
  let to_lexemes = function
    | `Begin -> Seq.return `Os
    | `Item (name, Json value) -> Seq.cons (`Name name) (Json.to_lexemes value)
    | `Item (name, Json_from_location location) ->
        let seq = Json.location_to_lexemes location in
        Seq.cons (`Name name) seq
    | `Item (name, String_from_location location) ->
        let str = Miou.Lazy.force (location_to_string location) in
        List.to_seq [ `Name name; `String str ]
    | `End -> Seq.return `Oe
  in
  let items =
    List.fold_left
      begin
        fun acc -> function
          | Header _ | Url_parameter _ | Part _ -> acc
          | Json (k, v) -> `Item (k, Json v) :: acc
          | String (k, v) -> `Item (k, Json (`String v)) :: acc
          | Json_from_location (k, v) -> `Item (k, Json_from_location v) :: acc
          | String_from_location (k, v) ->
              `Item (k, String_from_location v) :: acc
      end
      [ `End ] (List.rev request_items)
  in
  let items = List.to_seq (`Begin :: items) in
  Seq.(concat (map to_lexemes items))

let stream_of_location ?(size_chunk = 0x800) location =
  let ic = Miou.Lazy.from_fun @@ fun () -> open_in (Fpath.to_string location) in
  let buf = Bytes.create size_chunk in
  fun () ->
    let ic = Miou.Lazy.force ic in
    match input ic buf 0 (Bytes.length buf) with
    | 0 -> close_in ic; None
    | len -> Some (Bytes.unsafe_to_string buf, 0, len)
    | exception End_of_file -> close_in ic; None

let parts_of_request_items =
  let open Multipart_form in
  Fun.flip List.fold_left [] @@ fun acc -> function
  | Header _ | Json _ | Url_parameter _ | Json_from_location _ -> acc
  | Part (name, location) ->
      let stream = stream_of_location location in
      let filename = Fpath.basename location in
      let disposition = Content_disposition.v ~filename name in
      part ~disposition stream :: acc
  | String (name, v) ->
      let stream = Seq.(to_dispenser (return (v, 0, String.length v))) in
      let disposition = Content_disposition.v name in
      part ~disposition stream :: acc
  | String_from_location (name, location) ->
      let stream = stream_of_location location in
      let disposition = Content_disposition.v name in
      part ~disposition stream :: acc

let add_unless_exists headers k v =
  if List.mem_assoc k headers = false then (k, v) :: headers else headers

let rng ?g:_ len =
  let res = Bytes.create len in
  for i = 0 to len - 1 do
    match Random.int (26 + 26 + 10) with
    | n when n < 26 -> Bytes.set res i Char.(chr (code 'a' + n))
    | n when n < 26 + 26 -> Bytes.set res i Char.(chr (code 'A' + n - 26))
    | n -> Bytes.set res i Char.(chr (code '0' + n - 26 - 26))
  done;
  Bytes.unsafe_to_string res

let simple_list_to_multipart_header lst =
  let open Multipart_form in
  let fields =
    Fun.flip List.map lst @@ fun (k, v) ->
    let field_name = Field_name.v k in
    match Unstrctrd.of_string v with
    | Ok (_, unstrctrd) -> Field.Field (field_name, Field.Field, unstrctrd)
    | Error (`Msg _) -> assert false (* TODO *)
  in
  Header.of_list fields

let multipart_header_to_simple_list hdr =
  let open Multipart_form in
  let lst = Header.to_list hdr in
  Fun.flip List.map lst @@ function
  | Field.Field (_, Content_type, v) ->
      ("Content-Type", Content_type.to_string v)
  | Field.Field (_, Content_encoding, v) ->
      ("Content-Transfer-Encoding", Content_encoding.to_string v)
  | Field.Field (_, Content_disposition, v) ->
      ("Content-Disposition", Content_disposition.to_string v)
  | Field.Field (k, Field, v) -> ((k :> string), Unstrctrd.to_utf_8_string v)

let setup_request_items format_of_input boundary minify request_items =
  let a_part = function Part _ -> true | _ -> false in
  let a_complex_json : request_item -> bool = function
    | Json _ | Json_from_location _ -> true
    | _ -> false
  in
  if List.filter not_header_and_query request_items = [] then
    let headers = headers_of_request_items request_items in
    let query = query_of_request_items request_items in
    `Ok { headers; query; body= None }
  else
    match format_of_input with
    | `Json -> begin
        match List.find_opt a_part request_items with
        | Some (Part (key, _)) ->
            error_clif "Invalid file fields (perhaps you meant --form?): %s" key
        | _ ->
            let headers = headers_of_request_items request_items in
            let headers =
              add_unless_exists headers "Content-Type" "application/json"
            in
            let query = query_of_request_items request_items in
            let json = json_of_request_items request_items in
            let seq = Json.lexemes_to_seq_of_bytes ~minify json in
            `Ok { headers; query; body= Some seq }
      end
    | `Multipart | `Form -> begin
        match List.find_opt a_complex_json request_items with
        | Some (Json _) | Some (Json_from_location _) ->
            error_clif
              "Cannot use complex JSON value types with --form/--multipart."
        | _ ->
            let headers = headers_of_request_items request_items in
            let header = simple_list_to_multipart_header headers in
            let query = query_of_request_items request_items in
            let parts = parts_of_request_items request_items in
            let multipart =
              Multipart_form.multipart ~rng ~header ?boundary parts
            in
            let header, stream = Multipart_form.to_stream multipart in
            let seq = Seq.of_dispenser stream in
            let seq =
              Seq.map (fun (str, off, len) -> String.sub str off len) seq
            in
            let headers = multipart_header_to_simple_list header in
            `Ok { headers; query; body= Some seq }
      end

let boundary =
  let doc =
    "Specify a custom boundary string for multipart/form-data requests."
  in
  let valid =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
  in
  let parser str =
    let is_valid = String.contains valid in
    if String.for_all is_valid str then Ok str
    else error_msgf "Invalid boundary: %S" str
  in
  let boundary = Arg.conv (parser, Fmt.string) in
  Arg.(value & opt (some boundary) None & info [ "boundary" ] ~doc)

let minify_input =
  let doc = "Minify the JSON to send." in
  Arg.(value & flag & info [ "minify" ] ~doc)

let setup_request_items =
  let open Term in
  const setup_request_items
  $ format_of_input
  $ boundary
  $ minify_input
  $ request_items
  |> ret

let cookie =
  let doc = "The cookie file (according RFC822)." in
  let parser str =
    match Fpath.of_string str with
    | Ok _ as v when Sys.file_exists str -> v
    | Ok v -> error_msgf "%a does not exist" Fpath.pp v
    | Error _ as err -> err
  in
  let open Arg in
  let cookie = Arg.conv (parser, Fpath.pp) in
  value
  & opt (some cookie) None
  & info [ "cookie" ] ~doc ~docv:"FILE" ~docs:docs_output

let setup_cookies =
  let open Term in
  ret (const Cookie.setup $ cookie)
