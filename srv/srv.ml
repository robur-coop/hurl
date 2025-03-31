module Uri = struct
  let int_of_hex_char chr =
    let chr = int_of_char (Char.uppercase_ascii chr) - 48 in
    if chr > 9 then
      if chr > 16 && chr < 23 then chr - 7 else failwith "Uri.int_of_hex_char"
    else if chr >= 0 then chr
    else failwith "Uri.int_of_hex_char"

  let pct_decode str =
    let len = String.length str in
    let buf = Buffer.create len in
    let rec scan start cur =
      if cur >= len then Buffer.add_substring buf str start (cur - start)
      else if str.[cur] = '%' then begin
        Buffer.add_substring buf str start (cur - start);
        let cur = cur + 1 in
        if cur >= len then Buffer.add_char buf '%'
        else
          match int_of_hex_char str.[cur] with
          | exception _ -> Buffer.add_char buf '%'; scan cur cur
          | high ->
              let cur = cur + 1 in
              if cur >= len then begin
                Buffer.add_char buf '%';
                Buffer.add_char buf str.[cur - 1]
              end
              else begin
                let start_at =
                  match int_of_hex_char str.[cur] with
                  | low ->
                      Buffer.add_char buf (Char.chr ((high lsl 4) + low));
                      cur + 1
                  | exception _ ->
                      Buffer.add_char buf '%';
                      Buffer.add_char buf str.[cur - 1];
                      cur
                in
                scan start_at start_at
              end
      end
      else scan start (cur + 1)
    in
    scan 0 0; Buffer.contents buf

  let plus_to_space str =
    let buf = String.to_bytes str in
    for i = 0 to Bytes.length buf - 1 do
      if Bytes.get buf i = '+' then Bytes.set buf i ' '
    done;
    Bytes.unsafe_to_string buf

  let query_decode str =
    let lst = String.split_on_char '&' str in
    let split str =
      match String.index_opt str '=' with
      | None -> [ str ]
      | Some pos ->
          let k = String.sub str 0 pos in
          let v = String.sub str (pos + 1) (String.length str - pos - 1) in
          [ k; v ]
    in
    let rec go acc = function
      | (k :: v :: _) :: lst ->
          let k = plus_to_space k in
          let v = String.split_on_char ',' (plus_to_space v) in
          go ((k, v) :: acc) lst
      | [ k ] :: lst ->
          let k = plus_to_space k in
          go ((k, []) :: acc) lst
      | [] :: _ -> assert false
      | [] -> acc
    in
    let lst = List.rev_map split lst in
    go [] lst

  let path_decode str =
    match String.index_opt str '?' with
    | Some pos -> String.sub str 0 pos
    | None -> (
        match String.index_opt str '#' with
        | Some pos -> String.sub str 0 pos
        | None -> str)

  let decode str =
    match String.index_opt str '?' with
    | None -> None
    | Some pos ->
        let query = String.sub str (pos + 1) (String.length str - pos - 1) in
        let query = query_decode query in
        let query =
          List.map (fun (k, v) -> (pct_decode k, List.map pct_decode v)) query
        in
        Some query
end

module Json = struct
  type value = [ `Null | `Bool of bool | `String of string | `Float of float ]
  type t = [ value | `A of t list | `O of (string * t) list ]

  let _max_young_size = 0x7ff

  module Stack = struct
    type stack =
      | In_array of t list * stack
      | In_object of (string * t) list * stack
      | Empty
  end

  let encode ?minify ?(size_chunk = _max_young_size) ~output t =
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

  let to_string ?minify t =
    let buf = Buffer.create 0x7ff in
    let output str = Buffer.add_string buf str in
    encode ?minify ~output t; Buffer.contents buf

  let append_field ~field value = function
    | `O fields -> `O ((field, value) :: fields)
    | _ -> failwith "Json.append_field"
end

module Gzip = struct
  let now () = Int32.of_float (Unix.gettimeofday ())

  let of_string_to_string ?level str =
    let w = De.Lz77.make_window ~bits:15 in
    let q = De.Queue.create 0x100 in
    let ic = De.bigstring_create 0x7ff in
    let oc = De.bigstring_create 0x7ff in
    let pos = ref 0 in
    let buf = Buffer.create 0x7ff in
    let cfg = Gz.Higher.configuration Gz.Unix now in
    let refill bs =
      let len = min (De.bigstring_length bs) (String.length str - !pos) in
      Bigstringaf.blit_from_string str ~src_off:!pos bs ~dst_off:0 ~len;
      pos := !pos + len;
      len
    in
    let flush bs len =
      let str = Bigstringaf.substring bs ~off:0 ~len in
      Buffer.add_string buf str
    in
    Gz.Higher.compress ?level ~w ~q ~refill ~flush () cfg ic oc;
    Buffer.contents buf
end

module Zlib = struct
  let of_string_to_string ?level str =
    let w = De.Lz77.make_window ~bits:15 in
    let q = De.Queue.create 0x100 in
    let ic = De.bigstring_create 0x7ff in
    let oc = De.bigstring_create 0x7ff in
    let pos = ref 0 in
    let buf = Buffer.create 0x7ff in
    let refill bs =
      let len = min (De.bigstring_length bs) (String.length str - !pos) in
      Bigstringaf.blit_from_string str ~src_off:!pos bs ~dst_off:0 ~len;
      pos := !pos + len;
      len
    in
    let flush bs len =
      let str = Bigstringaf.substring bs ~off:0 ~len in
      Buffer.add_string buf str
    in
    Zl.Higher.compress ?level ~w ~q ~refill ~flush ic oc;
    Buffer.contents buf
end

let pp_sockaddr ppf = function
  | Unix.ADDR_INET (inet_addr, port) ->
      Fmt.pf ppf "%s:%d" (Unix.string_of_inet_addr inet_addr) port
  | Unix.ADDR_UNIX str -> Fmt.string ppf str

let pp_origin ppf = function
  | Unix.ADDR_INET (inet_addr, _) ->
      Fmt.string ppf (Unix.string_of_inet_addr inet_addr)
  | Unix.ADDR_UNIX str -> Fmt.string ppf str

let json_of_meta ~host:default_host meta request =
  let headers = H1.Headers.to_list request.H1.Request.headers in
  let headers = `O (List.map (fun (k, v) -> (k, `String v)) headers) in
  let origin, scheme =
    match meta with
    | `Tcp socket ->
        let socket = Miou_unix.to_file_descr socket in
        let sockaddr = Unix.getpeername socket in
        (`String (Fmt.to_to_string pp_origin sockaddr), "http")
    | `Tls socket ->
        let socket = Tls_miou_unix.file_descr socket in
        let socket = Miou_unix.to_file_descr socket in
        let sockaddr = Unix.getpeername socket in
        (`String (Fmt.to_to_string pp_origin sockaddr), "https")
  in
  let host = H1.Headers.get request.H1.Request.headers "Host" in
  let host = Option.value ~default:default_host host in
  let url =
    `String (Fmt.str "%s://%s%s" scheme host request.H1.Request.target)
  in
  let args =
    match Uri.decode request.H1.Request.target with
    | None -> `O []
    | Some queries ->
        let queries =
          List.map (fun (k, v) -> (k, `String (String.concat "," v))) queries
        in
        `O queries
  in
  `O [ ("args", args); ("headers", headers); ("origin", origin); ("url", url) ]

let headers_http_1_1 ?(content_type = "application/json") ?content_length () =
  let hdrs =
    H1.Headers.of_list
      [
        ("Content-Type", content_type); ("Server", "bin/%%VERSION%%")
      ; ("Access-Control-Allow-Origin", "*")
      ; ("Access-Control-Allow-Credentials", "true")
      ]
  in
  Option.fold ~none:hdrs
    ~some:(fun content_length ->
      let value = string_of_int content_length in
      let hdrs = H1.Headers.add hdrs "Content-Length" value in
      H1.Headers.add hdrs "Connection" "close")
    content_length

let handler_http_1_1 ~host meta reqd =
  let request = H1.Reqd.request reqd in
  match
    (request.H1.Request.meth, Uri.path_decode request.H1.Request.target)
  with
  | `GET, "/get" ->
      let json = json_of_meta ~host meta request in
      let body = Json.to_string json in
      let content_length = String.length body in
      let headers = headers_http_1_1 ~content_length () in
      let response = H1.Response.create ~headers `OK in
      H1.Reqd.respond_with_string reqd response body
  | `GET, "/gzip" ->
      let json = json_of_meta ~host meta request in
      let json = Json.append_field ~field:"gzipped" (`Bool true) json in
      let body = Json.to_string json in
      let body = Gzip.of_string_to_string body in
      let content_encoding = "gzip" in
      let content_length = String.length body in
      let headers = headers_http_1_1 ~content_length () in
      let headers =
        H1.Headers.add headers "Content-Encoding" content_encoding
      in
      let response = H1.Response.create ~headers `OK in
      H1.Reqd.respond_with_string reqd response body
  | `GET, "/deflate" ->
      let json = json_of_meta ~host meta request in
      let json = Json.append_field ~field:"deflated" (`Bool true) json in
      let body = Json.to_string json in
      let body = Zlib.of_string_to_string body in
      let content_encoding = "deflate" in
      let content_length = String.length body in
      let headers = headers_http_1_1 ~content_length () in
      let headers =
        H1.Headers.add headers "Content-Encoding" content_encoding
      in
      let response = H1.Response.create ~headers `OK in
      H1.Reqd.respond_with_string reqd response body
  | `GET, "/robot.txt" ->
      let body = {text|User-Agent: *
Disallow: /deny
|text} in
      let content_length = String.length body in
      let content_type = "text/plain" in
      let headers = headers_http_1_1 ~content_type ~content_length () in
      let response = H1.Response.create ~headers `OK in
      H1.Reqd.respond_with_string reqd response body
  | _ ->
      let body =
        {html|<!doctype html><title>404 Not found</title><h1>Not found</h1>|html}
      in
      let content_length = String.length body in
      let headers =
        headers_http_1_1 ~content_type:"text/html" ~content_length ()
      in
      let response = H1.Response.create ~headers `Not_found in
      H1.Reqd.respond_with_string reqd response body

let handler ~host meta = function
  | `V1 reqd -> handler_http_1_1 ~host meta reqd
  | `V2 _reqd -> (* handler_h2 ~host reqd *) assert false

let register_pid = function
  | None -> ()
  | Some filename ->
      let pid = Unix.getpid () in
      let oc = open_out (Fpath.to_string filename) in
      let unlink () = Unix.unlink (Fpath.to_string filename) in
      at_exit unlink;
      let finally () = close_out oc in
      Fun.protect ~finally @@ fun () -> output_string oc (string_of_int pid)

let run _quiet on pid =
  Miou_unix.run @@ fun () ->
  let stop = Httpcats.Server.stop () in
  let switch _sigint = Httpcats.Server.switch stop in
  let _ = Miou.sys_signal Sys.sigint (Sys.Signal_handle switch) in
  let sockaddr =
    let inet_addr = Ipaddr_unix.to_inet_addr (fst on) in
    let port = snd on in
    Unix.ADDR_INET (inet_addr, port)
  in
  let handler meta reqd =
    let host = Fmt.to_to_string pp_sockaddr sockaddr in
    handler ~host meta reqd
  in
  let ready = Miou.Computation.create () in
  let prm0 =
    Miou.async @@ fun () ->
    Miou.Computation.await_exn ready;
    register_pid pid
  in
  Httpcats.Server.clear ~stop ~ready ~handler sockaddr;
  Miou.await_exn prm0

open Cmdliner

let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

let on =
  let doc =
    "The address where we mount the HTTP server (including the port)."
  in
  let parser str = Ipaddr.with_port_of_string ~default:8000 str in
  let pp ppf (ipaddr, port) = Fmt.pf ppf "%a:%d" Ipaddr.pp ipaddr port in
  let[@warning "-8"] (Ok default) =
    Ipaddr.with_port_of_string ~default:8000 "127.0.0.1:8000"
  in
  let ipaddr_with_port = Arg.conv (parser, pp) in
  Arg.(value & pos 0 ipaddr_with_port default & info [] ~doc ~docv:"ADDRESS")

let pid =
  let doc = "Register the PID of the program into the given file." in
  let parser str =
    match Fpath.of_string str with
    | Ok v when Sys.file_exists str -> error_msgf "%a already exists" Fpath.pp v
    | Ok _ as v -> v
    | Error _ as err -> err
  in
  let non_existing_file = Arg.conv (parser, Fpath.pp) in
  Arg.(
    value
    & opt (some non_existing_file) None
    & info [ "p"; "pid" ] ~doc ~docv:"FILE")

let docs_output = "OUTPUT"

let verbosity =
  let env = Cmd.Env.info "SRV_LOGS" in
  Logs_cli.level ~env ~docs:docs_output ()

let renderer =
  let env = Cmd.Env.info "SRV_FMT" in
  Fmt_cli.style_renderer ~env ~docs:docs_output ()

let utf_8 =
  let doc = "Allow us to emit UTF-8 characters." in
  let env = Cmd.Env.info "SRV_UTF_8" in
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
  | Logs.Error -> pp_h ppf err_style (Option.value ~default:"ERROR" h)
  | Logs.Warning -> pp_h ppf warn_style (Option.value ~default:"WARN" h)
  | Logs.Info -> pp_h ppf info_style (Option.value ~default:"INFO" h)
  | Logs.Debug -> pp_h ppf debug_style (Option.value ~default:"DEBUG" h)
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

let setup_logs utf_8 style_renderer level =
  Fmt_tty.setup_std_outputs ~utf_8 ?style_renderer ();
  let reporter = reporter Fmt.stderr in
  Logs.set_reporter reporter; Logs.set_level level; Option.is_none level

let setup_logs = Term.(const setup_logs $ utf_8 $ renderer $ verbosity)
let term = Term.(const run $ setup_logs $ on $ pid)

let cmd =
  let doc =
    "A simple server to test multiple ways to communicate with an HTTP service."
  in
  let man = [] in
  Cmd.(v (info "srv" ~doc ~man)) term

let () = Sys.set_signal Sys.sigpipe Sys.Signal_ignore
let () = Logs_threaded.enable ()
let () = exit (Cmd.eval cmd)
