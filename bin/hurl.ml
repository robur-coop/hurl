let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let ( % ) f g x = f (g x)

let recognize_mime_type resp =
  let with_crlf str = str ^ "\r\n" in
  let parse =
    Result.to_option % Multipart_form.Content_type.of_string % with_crlf
  in
  match
    Option.(join % map parse)
      (Httpcats.Headers.get resp.Httpcats.headers "content-type")
  with
  | Some content_type -> Some (content_type.ty, content_type.subty)
  | None -> None

let content_encoding resp =
  match Httpcats.Headers.get resp.Httpcats.headers "content-encoding" with
  | Some "gzip" -> Some `Gzip
  | Some "deflate" -> Some `Deflate
  | Some _ -> Some `Unknown
  | None -> None

type printer = Printer : (string, 'v) Flow.t * ('v, unit) Sink.t -> printer
type compression = [ `Gzip | `Deflate ]

let guess_how_to_print cfg resp =
  match (recognize_mime_type resp, cfg.Out.format_output) with
  | Some (`Text, _), (`Raw | `None) -> `Text
  | Some (`Application, `Iana_token "json"), (`Json | `None) -> `Json
  | _ -> `Hex

let uncompress = function
  | `Gzip -> Flow.(to_bigstring () % gzip () % to_string)
  | `Deflate -> Flow.(to_bigstring () % deflate () % to_string)

let show = function
  | `DNS -> ()
  | `IP conn -> Printers.print_address conn
  | `TLS tls -> Printers.print_tls tls
  | `Request (req : Httpcats.request) -> Printers.print_request req
  | `Headers_request (req : Httpcats.request) ->
      Printers.print_headers req.Httpcats.headers
  | `Response (resp : Httpcats.response) -> Printers.print_response resp
  | `Headers_response (resp : Httpcats.response) ->
      Printers.print_headers resp.Httpcats.headers

let rec consumer first cfg qqueue =
  match Bqueue.get qqueue with
  | None -> ()
  | Some (((conn, tls), (req : Httpcats.request), resp), queue) ->
      let from = Source.of_bqueue queue in
      let (Printer (via, into)) =
        match (content_encoding resp, guess_how_to_print cfg resp) with
        | Some (#compression as algo), `Hex ->
            let into = Sink.hex cfg.hxd cfg.ppf in
            Printer (uncompress algo, into)
        | Some (#compression as algo), `Json ->
            let via = Flow.(uncompress algo % jsonm ()) in
            let into = Sink.jsonm cfg.ppf in
            Printer (via, into)
        | Some (#compression as algo), `Text ->
            let into = Sink.to_formatter cfg.ppf in
            Printer (uncompress algo, into)
        | None, `Hex -> Printer (Flow.identity, Sink.hex cfg.hxd cfg.ppf)
        | None, `Json ->
            let via = Flow.jsonm () in
            let into = Sink.jsonm cfg.ppf in
            Printer (via, into)
        | None, `Text -> Printer (Flow.identity, Sink.to_formatter cfg.ppf)
        | Some `Unknown, _ -> Printer (Flow.identity, Sink.hex cfg.hxd cfg.ppf)
      in
      let fn = function
        | `DNS -> if first then Some `DNS else None
        | `IP -> Some (`IP conn)
        | `TLS -> Some (`TLS tls)
        | `Request -> Some (`Request req)
        | `Headers_request ->
            let is_empty = H2.Headers.to_list req.Httpcats.headers = [] in
            if not is_empty then Some (`Headers_request req) else None
        | `Body_request -> None (* TODO *)
        | `Response -> Some (`Response resp)
        | `Headers_response ->
            let is_empty = H2.Headers.to_list resp.Httpcats.headers = [] in
            if not is_empty then Some (`Headers_response resp) else None
        | `Body_response -> None (* NOTE(dinosaure): done by [Stream.run]. *)
      in
      let datas = List.filter_map fn cfg.show in
      let rec go = function
        | [] -> ()
        | [ x ] -> show x
        | x :: r -> show x; Fmt.pr "\n%!"; go r
      in
      (* NOTE(dinosaure): if we need to show a second request, we must add a
         "\n" to separate the previous one and the current one. *)
      if not first then Fmt.pr "\n%!";
      go datas;
      (* NOTE(dinosaure): if we show up some meta-data and we would like to show
         the body of the response, we must separate these informations and the
         body of the response (regardless the format of it). *)
      if datas <> [] && List.mem `Body_response cfg.show then Fmt.pr "\n%!";
      let (), _source = Stream.run ~from ~via ~into in
      consumer false cfg qqueue

let run out_cfg ~resolver tls_config http_version ~follow_redirect max_redirect
    meth uri { Arg.headers; query; body } cookie =
  Logs.debug (fun m -> m "Add %d cookie(s)" (List.length cookie));
  let headers = List.append headers (Cookie.to_headers cookie) in
  let uri = uri ^ query in
  Logs.debug (fun m -> m "run with %s (body: %b)" uri (Option.is_some body));
  let body = Option.map Httpcats.stream body in
  let config =
    match http_version with
    | Some `HTTP_1_1 -> Some (`HTTP_1_1 H1.Config.default)
    | Some `H2 -> Some (`H2 H2.Config.default)
    | None -> None
  in
  let qqueue = Bqueue.create max_redirect (Obj.magic () (* TODO *)) in
  let fn meta req resp state str =
    match (state, str) with
    | `Continue queue, None -> Bqueue.close queue; `New_response
    | `Continue queue, Some str -> Bqueue.put queue str; `Continue queue
    | `New_response, Some str ->
        Logs.debug (fun m -> m "Got a new response");
        let queue = Bqueue.create 0x100 String.empty in
        let value = ((meta, req, resp), queue) in
        Bqueue.put queue str; Bqueue.put qqueue value; `Continue queue
    | `New_response, None ->
        let queue = Bqueue.create 0 String.empty in
        let value = ((meta, req, resp), queue) in
        Bqueue.close queue; Bqueue.put qqueue value; `New_response
  in
  Fun.protect ~finally:out_cfg.Out.finally @@ fun () ->
  let consumer = Miou.async @@ fun () -> consumer true out_cfg qqueue in
  Httpcats.request ?config ?tls_config ~resolver ~follow_redirect ~max_redirect
    ?meth ~headers ?body ~fn ~uri `New_response
  |> function
  | Ok (_resp, `New_response) -> Bqueue.close qqueue; Miou.await_exn consumer
  | Ok (_resp, `Continue queue) ->
      Bqueue.close queue; Bqueue.close qqueue; Miou.await_exn consumer
  | Error err ->
      Logs.err (fun m -> m "Got an error: %a" Httpcats.pp_error err);
      Miou.cancel consumer;
      Fmt.failwith "%a" Httpcats.pp_error err

let run out_cfg (tls_cfg, http_version) (daemon, resolver) follow_redirect
    max_redirect meth uri request cookies =
  let rng = Mirage_crypto_rng_miou_unix.(initialize (module Pfortuna)) in
  let finally () =
    Option.iter Happy_eyeballs_miou_unix.kill daemon;
    Mirage_crypto_rng_miou_unix.kill rng
  in
  Fun.protect ~finally @@ fun () ->
  match
    run out_cfg tls_cfg http_version ~resolver ~follow_redirect max_redirect
      meth uri request cookies
  with
  | () -> `Ok 0
  | exception Failure msg -> `Error (false, msg)

open Arg
open Cmdliner

let term =
  let open Term in
  ret
    (const run
    $ Out.setup
    $ setup_tls
    $ Resolver.setup
    $ follow_redirect
    $ max_redirect
    $ meth
    $ uri
    $ setup_request_items
    $ setup_cookies)

[@@@ocamlformat "disable"]

let cmd =
  let doc =
    "modern, user-friendly command-line HTTP client for the API era in OCaml."
  in
  let man = [
      `S "REQUEST ITEMS"
    ; `P "A request item is a key-value pair which will be included in the \
          request. The separator used determines the type:"
    ; `I ("':' HTTP headers:",
          "Referer:https://h.url Cookie:foo=bar User-Agent:Bacon/1.0")
    ; `I ("'==' URL parameters to be appended to the request URL:",
          "search==hurl")
    ; `I ("'=' Data fields to be serialized into a JSON object (with \
           $(b,--json), $(b,-j)) or form data (with $(b,--form), $(b,-f)):",
          "name=Hurl language=OCaml description='CLI HTTP client'")
    ; `I ("':=' Non-string JSON data fields (only with $(b,--json), $(b,-j)):",
          "awesome:=true amount:=42 colors='[\"red\", \"green\", \"blue\"]'")
    ; `I ("'@' Form file fields (only with $(b,--form) or $(b,--multipart)):",
          "cv@~/Documents/CV.pdf")
    ; `I ("'=@' A data field like '=', but takes a file path and embeds its \
           contents:",
          "essay=@Documents/essay.txt")
    ; `I ("':=@' A raw JSON field like ':=', but takes a file path and embeds \
           its content:",
          "package:=@./package.json")
    ] in
  Cmd.(v (info "hurl" ~doc ~man)) term
[@@@ocamlformat "enable"]

let () = Sys.set_signal Sys.sigpipe Sys.Signal_ignore

let () =
  let available = Stdlib.Domain.recommended_domain_count () in
  let domains = min 1 available in
  Miou_unix.run ~domains @@ fun () -> exit (Cmd.eval' cmd)
