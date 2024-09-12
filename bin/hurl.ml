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

let rec consumer cfg qqueue =
  match Bqueue.get qqueue with
  | None -> ()
  | Some (((conn, tls), resp), queue) ->
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
      Out.show_ip cfg conn;
      Out.show_tls cfg tls;
      Out.show_http cfg resp;
      Out.show_headers_response cfg resp;
      let (), _source = Stream.run ~from ~via ~into in
      Fmt.pf cfg.ppf "\n%!"; consumer cfg qqueue

let run out_cfg ~resolver tls_config http_version ~follow_redirect meth uri
    { Arg.headers; query; body } =
  let uri = uri ^ query in
  let body = Option.map Httpcats.stream body in
  let config =
    match http_version with
    | Some `HTTP_1_1 -> Some (`HTTP_1_1 H1.Config.default)
    | Some `H2 -> Some (`H2 H2.Config.default)
    | None -> None
  in
  let qqueue = Bqueue.create 16 (* max_redirect *) (Obj.magic () (* TODO *)) in
  let fn meta resp state str =
    match (state, str) with
    | `Continue queue, None -> Bqueue.close queue; `New_response
    | `Continue queue, Some str -> Bqueue.put queue str; `Continue queue
    | `New_response, Some str ->
        Logs.debug (fun m -> m "Got a new response");
        let queue = Bqueue.create 0x100 String.empty in
        let value = ((meta, resp), queue) in
        Bqueue.put queue str; Bqueue.put qqueue value; `Continue queue
    | `New_response, None ->
        let queue = Bqueue.create 0 String.empty in
        let value = ((meta, resp), queue) in
        Bqueue.close queue; Bqueue.put qqueue value; `New_response
  in
  Fun.protect ~finally:out_cfg.Out.finally @@ fun () ->
  let consumer = Miou.async @@ fun () -> consumer out_cfg qqueue in
  Httpcats.request ?config ?tls_config ~resolver ~follow_redirect ?meth ~headers
    ?body ~f:fn ~uri `New_response
  |> function
  | Ok (_resp, `New_response) -> Bqueue.close qqueue; Miou.await_exn consumer
  | Ok (_resp, `Continue queue) ->
      Bqueue.close queue; Bqueue.close qqueue; Miou.await_exn consumer
  | Error err ->
      Miou.await_exn consumer;
      Logs.err (fun m -> m "Got an error: %a" Httpcats.pp_error err);
      Fmt.failwith "%a" Httpcats.pp_error err

let run out_cfg (tls_cfg, http_version) happy_eyeballs_cfg dns_cfg nameservers
    follow_redirect meth uri request =
  Miou_unix.run @@ fun () ->
  let rng = Mirage_crypto_rng_miou_unix.(initialize (module Pfortuna)) in
  let daemon, resolver =
    Resolver.v out_cfg happy_eyeballs_cfg dns_cfg nameservers
  in
  let finally () =
    Option.iter Happy_eyeballs_miou_unix.kill daemon;
    Mirage_crypto_rng_miou_unix.kill rng
  in
  Fun.protect ~finally @@ fun () ->
  match
    run out_cfg tls_cfg http_version ~resolver ~follow_redirect meth uri request
  with
  | () -> `Ok 0
  | exception Failure msg -> `Error (false, msg)

open Arg
open Cmdliner

let term =
  let open Term in
  ret
    (const run
    $ setup_out
    $ setup_tls
    $ setup_happy_eyeballs
    $ dns
    $ setup_nameservers
    $ follow_redirect
    $ meth
    $ uri
    $ setup_request_items)

let cmd =
  let doc =
    "modern, user-friendly command-line HTTP client for the API era in OCaml."
  in
  let man = [] in
  Cmd.(v (info "hurl" ~doc ~man)) term

let () = Sys.set_signal Sys.sigpipe Sys.Signal_ignore
let () = exit (Cmd.eval' cmd)
