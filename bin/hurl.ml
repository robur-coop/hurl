let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let ( % ) f g x = f (g x)

let stream_of_location ?(size_chunk = 0x1000) location =
  let ic = Miou.Lazy.from_fun @@ fun () -> open_in (Fpath.to_string location) in
  let buf = Bytes.create size_chunk in
  fun () ->
    let ic = Miou.Lazy.force ic in
    match input ic buf 0 (Bytes.length buf) with
    | 0 -> close_in ic; None
    | len -> Some (Bytes.unsafe_to_string buf, 0, len)
    | exception End_of_file -> close_in ic; None

let headers_of_request_items =
  Fun.flip List.fold_left [] @@ fun acc -> function
  | Arg.Header (k, v) -> (k, v) :: acc | _ -> acc

let query_of_request_items request_items =
  let parameters =
    Fun.flip List.fold_left [] @@ fun acc -> function
    | Arg.Url_parameter (k, v) -> (k, [ v ]) :: acc | _ -> acc
  in
  Pct.encode (parameters request_items)

let parts_of_request_items =
  Fun.flip List.fold_left [] @@ fun acc -> function
  | Arg.Header _ | Json _ | String _ | Json_from_location _
  | String_from_location _ ->
      acc
  | Part (name, location) ->
      let stream = stream_of_location location in
      let disposition =
        Multipart_form.Content_disposition.v ~filename:(Fpath.basename location)
          name
      in
      Multipart_form.part ~disposition stream :: acc
  | Url_parameter (name, v) ->
      let stream = Seq.(to_dispenser (return (v, 0, String.length v))) in
      let disposition = Multipart_form.Content_disposition.v name in
      Multipart_form.part ~disposition stream :: acc

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
          | Arg.Header _ | Url_parameter _ | Part _ -> acc
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

let application_json =
  (`Application, Result.get_ok (Multipart_form.Content_type.Subtype.iana "json"))

type state =
  | Begin
  | Hex of (unit, Hex.n) result Hex.command
  | Raw
  | Json
  | Ignore

let json_request request_items =
  let seq = json_of_request_items request_items in
  let seq = Json.lexemes_to_seq_of_bytes ~minify:true seq in
  Httpcats.stream seq

let rng ?g:_ len =
  let res = Bytes.create len in
  for i = 0 to len - 1 do
    match Random.int (26 + 26 + 10) with
    | n when n < 26 -> Bytes.set res i Char.(chr (code 'a' + n))
    | n when n < 26 + 26 -> Bytes.set res i Char.(chr (code 'A' + n - 26))
    | n -> Bytes.set res i Char.(chr (code '0' + n - 26 - 26))
  done;
  Bytes.unsafe_to_string res

let multipart_request request_items =
  let parts = parts_of_request_items request_items in
  let multipart = Multipart_form.multipart ~rng parts in
  let _hdr, stream = Multipart_form.to_stream multipart in
  let seq =
    let open Seq in
    map (fun (str, off, len) -> String.sub str off len) (of_dispenser stream)
  in
  Httpcats.stream seq

let ready_for_a_redirection ~uri resp =
  let open Httpcats in
  if Status.is_redirection resp.status then
    match Headers.get resp.headers "location" with
    | Some location ->
        let uri = resolve_location ~uri ~location in
        Result.is_ok uri
    | None -> false
  else false

type printer = Printer : (string, 'v) Flow.t * ('v, unit) Sink.t -> printer

module Out = struct
  type cfg = {
      quiet: bool
    ; ppf: Format.formatter
    ; finally: unit -> unit
    ; hex: bool
    ; hxd: Hxd.cfg
    ; fields_filter: string list
    ; meta_and_resp: (Httpcats.meta * Httpcats.response) Miou.Computation.t
  }

  let setup_out (quiet, stdout) hxd hex fields_filter output =
    let ppf, finally =
      match output with
      | Some location ->
          let oc = open_out (Fpath.to_string location) in
          ( Format.make_formatter (output_substring oc) (fun () -> flush oc)
          , fun () -> close_out oc )
      | None -> (stdout, Fun.const ())
    in
    let meta_and_resp = Miou.Computation.create () in
    { quiet; hxd; hex; ppf; finally; fields_filter; meta_and_resp }

  let setup_out =
    let open Arg in
    Cmdliner.Term.(
      const setup_out
      $ setup_logs
      $ setup_hxd
      $ hex
      $ setup_fields_filter
      $ output)
end

let guess_how_to_print cfg resp =
  if cfg.Out.hex then `Hex
  else
    match recognize_mime_type resp with
    | Some (`Text, _) -> `Text
    | Some (`Application, `Iana_token "json") -> `Json
    | _ -> `Hex

let compressed_content resp = content_encoding resp

let rec consumer cfg qqueue =
  match Bqueue.get qqueue with
  | None -> ()
  | Some (((conn, tls), resp), queue) ->
      let from = Source.of_bqueue queue in
      let (Printer (via, into)) =
        match (compressed_content resp, guess_how_to_print cfg resp) with
        | Some `Gzip, `Hex ->
            let via = Flow.(to_bigstring () % gzip () % to_string) in
            let into = Sink.hex cfg.hxd cfg.ppf in
            Printer (via, into)
        | Some `Gzip, `Json ->
            Logs.debug (fun m -> m "bigstring %% gzip %% string %% jsonm");
            let via = Flow.(to_bigstring () % gzip () % to_string % jsonm ()) in
            let into = Sink.jsonm cfg.ppf in
            Printer (via, into)
        | Some `Gzip, `Text ->
            Logs.debug (fun m -> m "bigstring %% gzip %% string");
            let via = Flow.(to_bigstring () % gzip () % to_string) in
            let into = Sink.to_formatter cfg.ppf in
            Printer (via, into)
        | None, `Hex -> Printer (Flow.identity, Sink.hex cfg.hxd cfg.ppf)
        | None, `Json ->
            let via = Flow.jsonm () in
            let into = Sink.jsonm cfg.ppf in
            Printer (via, into)
        | None, `Text -> Printer (Flow.identity, Sink.to_formatter cfg.ppf)
        | _ -> assert false
      in
      Printers.print_address conn;
      Fmt.pf cfg.ppf "\n%!";
      Printers.print_tls tls;
      if Option.is_some tls then Fmt.pf cfg.ppf "\n%!";
      Printers.print_response ~fields_filter:cfg.fields_filter resp;
      Fmt.pf cfg.ppf "\n%!";
      let (), _source = Stream.run ~from ~via ~into in
      Fmt.pf cfg.ppf "\n%!"; consumer cfg qqueue

let run out_cfg ~resolver tls_config http_version ~follow_redirect meth uri
    request_items =
  let headers = headers_of_request_items request_items in
  let query = query_of_request_items request_items in
  let uri = uri ^ query in
  let body =
    match request_items with
    | [] -> None
    | request_items -> Some (json_request request_items)
  in
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
  Printers.print_headers (Httpcats.Headers.of_list headers);
  Fmt.pf out_cfg.Out.ppf "\n%!";
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

let system_getaddrinfo record domain_name =
  let opt =
    match record with
    | `A -> [ Unix.AI_FAMILY Unix.PF_INET ]
    | `AAAA -> [ Unix.AI_FAMILY Unix.PF_INET6 ]
  in
  let opt = Unix.AI_SOCKTYPE Unix.SOCK_STREAM :: opt in
  match Unix.getaddrinfo (Domain_name.to_string domain_name) "" opt with
  | [] -> error_msgf "%a not found" Domain_name.pp domain_name
  | addrs ->
      let set =
        List.fold_left
          (fun set { Unix.ai_addr; _ } ->
            match ai_addr with
            | Unix.ADDR_INET (inet_addr, _) ->
                Ipaddr.Set.add (Ipaddr_unix.of_inet_addr inet_addr) set
            | Unix.ADDR_UNIX _ -> set)
          Ipaddr.Set.empty addrs
      in
      if Ipaddr.Set.is_empty set = false then
        Printers.print_dns_result (record, domain_name, set);
      if Ipaddr.Set.is_empty set then
        error_msgf "%a not found as an inet service" Domain_name.pp domain_name
      else Ok set

let dns_getaddrinfo dns record domain_name =
  let ( let* ) = Result.bind in
  match record with
  | `A ->
      let* ipaddr = Dns_client_miou_unix.gethostbyname dns domain_name in
      let set = Ipaddr.(Set.singleton (V4 ipaddr)) in
      Printers.print_dns_result (record, domain_name, set);
      Ok set
  | `AAAA ->
      let* ipaddr = Dns_client_miou_unix.gethostbyname6 dns domain_name in
      let set = Ipaddr.(Set.singleton (V6 ipaddr)) in
      Printers.print_dns_result (record, domain_name, set);
      Ok set

let now = Mtime_clock.elapsed_ns

let run out_cfg (tls_cfg, http_version) happy_eyeballs_cfg dns_cfg nameservers
    follow_redirect meth uri request_items =
  Miou_unix.run @@ fun () ->
  let daemon, resolver =
    match (happy_eyeballs_cfg, dns_cfg) with
    | None, (`System | `OCaml) -> (None, `System)
    | Some he, `System ->
        let {
          Arg.aaaa_timeout
        ; connect_delay
        ; connect_timeout
        ; resolve_timeout
        ; resolve_retries
        } =
          he
        in
        let happy_eyeballs =
          Happy_eyeballs.create ?aaaa_timeout ?connect_delay ?connect_timeout
            ?resolve_timeout ?resolve_retries (now ())
        in
        let daemon, resolver =
          Happy_eyeballs_miou_unix.create ~happy_eyeballs
            ~getaddrinfo:system_getaddrinfo ()
        in
        (Some daemon, `Happy resolver)
    | Some he, `OCaml ->
        let {
          Arg.aaaa_timeout
        ; connect_delay
        ; connect_timeout
        ; resolve_timeout
        ; resolve_retries
        } =
          he
        in
        let happy_eyeballs =
          Happy_eyeballs.create ?aaaa_timeout ?connect_delay ?connect_timeout
            ?resolve_timeout ?resolve_retries (now ())
        in
        let daemon, resolver =
          Happy_eyeballs_miou_unix.create ~happy_eyeballs ?getaddrinfo:None ()
        in
        let dns = Dns_client_miou_unix.create ~nameservers resolver in
        Happy_eyeballs_miou_unix.inject resolver (dns_getaddrinfo dns);
        (Some daemon, `Happy resolver)
  in
  let rng = Mirage_crypto_rng_miou_unix.(initialize (module Pfortuna)) in
  let finally () =
    Option.iter Happy_eyeballs_miou_unix.kill daemon;
    Mirage_crypto_rng_miou_unix.kill rng
  in
  Fun.protect ~finally @@ fun () ->
  match
    run out_cfg tls_cfg http_version ~resolver ~follow_redirect meth uri
      request_items
  with
  | () -> `Ok 0
  | exception Failure msg -> `Error (false, msg)

open Arg
open Cmdliner

let term =
  Term.(
    ret
      (const run
      $ Out.setup_out
      $ setup_tls
      $ setup_happy_eyeballs
      $ dns
      $ setup_nameservers
      $ follow_redirect
      $ meth
      $ uri
      $ request_items))

let cmd =
  let doc =
    "$(tname): modern, user-friendly command-line HTTP client for the API era \
     in OCaml."
  in
  let man = [] in
  Cmd.(v (info "hurl" ~doc ~man)) term

let () = Sys.set_signal Sys.sigpipe Sys.Signal_ignore
let () = exit (Cmd.eval' cmd)
