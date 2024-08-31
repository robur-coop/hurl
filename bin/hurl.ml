let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let ( % ) f g x = f (g x)

let pp_version ppf { H1.Version.major; minor } =
  match (major, minor) with
  | 1, 0 -> Fmt.(styled (`Fg `Blue) string) ppf "HTTP/1.0"
  | 1, 1 -> Fmt.(styled (`Fg `Blue) string) ppf "HTTP/1.1"
  | 2, 0 -> Fmt.(styled (`Fg `Blue) string) ppf "H2"
  | v -> Fmt.(styled (`Fg `Red) (fun ppf (x, y) -> fmt "%d.%d" ppf x y)) ppf v

let pp_status ppf status =
  let code = H2.Status.to_code status in
  let color =
    if code < 299 then `Green
    else if code >= 300 && code < 399 then `Cyan
    else if code >= 400 && code < 499 then `Yellow
    else `Red
  in
  Fmt.pf ppf "%a" Fmt.(styled (`Fg color) (fmt "%3d")) code

let pp_reason ppf = function
  | #H2.Status.standard as v ->
      Fmt.string ppf (H2.Status.default_reason_phrase v)
  | `Code _ -> ()

let wrap ?first ~column str =
  let first =
    match first with
    | Some first when first > 0 -> first
    | Some _ -> 0
    | None -> column
  in
  let words = String.split_on_char ' ' str in
  let add_word word = function
    | [] -> [ [ word ] ]
    | line :: rest -> (word :: line) :: rest
  in
  let add_line lines = [] :: lines in
  let _, _, rlines =
    List.fold_left
      begin
        fun (width, c, acc) word ->
          let wlen = String.length word in
          let len = width + wlen in
          if len >= c then (wlen, column, add_word word (add_line acc))
          else (len, c, add_word word acc)
      end
      (0, first, []) words
  in
  List.rev_map List.rev rlines

let print_field ppf (name, value) =
  match wrap ~first:(80 - String.length name + 1) ~column:80 value with
  | [] -> Fmt.pf ppf "%a\n%!" Fmt.(styled (`Fg `Cyan) string) name
  | [ line ] ->
      Fmt.pf ppf "%a: %s\n%!"
        Fmt.(styled (`Fg `Cyan) string)
        name (String.concat " " line)
  | x :: r ->
      Fmt.pf ppf "%a: %s\n%!"
        Fmt.(styled (`Fg `Cyan) string)
        name (String.concat " " x);
      List.iter (fun line -> Fmt.pf ppf "  %s\n%!" (String.concat " " line)) r

let print_response ppf resp =
  Fmt.pf ppf "%a %a %a\n%!" pp_version resp.Httpcats.version pp_status
    resp.Httpcats.status pp_reason resp.Httpcats.status;
  List.iter (print_field ppf) (Httpcats.Headers.to_list resp.Httpcats.headers)

let print_address ppf (ipaddr, port) =
  let color_of_port =
    match port with 80 -> `Blue | 443 -> `Green | _ -> `Yellow
  in
  Fmt.pf ppf "%a:%a\n%!"
    Fmt.(styled `Magenta Ipaddr.pp)
    ipaddr
    Fmt.(styled color_of_port int)
    port

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
  List.fold_left
    begin
      fun acc -> function Arg.Header (k, v) -> (k, v) :: acc | _ -> acc
    end
    []

let parts_of_request_items request_items =
  List.fold_left
    begin
      fun acc -> function
        | Arg.Header _ | Json _ | String _ | Json_from_location _
        | String_from_location _ ->
            acc
        | Part (name, location) ->
            let stream = stream_of_location location in
            let disposition =
              Multipart_form.Content_disposition.v
                ~filename:(Fpath.basename location) name
            in
            Multipart_form.part ~disposition stream :: acc
        | Url_parameter (name, v) ->
            let stream = Seq.(to_dispenser (return (v, 0, String.length v))) in
            let disposition = Multipart_form.Content_disposition.v name in
            Multipart_form.part ~disposition stream :: acc
    end
    [] request_items

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
  let seq = Json.lexemes_to_seq_of_bytes seq in
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

let guess_how_to_print ?(hex = false) resp =
  if hex then `Hex
  else
    match (recognize_mime_type resp, content_encoding resp) with
    | Some (`Text, _), None -> `Raw
    | _ -> `Hex

let run ?(hex = true) (hxd_cfg, stdout) ~resolver tls_config http_version
    ~follow_redirect meth uri request_items output =
  let ppf, finally =
    match output with
    | Some location ->
        let oc = open_out (Fpath.to_string location) in
        ( Format.make_formatter (output_substring oc) (fun () -> flush oc)
        , fun () -> close_out oc )
    | None -> (stdout, Fun.const ())
  in
  let headers = headers_of_request_items request_items in
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
  let rec fn (((ipaddr, port), _epoch) as meta) resp acc str =
    match (str, acc) with
    | _, Begin when ready_for_a_redirection ~uri resp && follow_redirect ->
        print_address Fmt.stdout (ipaddr, port);
        print_response Fmt.stdout resp;
        Fmt.pr "\n%!";
        Ignore
    | None, Begin ->
        print_response Fmt.stdout resp;
        Begin
    | Some str, Begin ->
        print_address Fmt.stdout (ipaddr, port);
        print_response Fmt.stdout resp;
        Fmt.pr "\n%!";
        begin
          match guess_how_to_print ~hex resp with
          | `Raw -> fn meta resp Raw (Some str)
          | `Hex -> fn meta resp (Hex (Hex.generate hxd_cfg ppf)) (Some str)
        end
    | Some str, Raw -> Fmt.pf ppf "%s" str; Raw
    | None, Raw -> Raw
    | _, Json -> Json
    | _, Ignore -> Ignore
    | Some str, Hex t -> Hex (Hex.consume str t)
    | None, Hex t ->
        let (Ok ()) = Hex.finalize t in
        Fmt.pf ppf "%!"; Hex t
  in
  Fun.protect ~finally @@ fun () ->
  Httpcats.request ?config ?tls_config ~resolver ~follow_redirect ?meth ~headers
    ?body ~f:fn ~uri Begin
  |> function
  | Ok (resp, Begin) -> print_response Fmt.stdout resp
  | Ok (_resp, _) -> ()
  | Error err -> Fmt.failwith "%a" Httpcats.pp_error err

let pp_record ppf = function
  | `A -> Fmt.(styled `Blue string) ppf "A"
  | `AAAA -> Fmt.(styled `Blue string) ppf "AAAA"

let print_dns_result ppf (record, domain_name, set) =
  let ipaddrs = Ipaddr.Set.to_list set in
  Fmt.pf ppf "%a %a => @[<hov>%a@]\n%!" pp_record record Domain_name.pp
    domain_name
    Fmt.(list ~sep:(any "@\n") Fmt.(styled `Green Ipaddr.pp))
    ipaddrs

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
        print_dns_result Fmt.stdout (record, domain_name, set);
      if Ipaddr.Set.is_empty set then
        error_msgf "%a not found as an inet service" Domain_name.pp domain_name
      else Ok set

let dns_getaddrinfo dns record domain_name =
  let ( let* ) = Result.bind in
  match record with
  | `A ->
      let* ipaddr = Dns_client_miou_unix.gethostbyname dns domain_name in
      let set = Ipaddr.(Set.singleton (V4 ipaddr)) in
      print_dns_result Fmt.stdout (record, domain_name, set);
      Ok set
  | `AAAA ->
      let* ipaddr = Dns_client_miou_unix.gethostbyname6 dns domain_name in
      let set = Ipaddr.(Set.singleton (V6 ipaddr)) in
      print_dns_result Fmt.stdout (record, domain_name, set);
      Ok set

let now = Mtime_clock.elapsed_ns

let run (_quiet, stdout) hxd_cfg (tls_cfg, http_version) happy_eyeballs_cfg
    dns_cfg nameservers follow_redirect hex meth uri request_items output =
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
    run ~hex (hxd_cfg, stdout) tls_cfg http_version ~resolver ~follow_redirect
      meth uri request_items output
  with
  | () -> `Ok 0
  | exception Failure msg -> `Error (false, msg)

open Arg
open Cmdliner

let term =
  Term.(
    ret
      (const run
      $ setup_logs
      $ setup_hxd
      $ setup_tls
      $ setup_happy_eyeballs
      $ dns
      $ setup_nameservers
      $ follow_redirect
      $ hex
      $ meth
      $ uri
      $ request_items
      $ output))

let cmd =
  let doc =
    "$(tname): modern, user-friendly command-line HTTP client for the API era \
     in OCaml."
  in
  let man = [] in
  Cmd.(v (info "hurl" ~doc ~man)) term

let () = exit (Cmd.eval' cmd)
