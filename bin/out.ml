type show =
  [ `DNS
  | `IP
  | `TLS
  | `Request
  | `Response
  | `Headers_request
  | `Headers_response
  | `Body_request
  | `Body_response ]

type cfg = {
    quiet: bool
  ; ppf: Format.formatter
  ; finally: unit -> unit
  ; format_output: [ `Hex | `Json | `Raw | `None ]
  ; hxd: Hxd.cfg
  ; fields_filter: string list
  ; meta_and_resp: (Httpcats.meta * Httpcats.response) Miou.Computation.t
  ; show: show list
}

let show_dns cfg (record, domain_name, set) =
  if List.mem `DNS cfg.show && Ipaddr.Set.is_empty set = false then begin
    Printers.print_dns_result (record, domain_name, set);
    Fmt.pf cfg.ppf "\n%!"
  end

let show_ip cfg conn =
  if List.mem `IP cfg.show then begin
    Printers.print_address conn;
    Fmt.pf cfg.ppf "\n%!"
  end

let show_tls cfg = function
  | Some _ as tls when List.mem `TLS cfg.show ->
      Printers.print_tls tls; Fmt.pf cfg.ppf "\n%!"
  | _ -> ()

let show_response cfg resp =
  if List.mem `Response cfg.show then begin
    Printers.print_response resp;
    Fmt.pf cfg.ppf "\n%!"
  end

let show_request cfg req =
  if List.mem `Request cfg.show then begin
    Printers.print_request req; Fmt.pf cfg.ppf "\n%!"
  end

let show_headers_response cfg (resp : Httpcats.response) =
  let hdrs = resp.Httpcats.headers in
  let is_empty = H2.Headers.to_list hdrs = [] in
  if List.mem `Headers_response cfg.show && not is_empty then begin
    Printers.print_headers ~fields_filter:cfg.fields_filter hdrs;
    Fmt.pf cfg.ppf "\n%!"
  end

let show_headers_request cfg (req : Httpcats.request) =
  let hdrs = req.Httpcats.headers in
  let is_empty = H2.Headers.to_list hdrs = [] in
  if List.mem `Headers_request cfg.show && not is_empty then begin
    Printers.print_headers hdrs;
    Fmt.pf cfg.ppf "\n%!"
  end

let null = Format.make_formatter (fun _ _ _ -> ()) (fun _ -> ())

let setup (quiet, stdout) hxd print format_output fields_filter output =
  let ppf, finally =
    match (output, List.find_opt (( = ) `Body_response) print) with
    | _, None -> (null, Fun.const ())
    | Some location, _ ->
        let oc = open_out (Fpath.to_string location) in
        let finally () = close_out oc in
        let ppf =
          Format.make_formatter (output_substring oc) (fun () -> flush oc)
        in
        (ppf, finally)
    | None, _ -> (stdout, Fun.const ())
  in
  let meta_and_resp = Miou.Computation.create () in
  {
    quiet
  ; hxd
  ; format_output
  ; ppf
  ; finally
  ; fields_filter
  ; meta_and_resp
  ; show= print
  }

open Arg
open Cmdliner

let setup =
  let open Term in
  const setup
  $ setup_logs
  $ setup_hxd
  $ printers
  $ format_of_output
  $ setup_fields_filter
  $ output
