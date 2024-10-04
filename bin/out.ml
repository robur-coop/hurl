type show =
  [ `DNS
  | `IP
  | `TLS
  | `HTTP
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

let show_http cfg resp =
  if List.mem `HTTP cfg.show then begin
    Printers.print_response resp;
    Fmt.pf cfg.ppf "\n%!"
  end

let show_headers_response cfg resp =
  if List.mem `Headers_response cfg.show then begin
    Printers.print_headers_response ~fields_filter:cfg.fields_filter resp;
    Fmt.pf cfg.ppf "\n%!"
  end

let setup (quiet, stdout) hxd print format_output fields_filter output =
  let ppf, finally =
    match output with
    | Some location ->
        let oc = open_out (Fpath.to_string location) in
        let finally () = close_out oc in
        let ppf =
          Format.make_formatter (output_substring oc) (fun () -> flush oc)
        in
        (ppf, finally)
    | None -> (stdout, Fun.const ())
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
