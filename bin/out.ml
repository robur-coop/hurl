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

let pp_show ppf = function
  | `DNS -> Fmt.string ppf "dns"
  | `IP -> Fmt.string ppf "ip"
  | `TLS -> Fmt.string ppf "tls"
  | `Request -> Fmt.string ppf "request"
  | `Response -> Fmt.string ppf "response"
  | `Headers_request -> Fmt.string ppf "request:headers"
  | `Headers_response -> Fmt.string ppf "response:headers"
  | `Body_request -> Fmt.string ppf "request:body"
  | `Body_response -> Fmt.string ppf "response:body"

let show_to_int = function
  | `DNS -> 0
  | `IP -> 1
  | `TLS -> 2
  | `Request -> 3
  | `Headers_request -> 4
  | `Body_request -> 5
  | `Response -> 6
  | `Headers_response -> 7
  | `Body_response -> 8

let show_compare a b = show_to_int a - show_to_int b

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
  let show = List.sort show_compare print in
  {
    quiet
  ; hxd
  ; format_output
  ; ppf
  ; finally
  ; fields_filter
  ; meta_and_resp
  ; show
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
