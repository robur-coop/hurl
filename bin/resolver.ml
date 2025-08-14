let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let now = Mtime_clock.elapsed_ns

type resolver =
  [ `Happy of Happy_eyeballs_miou_unix.t | `System | `User of Httpcats.resolver ]

let system_getaddrinfo out_cfg record domain_name =
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
      if List.mem `DNS out_cfg.Out.show && Ipaddr.Set.is_empty set = false then
        Printers.print_dns_result (record, domain_name, set);
      if Ipaddr.Set.is_empty set then
        error_msgf "%a not found as an inet service" Domain_name.pp domain_name
      else Ok set

let dns_getaddrinfo out_cfg dns record domain_name =
  let ( let* ) = Result.bind in
  match record with
  | `A ->
      let* ipaddr = Dns_client_miou_unix.gethostbyname dns domain_name in
      let set = Ipaddr.(Set.singleton (V4 ipaddr)) in
      if List.mem `DNS out_cfg.Out.show && Ipaddr.Set.is_empty set = false then
        Printers.print_dns_result (record, domain_name, set);
      Ok set
  | `AAAA ->
      let* ipaddr = Dns_client_miou_unix.gethostbyname6 dns domain_name in
      let set = Ipaddr.(Set.singleton (V6 ipaddr)) in
      if List.mem `DNS out_cfg.Out.show && Ipaddr.Set.is_empty set = false then
        Printers.print_dns_result (record, domain_name, set);
      Ok set

let setup out_cfg happy_eyeballs_cfg dns_cfg nameservers =
  let happy_eyeballs =
    match happy_eyeballs_cfg with
    | None -> None
    | Some
        {
          Arg.aaaa_timeout
        ; connect_delay
        ; connect_timeout
        ; resolve_timeout
        ; resolve_retries
        } ->
        Happy_eyeballs.create ?aaaa_timeout ?connect_delay ?connect_timeout
          ?resolve_timeout ?resolve_retries (now ())
        |> Option.some
  in
  match dns_cfg with
  | `System ->
      let daemon, resolver =
        Happy_eyeballs_miou_unix.create ?happy_eyeballs
          ~getaddrinfo:(system_getaddrinfo out_cfg)
          ()
      in
      (Some daemon, `Happy resolver)
  | `OCaml ->
      let daemon, resolver =
        Happy_eyeballs_miou_unix.create ~timer_interval:(Duration.of_sec 1)
          ?happy_eyeballs ?getaddrinfo:None ()
      in
      let dns = Dns_client_miou_unix.create ~nameservers resolver in
      Happy_eyeballs_miou_unix.inject resolver (dns_getaddrinfo out_cfg dns);
      (Some daemon, `Happy resolver)

open Arg
open Cmdliner

let setup =
  let open Term in
  const setup $ Out.setup $ setup_happy_eyeballs $ dns $ setup_nameservers
