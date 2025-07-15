let mutex = Miou.Mutex.create ()
let pr fmt = Miou.Mutex.protect mutex @@ fun () -> Fmt.pf Fmt.stdout fmt

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

let pp_version ppf { H1.Version.major; minor } =
  match (major, minor) with
  | 1, 0 -> Fmt.(styled (`Fg `Blue) string) ppf "HTTP/1.0"
  | 1, 1 -> Fmt.(styled (`Fg `Blue) string) ppf "HTTP/1.1"
  | 2, 0 -> Fmt.(styled (`Fg `Blue) string) ppf "H2"
  | v -> Fmt.(styled (`Fg `Red) (fun ppf (x, y) -> fmt "%d.%d" ppf x y)) ppf v

let pp_meth ppf = function
  | `CONNECT -> Fmt.(styled (`Fg `Green) string) ppf "CONNECT"
  | `DELETE -> Fmt.(styled (`Fg `Green) string) ppf "DELETE"
  | `GET -> Fmt.(styled (`Fg `Green) string) ppf "GET"
  | `HEAD -> Fmt.(styled (`Fg `Green) string) ppf "HEAD"
  | `OPTIONS -> Fmt.(styled (`Fg `Green) string) ppf "OPTIONS"
  | `POST -> Fmt.(styled (`Fg `Green) string) ppf "POST"
  | `PUT -> Fmt.(styled (`Fg `Green) string) ppf "PUT"
  | `TRACE -> Fmt.(styled (`Fg `Green) string) ppf "TRACE"
  | `Other str -> Fmt.(styled (`Fg `Yellow) string) ppf str

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

let split ~first ~column str =
  let rec go acc (off, len) =
    if len <= 0 then List.rev acc
    else
      let len' = Int.min (column - 1) len in
      let sub = String.sub str off len' in
      go (sub :: acc) (off + len', len - len')
  in
  let len' = Int.min first (String.length str) in
  let x = String.sub str 0 len' in
  go [ x ] (len', String.length str - len')

let print_field ppf (name, value) =
  let first = 80 - (String.length name + 2) in
  match wrap ~first ~column:80 value with
  | [] -> Fmt.pf ppf "%a\n%!" Fmt.(styled (`Fg `Cyan) string) name
  | [ [ huge_line ] ] when first + String.length huge_line > 80 ->
      let[@warning "-8"] (x :: r) = split ~first ~column:80 huge_line in
      Fmt.pf ppf "%a: %s\n%!" Fmt.(styled (`Fg `Cyan) string) name x;
      List.iter (fun str -> Fmt.pf ppf " %s\n%!" str) r
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
    resp.Httpcats.status pp_reason resp.Httpcats.status

let print_request ppf req =
  Fmt.pf ppf "%a %s\n%!" pp_meth req.Httpcats.meth req.Httpcats.target

let print_headers ?(fields_filter = []) ppf hdrs =
  List.iter
    (fun (field, value) ->
      if List.mem field fields_filter = false then print_field ppf (field, value))
    (Httpcats.Headers.to_list hdrs)

let print_address ppf (ipaddr, port) =
  let color_of_port =
    match port with 80 -> `Blue | 443 -> `Green | _ -> `Yellow
  in
  Fmt.pf ppf "%a:%a\n%!"
    Fmt.(styled `Magenta Ipaddr.pp)
    ipaddr
    Fmt.(styled color_of_port int)
    port

let pp_record ppf = function
  | `A -> Fmt.(styled `Blue string) ppf "A"
  | `AAAA -> Fmt.(styled `Blue string) ppf "AAAA"

let print_dns_result ppf (record, domain_name, set) =
  Fmt.pf ppf "%a %a:\n%!" pp_record record Domain_name.pp domain_name;
  List.iter
    (Fmt.pf ppf "  %a\n%!" Fmt.(styled `Green Ipaddr.pp))
    (Ipaddr.Set.to_list set)

let pp_tls_version ppf = function
  | `TLS_1_0 -> Fmt.string ppf "TLS 1.0"
  | `TLS_1_1 -> Fmt.string ppf "TLS 1.1"
  | `TLS_1_2 -> Fmt.string ppf "TLS 1.2"
  | `TLS_1_3 -> Fmt.string ppf "TLS 1.3"

let pp_alpn_protocol ppf = function
  | None -> ()
  | Some v -> Fmt.pf ppf "(%a) " Fmt.(styled `Cyan string) v

let print_tls ppf { Tls.Core.protocol_version; ciphersuite; alpn_protocol; _ } =
  Fmt.pf ppf "%a %awith %a\n%!"
    Fmt.(styled `Blue pp_tls_version)
    protocol_version pp_alpn_protocol alpn_protocol
    Fmt.(styled `Magenta Tls.Ciphersuite.pp_ciphersuite)
    ciphersuite

let print_address value = pr "%a" print_address value
let print_response value = pr "%a" print_response value
let print_request value = pr "%a" print_request value

let print_headers ?fields_filter hdrs =
  pr "%a" (print_headers ?fields_filter) hdrs

let print_dns_result value = pr "%a" print_dns_result value
let print_tls value = Option.iter (pr "%a" print_tls) value
