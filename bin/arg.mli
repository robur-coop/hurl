open Cmdliner

type request_item = private
  | Header of string * string
  | Url_parameter of string * string
  | Json of string * Json.t
  | String of string * string
  | Json_from_location of string * Fpath.t
  | String_from_location of string * Fpath.t
  | Part of string * Fpath.t

type happy_eyeballs = {
    aaaa_timeout: int64 option
  ; connect_delay: int64 option
  ; connect_timeout: int64 option
  ; resolve_timeout: int64 option
  ; resolve_retries: int option
}

type nameserver =
  [ `Plaintext of Ipaddr.t * int | `Tls of Tls.Config.client * Ipaddr.t * int ]

type printer =
  [ `DNS
  | `IP
  | `TLS
  | `Request
  | `Response
  | `Headers_request
  | `Headers_response
  | `Body_request
  | `Body_response ]

type request = {
    headers: (string * string) list
  ; query: string
  ; body: string Seq.t option
}

val meth : H1.Method.t option Term.t
val max_redirect : int Term.t
val uri : string Term.t
val request_items : request_item list Term.t
val follow_redirect : bool Term.t
val output : Fpath.t option Term.t
val dns : [ `System | `OCaml ] Term.t
val printers : printer list Term.t
val format_of_output : [ `Hex | `Json | `Raw | `None ] Term.t
val format_of_input : [ `Json | `Form | `Multipart | `Raw ] Term.t

(**/*)

val setup_logs : (bool * Format.formatter) Term.t
val setup_hxd : Hxd.cfg Term.t
val setup_tls : (Tls.Config.client option * [ `HTTP_1_1 | `H2 ] option) Term.t
val setup_happy_eyeballs : happy_eyeballs option Term.t
val setup_nameservers : (Dns.proto * nameserver list) Term.t
val setup_fields_filter : string list Term.t
val setup_request_items : request Term.t
val setup_cookies : string list Term.t
