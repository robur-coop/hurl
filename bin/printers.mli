val print_address : Ipaddr.t * int -> unit
val print_response : Httpcats.response -> unit
val print_request : Httpcats.request -> unit
val print_headers : ?fields_filter:string list -> Httpcats.Headers.t -> unit

val print_dns_result :
  [ `A | `AAAA ] * [ `host ] Domain_name.t * Ipaddr.Set.t -> unit

val print_tls : Tls.Core.epoch_data option -> unit
