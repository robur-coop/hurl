open Cmdliner

val meth : H1.Method.t option Term.t
val uri : string Term.t

type request_item = private
  | Header of string * string
  | Url_parameter of string * string
  | Json of string * Json.t
  | String of string * string
  | Json_from_location of string * Fpath.t
  | String_from_location of string * Fpath.t
  | Part of string * Fpath.t

val request_items : request_item list Term.t
val hex : bool Term.t
val follow_redirect : bool Term.t
val output : Fpath.t option Term.t
val setup_logs : (bool * Format.formatter) Term.t
val setup_hxd : Hxd.cfg Term.t
val setup_tls : (Tls.Config.client option * [ `HTTP_1_1 | `H2 ] option) Term.t
