(* Copyright (c) 2020 Rizo I. <rizo@odis.io>
   Copyright (c) 2022 Romain Calascibetta <romain.calascibetta@gmail.com>

   Permission to use, copy, modify, and distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*)

type ('a, 'r) t =
  | Sink : {
        init: unit -> 's
      ; push: 's -> 'a -> 's
      ; full: 's -> bool
      ; stop: 's -> 'r
    }
      -> ('a, 'r) t

let hex cfg ppf =
  let init () = (Hex.generate cfg ppf, ppf) in
  let push ((hex : (unit, Hex.n) result Hex.command), ppf) str =
    (Hex.consume str hex, ppf)
  in
  let full _ = false in
  let stop ((hex : (unit, Hex.n) result Hex.command), ppf) =
    let (Ok ()) = Hex.finalize hex in
    Fmt.pf ppf "%!"
  in
  Sink { init; push; full; stop }

let rec until_ok encoder buf ppf = function
  | `Partial ->
      let len = Bytes.length buf - Jsonm.Manual.dst_rem encoder in
      Format.pp_print_string ppf (Bytes.sub_string buf 0 len);
      Jsonm.Manual.dst encoder buf 0 (Bytes.length buf);
      until_ok encoder buf ppf (Jsonm.encode encoder `Await)
  | `Ok -> ()

let jsonm ?(minify = false) ?(len = 0x800) ppf =
  let init () =
    let encoder = Jsonm.encoder ~minify `Manual in
    let buf = Bytes.create len in
    Jsonm.Manual.dst encoder buf 0 len;
    (encoder, buf, ppf)
  in
  let push (encoder, buf, ppf) lexeme =
    until_ok encoder buf ppf (Jsonm.encode encoder (`Lexeme lexeme));
    (encoder, buf, ppf)
  in
  let full _ = false in
  let stop (encoder, buf, ppf) =
    until_ok encoder buf ppf (Jsonm.encode encoder `End);
    Fmt.pf ppf "%!"
  in
  Sink { init; push; full; stop }

let to_formatter ppf =
  let init () = ppf
  and push ppf str =
    Format.pp_print_string ppf str;
    ppf
  and full _ = false
  and stop ppf = Fmt.pf ppf "%!" in
  Sink { init; push; full; stop }
