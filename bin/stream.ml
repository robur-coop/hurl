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

type 'a t = { stream: 'r. ('a, 'r) Sink.t -> 'r } [@@unboxed]

external reraise : exn -> 'a = "%reraise"

let run ~from:(Source.Source src) ~via:{ Flow.flow } ~into:snk =
  let (Sink.Sink snk) = flow snk in
  let rec loop r s =
    match snk.full r with
    | true ->
        let r' = snk.stop r in
        let leftover = Source.Source { src with init= Fun.const s } in
        (r', Some leftover)
    | false -> (
        match src.pull s with
        | Some (x, s') ->
            let r' = snk.push r x in
            loop r' s'
        | None ->
            src.stop s;
            let r' = snk.stop r in
            (r', None))
  in
  let r0 = snk.init () in
  match snk.full r0 with
  | true ->
      let r' = snk.stop r0 in
      (r', Some (Source.Source src))
  | false -> (
      let s0 =
        try src.init ()
        with exn ->
          let _ = snk.stop r0 in
          reraise exn
      in
      try loop r0 s0
      with exn ->
        src.stop s0;
        let _r' = snk.stop r0 in
        reraise exn)
