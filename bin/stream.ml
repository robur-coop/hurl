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
