type 'a t =
  | Source : {
        init: unit -> 's
      ; pull: 's -> ('a * 's) option
      ; stop: 's -> unit
    }
      -> 'a t

let of_bqueue bqueue =
  let init () = bqueue in
  let stop _bqueue = () in
  let pull bqueue =
    match Bqueue.get bqueue with
    | Some value -> Some (value, bqueue)
    | None -> None
  in
  Source { init; stop; pull }
