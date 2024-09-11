type 'a t =
  | Source : {
        init: unit -> 's
      ; pull: 's -> ('a * 's) option
      ; stop: 's -> unit
    }
      -> 'a t

val of_bqueue : 'a Bqueue.t -> 'a t
