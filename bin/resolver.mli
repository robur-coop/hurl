type resolver =
  [ `Happy of Happy_eyeballs_miou_unix.t | `System | `User of Httpcats.resolver ]

val setup : (Happy_eyeballs_miou_unix.daemon option * resolver) Cmdliner.Term.t
