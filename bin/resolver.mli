type resolver =
  [ `Happy of Happy_eyeballs_miou_unix.t | `System | `User of Httpcats.resolver ]

val v :
     Out.cfg
  -> Arg.happy_eyeballs option
  -> [ `System | `OCaml ]
  -> Dns.proto * Arg.nameserver list
  -> Happy_eyeballs_miou_unix.daemon option * resolver
