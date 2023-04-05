open Why3

type controller

type id = int

val session : controller -> Session_itp.session

val strategies : controller -> string list

val reload : controller -> unit

val unproved_tasks : controller -> Session_itp.proofNodeID list

val replay : controller -> unit Lwt.t

val reset : controller -> unit

val save : controller -> unit

val run_strategy : controller -> string -> Session_itp.proofNodeID -> unit Lwt.t

val from_file : mkdir:bool -> string -> controller * string