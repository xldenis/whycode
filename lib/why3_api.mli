open Why3

module C : module type of Controller_itp.Make(Sched.Lwt_S)

val string_of_task : Why3.Task.task -> Why3.Trans.naming_table -> string
val call_one_prover : Why3.Controller_itp.controller ->
    Why3.Whyconf.prover * float option * int option * int option ->
    callback:(Why3.Session_itp.proofAttemptID ->
              Why3.Controller_itp.proof_attempt_status -> unit) ->
    notification:Why3.Session_itp.notifier ->
    Why3.Session_itp.proofNodeID -> unit
val run_strategy_on_goal : Why3.Controller_itp.controller ->
    Why3.Session_itp.proofNodeID ->
    Why3.Strategy.instruction array ->
    notification:Why3.Session_itp.notifier ->
    finalize:(unit -> unit) -> unit
val add_file_to_session : Why3.Controller_itp.controller -> string -> unit
val get_goal_loc : Task.task -> Loc.position option
