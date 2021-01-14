open Why3.Itp_server
open Why3.Itp_communication

module Scheduler = struct
  let multiplier = 1
  let blocking = false

  (* the private list of functions to call on idle, sorted higher
         priority first. *)
  let idle_handler : (int * (unit -> bool)) list ref = ref []

  (* [insert_idle_handler p f] inserts [f] as a new function to call
         on idle, with priority [p] *)
  let insert_idle_handler p f =
    let rec aux l =
      match l with
      | [] -> [p,f]
      | (p1,_) as hd :: rem ->
         if p > p1 then (p,f) :: l else hd :: aux rem
    in
    idle_handler := aux !idle_handler

  (* the private list of functions to call on timeout, sorted on
         earliest trigger time first. *)
  let timeout_handler : (float * float * (unit -> bool)) list ref = ref []

  (* [insert_timeout_handler ms t f] inserts [f] as a new function to call
         on timeout, with time step of [ms] and first call time as [t] *)
  let insert_timeout_handler ms t f =
    let rec aux l =
      match l with
      | [] -> [ms,t,f]
      | (_,t1,_) as hd :: rem ->
         if t < t1 then (ms,t,f) :: l else hd :: aux rem
    in
    timeout_handler := aux !timeout_handler

  (* public function to register a task to run on idle *)
  let idle ~(prio:int) f = insert_idle_handler prio f

  (* public function to register a task to run on timeout *)
  let timeout ~ms f =
    assert (ms > 0);
    let ms = float ms /. 1000.0 in
    let time = Unix.gettimeofday () in
    insert_timeout_handler ms (time +. ms) f

end

module Protocol = struct
  let notifications = ref []

  let notify n = notifications := n :: ! notifications

  let get_notifications () =
    let l = !notifications in notifications := []; List.rev l

  let requests = ref []

  let push_request r =
    requests := r :: !requests

  let get_requests () =
    let l = !requests in requests := []; List.rev l
end

open Why3

let ( >>= ) o f = Result.bind o f

type prover = [%import: Why3.Whyconf.prover] [@@deriving yojson]
type prover_upgrade_policy = [%import: Why3.Whyconf.prover_upgrade_policy] [@@deriving of_yojson]

type node_ID = [%import: Why3.Itp_communication.node_ID] [@@deriving yojson]

type next_unproved_node_strat = [%import: Why3.Itp_communication.next_unproved_node_strat] [@@deriving of_yojson]

(* type position = [%import: Why3.Loc.position] [@@deriving to_yojson] *)


module Loc = struct
  type position = Why3.Loc.position

  type internal_pos = string * int * int * int [@@deriving yojson]

  let position_to_yojson pos = internal_pos_to_yojson (Why3.Loc.get pos)
  let position_of_yojson pos = internal_pos_of_yojson pos >>= fun (s, a, b, c) -> Ok(Why3.Loc.user_position s a b c)
end

type node_type = [%import: Why3.Itp_communication.node_type] [@@deriving to_yojson]
type prover_answer = [%import: Why3.Call_provers.prover_answer] [@@deriving to_yojson]
type process_status = [%import: Unix.process_status] [@@deriving to_yojson]



module Call_provers = struct
  type prover_result = Why3.Call_provers.prover_result

  type prover_result_fake = {
    pr_answer : prover_answer;
    (** The answer of the prover on the given task *)
    pr_status : process_status;
    (** The process exit status *)
    pr_output : string;
    (** The output of the prover currently stderr and stdout *)
    pr_time   : float;
    (** The time taken by the prover *)
    pr_steps  : int;
    (** The number of steps taken by the prover (-1 if not available) *)
  } [@@deriving to_yojson]

  let prover_result_to_yojson (pr : prover_result) =
    prover_result_fake_to_yojson ({
      pr_answer = pr.pr_answer;
      pr_status = pr.pr_status;
      pr_output = pr.pr_output;
      pr_time = pr.pr_time;
      pr_steps = pr.pr_steps;
    } : prover_result_fake)
end


module Facade = struct
type proof_attempt_status = Why3.Controller_itp.proof_attempt_status

type proof_attempt_status_internal =
  | Undone   (** prover was never called *)
  | Scheduled (** external proof attempt is scheduled *)
  | Running (** external proof attempt is in progress *)
  | Done of Call_provers.prover_result (** external proof done *)
  | Interrupted (** external proof has never completed *)
  | Detached (** parent goal has no task, is detached *)
  | Uninstalled of prover (** prover is uninstalled *)
  | UpgradeProver of prover (** prover is upgraded *)
  | Removed of prover (** prover has been removed or upgraded *)
  [@@deriving to_yojson]

  exception NotProofAttempt

  let proof_attempt_status_to_yojson (pas : proof_attempt_status) =
    proof_attempt_status_internal_to_yojson (match pas with
    | Undone -> Undone
    | Scheduled -> Scheduled
    | Running -> Running
    | Done res -> Done res
    | Interrupted -> Interrupted
    | Detached -> Detached
    | Uninstalled prvr -> Uninstalled prvr
    | UpgradeProver prvr -> UpgradeProver prvr
    | Removed prvr -> Removed prvr
    | _ -> raise NotProofAttempt)

end

type resource_limit = [%import: Why3.Call_provers.resource_limit] [@@deriving to_yojson]

type update_info = [%import: Why3.Itp_communication.update_info
  [@with Call_provers.resource_limit := resource_limit;
         Controller_itp.proof_attempt_status := Facade.proof_attempt_status
  ]
] [@@deriving to_yojson]

type transformation = [%import: Why3.Itp_communication.transformation] [@@deriving to_yojson]
type strategy = [%import: Why3.Itp_communication.strategy] [@@deriving to_yojson]
type global_information = [%import: Why3.Itp_communication.global_information] [@@deriving to_yojson]

type color = [%import: Why3.Itp_communication.color] [@@deriving to_yojson]

type fformat = [%import: Why3.Env.fformat] [@@deriving to_yojson]

type message_notification = [%import: Why3.Itp_communication.message_notification] [@@deriving to_yojson]

type notification = [%import: Why3.Itp_communication.notification
  [@with Env.fformat := fformat]
] [@@deriving to_yojson]
type notification_list = notification list [@@deriving to_yojson]

type request = [%import: Why3.Itp_communication.ide_request
  [@with Whyconf.prover := prover;
         Whyconf.prover_upgrade_policy := prover_upgrade_policy
  ]]
  [@@deriving of_yojson]

module S = Make (Scheduler) (Protocol)
open Lwt

let rec handle_incoming input = Lwt_io.read_line_opt input >>= fun line ->
  match line with
  | Some line ->
    let _req = request_of_yojson (Yojson.Safe.from_string line) in
    begin match _req with
    | Ok(req) ->
      Protocol.push_request req ;
      handle_incoming input
    | Error(_err) -> Lwt.return ()
    end
  | None -> Lwt.return ()


(* For now poll. In the future use a stream to push notifacations directly *)
let rec handle_outgoing out = Lwt_unix.sleep 0.1 >>= fun _ ->
  let _notifs = Protocol.get_notifications () in

  handle_outgoing out

let main_loop input output =
  Lwt.pick [handle_incoming input; handle_outgoing output]

let files : string Queue.t = Queue.create ()

let _ =
(*   let cli_opts = [] in
  let usage_str = "" in
  let config, _base_config, env =
      (* Can this be ditched entirely? *)
      Whyconf.Args.initialize cli_opts (fun f -> Queue.add f files) usage_str
    in
    let dir =
      try
        Server_utils.get_session_dir ~allow_mkdir:true files
      with Invalid_argument s ->
        Format.eprintf "Error: %s@." s;
        Whyconf.Args.exit_with_usage cli_opts usage_str
    in
    S.init_server config env dir;
    Queue.iter (fun f -> Protocol.push_request (Add_file_req f)) files; *)
    Lwt_main.run (main_loop Lwt_io.stdin Lwt_io.stdout);
    ()
