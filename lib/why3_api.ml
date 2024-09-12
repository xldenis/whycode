open Why3
module S = Sched.Lwt_S
module C = Controller_itp.Make (S)
open Controller_itp

let string_of_task (task : Task.task) tables =
  (* let task, tables = get_task_name_table d.cont.controller_session id in *)
  let pr = tables.Trans.printer in
  let apr = tables.Trans.aprinter in
  let module P =
    (val Pretty.create
           ~print_ext_any:(fun id -> id)
           ~do_forget_all:false ~shorten_axioms:true ~show_uses_clones_metas:false pr apr pr pr)
  in
  Pp.string_of P.print_sequent task

let call_one_prover (c : Controller_itp.controller) (p, timelimit, memlimit, steplimit) ~callback
    ~notification g =
  let main = Whyconf.get_main c.controller_config in
  let timelimit = Option.value ~default:(Whyconf.timelimit main) timelimit in
  let memlimit = Option.value ~default:(Whyconf.memlimit main) memlimit in
  let steplimit = Option.value ~default:0 steplimit in

  let limit =
    { Call_provers.limit_time = timelimit; limit_mem = memlimit; limit_steps = steplimit }
  in

  C.schedule_proof_attempt c g p ~limit ~callback ~notification

let run_strategy_on_goal c id strat ~notification ~finalize =
  let open Strategy in
  let open Controller_itp in
  let rec exec_strategy pc (mem : int ref list) (strat : Strategy.instruction array) g =
    let rec halt mem =
      match mem with
      | m :: tl ->
          m := !m - 1;
          if !m <= 0 then halt tl
      | [] -> finalize ()
    in
    if pc < 0 || pc >= Array.length strat then halt mem
    else begin
      match Array.get strat pc with
      | Icall_prover is ->
          let callback _panid res =
            match res with
            | UpgradeProver _ | Scheduled | Running -> (* nothing to do yet *) ()
            | Done { Call_provers.pr_answer = Call_provers.Valid; _ } ->
                (* proof succeeded, nothing more to do *)
                C.interrupt_proof_attempts_for_goal c g;
                halt mem
            | Interrupted -> halt mem
            | Done _ | InternalFailure _ ->
                (* proof did not succeed, goto to next step *)
                exec_strategy (pc + 1) mem strat g
            | Undone | Detached | Uninstalled _ | Removed _ ->
                (* should not happen *)
                assert false
          in
          List.iter (fun i -> call_one_prover c i ~callback ~notification g) is
      | Itransform (trname, pcsuccess) ->
          let callback ntr =
            match ntr with
            | TSfatal (_, _) -> halt mem
            | TSfailed _e ->
                (* transformation failed *)
                exec_strategy (pc + 1) mem strat g
            | TSscheduled -> ()
            | TSdone tid ->
                let sub_tasks = Session_itp.get_sub_tasks c.controller_session tid in
                let children = ref (List.length sub_tasks) in
                List.iter (fun g -> exec_strategy pcsuccess (children :: mem) strat g) sub_tasks
          in
          C.schedule_transformation c g trname [] ~callback ~notification
      | Igoto pc -> exec_strategy pc mem strat g
    end
  in
  exec_strategy 0 [] strat id

let add_file_to_session cont file =
  let dir = Session_itp.get_dir cont.controller_session in
  let fn = Sysutil.relativize_filename dir file in

  try
    let _ = Session_itp.find_file_from_path cont.controller_session fn in
    ()
  with Not_found -> if Sys.file_exists file then Controller_itp.add_file cont file

let get_goal_loc (task : Task.task) : Loc.position option =
  let location = try (Task.task_goal_fmla task).t_loc with Task.GoalNotFound -> None in
  match location with Some _ -> location | None -> (Task.task_goal task).pr_name.id_loc
    