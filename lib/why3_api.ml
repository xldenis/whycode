open Why3
module S = Sched.Lwt_S
module C = Controller_itp.Make (S)
open Controller_itp

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
      | Icall_prover (p, timelimit, memlimit, steplimit) ->
          let main = Whyconf.get_main c.controller_config in
          let timelimit = Opt.get_def (Whyconf.timelimit main) timelimit in
          let memlimit = Opt.get_def (Whyconf.memlimit main) memlimit in
          let steplimit = Opt.get_def 0 steplimit in
          let callback _panid res =
            match res with
            | UpgradeProver _ | Scheduled | Running -> (* nothing to do yet *) ()
            | Done { Call_provers.pr_answer = Call_provers.Valid; _ } ->
                (* proof succeeded, nothing more to do *)
                halt mem
            | Interrupted -> halt mem
            | Done _ | InternalFailure _ ->
                (* proof did not succeed, goto to next step *)
                exec_strategy (pc + 1) mem strat g
            | Undone | Detached | Uninstalled _ | Removed _ ->
                (* should not happen *)
                assert false
          in
          let limit =
            { Call_provers.limit_time = timelimit; limit_mem = memlimit; limit_steps = steplimit }
          in
          C.schedule_proof_attempt c g p ~limit ~callback ~notification
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

          begin
            match Session_itp.get_transformation c.controller_session g trname [] with
            | tid -> callback (TSdone tid)
            | exception Not_found -> C.schedule_transformation c g trname [] ~callback ~notification
          end
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
