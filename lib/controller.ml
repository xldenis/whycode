open Why3
open Controller_itp
open Session_itp
open Why3_api
open Wstdlib

type id = int [@@deriving to_yojson]

type controller = {
  controller : Controller_itp.controller;
  pan_to_id : id Hpan.t;
  pn_to_id : id Hpn.t;
  tn_to_id : id Htn.t;
  th_to_id : id Ident.Hid.t;
  file_to_id : id Hfile.t;
  id_to_any : any Hint.t;
  mutable next_id : id;
}

let from_why (controller : Controller_itp.controller) : controller =
  let pan_to_id : id Hpan.t = Hpan.create 17 in
  let pn_to_id : id Hpn.t = Hpn.create 17 in
  let tn_to_id : id Htn.t = Htn.create 17 in
  let th_to_id : id Ident.Hid.t = Ident.Hid.create 7 in
  let file_to_id : id Hfile.t = Hfile.create 3 in
  let id_to_any : any Hint.t = Hint.create 17 in
  { controller; pan_to_id; pn_to_id; tn_to_id; th_to_id; file_to_id; next_id = 0; id_to_any }

let id_from_file c file = Hfile.find c.file_to_id (file_id file)
let id_from_th c th = Ident.Hid.find c.th_to_id (theory_name th)
let id_from_tn c tn = Htn.find c.tn_to_id tn
let id_from_pn c pn = Hpn.find c.pn_to_id pn
let id_from_pan c pan = Hpan.find c.pan_to_id pan

let new_id c =
  let n = c.next_id in
  c.next_id <- c.next_id + 1;
  n

let id_from_any c any =
  try
    match any with
    | AFile file -> Hfile.find c.file_to_id (file_id file)
    | ATh th -> Ident.Hid.find c.th_to_id (theory_name th)
    | ATn tn -> Htn.find c.tn_to_id tn
    | APn pn -> Hpn.find c.pn_to_id pn
    | APa pan -> Hpan.find c.pan_to_id pan
  with Not_found ->
    let id = new_id c in
    Hint.replace c.id_to_any id any;
    begin
      match any with
      | AFile file -> Hfile.replace c.file_to_id (file_id file) id
      | ATh th -> Ident.Hid.replace c.th_to_id (theory_name th) id
      | ATn tn -> Htn.replace c.tn_to_id tn id
      | APn pn -> Hpn.replace c.pn_to_id pn id
      | APa pan -> Hpan.replace c.pan_to_id pan id
    end;
    id

exception PnNotFound of id

let any_from_id c id = Hint.find c.id_to_any id

let pn_from_id c id =
  try match Hint.find c.id_to_any id with APn pn -> pn | _ -> raise Not_found
  with Not_found -> raise (PnNotFound id)

let session (c : controller) : Session_itp.session = c.controller.controller_session

let strategies (c : controller) : string list =
  let open Wstdlib in
  Hstr.fold (fun s _ acc -> s :: acc) c.controller.controller_strategies []

let reload (c : controller) : unit =
  let _ = Controller_itp.reload_files c.controller in
  ()

let unproved_tasks (c : controller) : id list =
  let open Session_itp in
  let session = session c in

  let x =
    Session_itp.fold_all_session session
      (fun acc any ->
        match any with
        | Session_itp.APn id ->
            if get_transformations session id = [] && not (pn_proved session id) then
              id_from_any c any :: acc
            else acc
        | _ -> acc)
      []
  in

  let buf = Buffer.create 32 in
  let fmt = Format.formatter_of_buffer buf in
  Hint.iter (fun id a -> Format.fprintf fmt "%d %a\n" id fprintf_any a) c.id_to_any;
  Format.fprintf fmt "elems: %d %d" (List.length x) (Hint.length c.id_to_any);
  Format.pp_print_flush fmt ();
  (* raise (Omg (Buffer.contents buf)); *)
  x

let replay (c : controller) : unit Lwt.t =
  let promise, resolver = Lwt.wait () in
  C.replay ~valid_only:true ~obsolete_only:true c.controller
    ~callback:(fun _ _ -> ())
    ~notification:(fun _ -> ())
    ~final_callback:(fun _ _ -> Lwt.wakeup resolver ())
    ?use_steps:None ?filter:None ~any:None;
  promise

let save (c : controller) : unit = Session_itp.save_session (session c)

let reset (c : controller) =
  C.reset_proofs c.controller ~notification:(fun _ -> ()) ~removed:(fun _ -> ()) None

let get_goal_loc (task : Task.task) : Loc.position =
  let location = try (Task.task_goal_fmla task).t_loc with Task.GoalNotFound -> None in
  let location =
    match location with Some l -> l | None -> Option.get (Task.task_goal task).pr_name.id_loc
  in

  location

type task = { expl : string; loc : Loc.position; proved : bool }

let task c (id : id) =
  let session = session c in
  let id = pn_from_id c id in
  let task = get_task session id in
  let location = get_goal_loc task in
  let msg = get_proof_expl session id in
  let msg = if msg = "" then (get_proof_name session id).id_string else msg in
  let proved = Session_itp.pn_proved c.controller.controller_session id in
  { expl = msg; loc = location; proved }

let task_body c id =
  let id = pn_from_id c id in
  let task, tables = Session_itp.get_task_name_table c.controller.controller_session id in
  string_of_task task tables

type tree_elem = { expl : string; proved : bool; id : id; parent : id option }
[@@deriving to_yojson]

let any_to_elem c session any =
  let id = id_from_any c any in
  let parent = Option.map (id_from_any c) (Session_itp.get_any_parent session any) in
  let proved = Session_itp.any_proved c.controller.controller_session any in
  let expl =
    match any with
    | AFile file -> Sysutil.basename (file_path file)
    | ATh th -> (theory_name th).id_string
    | ATn tn -> get_transf_name session tn
    | APn _ -> (task c id).expl
    | APa pa ->
        let pa = get_proof_attempt_node session pa in
        Pp.string_of Whyconf.print_prover pa.prover
  in
  { id; parent; proved; expl }

let file_tree_as_list (c : controller) : (file * tree_elem list) list =
  let session = session c in
  let files = get_files session in

  Hfile.fold
    (fun _ f acc ->
      let elems =
        fold_all_any session
          (fun acc any ->
            let should_skip = match any with ATh th -> theory_goals th = [] | _ -> false in
            if should_skip then acc else any_to_elem c session any :: acc)
          [] (AFile f)
      in
      (f, elems) :: acc)
    files []

let run_strategy (c : controller) (strat : string) (id : id) : unit Lwt.t =
  let open Wstdlib in
  let promise, resolver = Lwt.wait () in
  let _, _, _, strat = Hstr.find c.controller.controller_strategies strat in
  run_strategy_on_goal c.controller (pn_from_id c id) strat
    ~notification:(fun _ -> ())
    ~finalize:(fun _ -> Lwt.wakeup resolver ());
  promise

let from_file ~mkdir config env (id : string) : controller * string =
  let files = Queue.create () in
  Queue.push id files;
  let dir = Server_utils.get_session_dir ~allow_mkdir:mkdir files in
  let ses = Session_itp.load_session dir in
  let cont = Controller_itp.create_controller config env ses in
  Server_utils.load_strategies cont;
  (* HACK: add mlcfg *)
  let why_file =
    if Filename.check_suffix id "rs" then Filename.chop_suffix id "rs" ^ "mlcfg" else id
  in
  add_file_to_session cont why_file;
  let cont = from_why cont in
  (cont, dir)
