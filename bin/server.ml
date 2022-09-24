open Why3

module RunTransformationNotification = struct
  let uri_of_yojson j =
    try Ok (Lsp.Types.DocumentUri.t_of_yojson j) with _ -> Error "could not parse uri"

  type t = {
    command : string;
    node : int;
    uri : Lsp.Types.DocumentUri.t; [@of_yojson uri_of_yojson]
  }
  [@@deriving of_yojson] [@@yojson.allow_extra_fields]

  let create com node uri = { command = com; node; uri }
end

module ResetSessionNotification = struct
  let uri_of_yojson j =
    try Ok (Lsp.Types.DocumentUri.t_of_yojson j) with _ -> Error "could not parse uri"

  type t = { uri : Lsp.Types.DocumentUri.t; [@of_yojson uri_of_yojson] dummy : bool }
  [@@deriving of_yojson] [@@yojson.allow_extra_fields]
end

(* Temporary, we should instead probably have a 'TreeChangeNotification' which bundles updates *)
module NewNodeNotification = struct
  let uri_to_yojson j = Lsp.Types.DocumentUri.yojson_of_t j

  type t = { uri: Lsp.Uri.t [@to_yojson uri_to_yojson]; id : int; parent_id : int; name : string; proved: bool }
  (* also type and detached *)
  [@@deriving to_yojson] [@@yojson.allow_extra_fields]

  let to_jsonrpc n : Jsonrpc.Message.notification =
    {
      method_ = "proof/addTreeNode";
      id = ();
      params = Some (Jsonrpc.Message.Structured.of_json (to_yojson n));
    }
end

module UpdateNodeNotification = struct
  type info =
    | Proved of bool
    (*todo use real type *)
    | NameChange of string
    | StatusChange of unit
  [@@deriving to_yojson]

  type t = { id : int; info : info } [@@deriving to_yojson]

  let of_notif (n : Itp_communication.notification) : t =
    match n with
    | Node_change (id, info) -> begin
        match info with
        | Proved b -> { id; info = Proved b }
        | Name_change s -> { id; info = NameChange s }
        | Proof_status_change _ -> { id; info = StatusChange () }
      end
    | _ -> failwith "of_notif: wrong notification"

  let to_jsonrpc n : Jsonrpc.Message.notification =
    {
      method_ = "proof/changeTreeNode";
      id = ();
      params = Some (Jsonrpc.Message.Structured.of_json (to_yojson n));
    }
end

module DeleteNodeNotification = struct
  type t = { id : int }
    [@@deriving to_yojson]

  let to_jsonrpc n : Jsonrpc.Message.notification =
  { method_ = "proof/removeTreeNode"; id = (); params = Some(Jsonrpc.Message.Structured.of_json (to_yojson n))}
end

let loc_to_range loc =
  let _, l1, c1, l2, c2 = Loc.get loc in
  Lsp.Types.Range.create (* why is this necessary?       ----------v *)
    ~start:(Lsp.Types.Position.create ~line:(l1 - 1) ~character:c1)
    ~end_:(Lsp.Types.Position.create ~line:(l2 - 1) ~character:c2)

let locs_to_range s e =
  let _, l1, c1, _, _ = Loc.get s in
  let _, _, _, l2, c2 = Loc.get e in
  Lsp.Types.Range.create (* why is this necessary?       ----------v *)
    ~start:(Lsp.Types.Position.create ~line:(l1 - 1) ~character:c1)
    ~end_:(Lsp.Types.Position.create ~line:(l2 - 1) ~character:c2)

let warn loc message =
  Lsp.Types.Diagnostic.create () ~range:(loc_to_range loc) ~severity:Warning ~source:"Why3" ~message

let error loc message =
  Lsp.Types.Diagnostic.create () ~range:(loc_to_range loc) ~severity:Error ~source:"Why3" ~message
(* TODO
   1. Kill whole server when one of the various async tasks dies
   2. Reduce the need to spawn as much?
*)

module TaskTree = struct
  open Itp_communication

  type task_info = {
    task : string;
    locations : (Loc.position * color) list;
    goal : Loc.position option;
    lang : string;
  }

  type node_info = { name : string; proved : bool }

  (* seperate out the node body from the tree itself *)
  type task_tree = Node of node_ID * node_info * task_info option * task_tree list

  let rec add_tree parent_id tree self =
    match self with
    | Node (id, info, task, children) ->
        if parent_id = id then Node (id, info, task, tree :: children)
        else Node (id, info, task, List.map (add_tree parent_id tree) children)

  let rec remove_tree node_id self =
    match self with
    | Node (id, nm, task, children) ->
        if node_id = id then None
        else
          let check (Node (id, _, _, _) as n) = if id = node_id then None else remove_tree id n in
          Some (Node (id, nm, task, List.filter_map check children))

  let rec update_node node_id f self =
    let (Node (id, nm, task, children)) = self in
    if node_id = id then f self else Node (id, nm, task, List.map (update_node node_id f) children)

  (* let to_list self : (node_ID * node_info * task_info option) list =
     let rec worker acc self =
       let (Node (id, info, task, children)) = self in
       List.fold_left worker ((id, info, task) :: acc) children
     in
     worker [] self *)

  let to_list self : (node_ID * node_info * task_info option) list =
    let rec worker acc self =
      let (Node (id, info, task, children)) = self in
      match children with [] -> (id, info, task) :: acc | _ -> List.fold_left worker acc children
    in
    worker [] self
end

open Itp_communication
open TaskTree

type session_data = Lsp.Uri.t * ide_request list ref * task_tree option ref

let session_path (s : session_data) : Lsp.Uri.t =
  let (p, _, _) = s in p

let send_req (s : session_data) req =
  let _, r, _ = s in
  r := req :: !r

let add_node (s : session_data) p_id n =
  let _, _, t = s in
  match !t with None -> t := Some n | Some tr -> t := Some (add_tree p_id n tr)

let remove_node (s : session_data) id : unit =
  let _, _, t = s in
  match !t with
  | None -> failwith "cannot remove from empty tree"
  | Some tr -> t := remove_tree id tr

let edit_node (s : session_data) id f : unit =
  let _, _, t = s in
  match !t with
  | None -> failwith "cannot edit empty tree"
  | Some tr -> t := Some (update_node id f tr)

let get_tasks (s : session_data) : (node_ID * node_info * task_info) list =
  let _, _, t = s in
  match !t with
  | None -> []
  | Some tr ->
      List.filter_map
        (fun (id, nm, t) -> match t with Some t -> Some (id, nm, t) | None -> None)
        (to_list tr)

open Linol_lwt

module type SERVER = sig
  val init_server : ?send_source:bool -> Whyconf.config -> Env.env -> string -> unit
end

(* let new_node_notif () : unit Jsonrpc.Message.t =
   Jsonrpc.Message.create () "newNode" ()
*)
let notif_str n =
  match n with
  | Message _ -> "Message"
  | Initialized _ -> "Initialized"
  | Saved -> "Saved"
  | New_node _ -> "New_node"
  | Node_change _ -> "Node_change"
  | Remove _ -> "Remove"
  | Next_Unproven_Node_Id _ -> "Next_Unproven_Node_Id"
  | Saving_needed _ -> "Saving_needed"
  | Dead _ -> "Dead"
  | Task _ -> "Task"
  | File_contents _ -> "File_contents"
  | Source_and_ce _ -> "Source_and_ce"
  | Ident_notif_loc _ -> "Ident_notif_loc"
  | Reset_whole_tree -> "Reset_whole_tree"

let message_notif_level m =
  match m with
  | Proof_error _ -> MessageType.Error
  | Transf_error _ -> MessageType.Error
  | Strat_error _ -> MessageType.Error
  | Replay_Info _ -> MessageType.Info
  | Query_Info _ -> MessageType.Info
  | Query_Error _ -> MessageType.Error
  | Information _ -> MessageType.Info
  | Task_Monitor _ -> MessageType.Info
  | Parse_Or_Type_Error _ -> MessageType.Error
  | File_Saved _ -> MessageType.Info
  | Error _ -> MessageType.Error
  | Open_File_Error _ -> MessageType.Error

(*
  Build a map containing the unsolved goals of every file who's spans are found in the sessions
*)
let get_unsolved_tasks (srvr : session_data) : (string, Diagnostic.t list) Hashtbl.t =
  let tasks = get_tasks srvr in

  List.fold_left (fun acc (_, info, t) ->
    if info.proved then
      acc
    else
      match t.goal with
      | Some g -> begin let (file, _, _, _, _) = Loc.get g in
          let prev : _ option = Hashtbl.find_opt acc file in
          let next  = (error g info.name) :: Option.value prev ~default:[] in
          Hashtbl.replace acc file next;
          acc
        end
      | None -> acc
  ) (Hashtbl.create 10) tasks

let log_info n msg = n#send_log_msg ~type_:MessageType.Info msg

(* Publish a batch of diagnostics for a set of files *)
let send_all_diags (notify_back : Jsonrpc2.notify_back) (diags: (string, Diagnostic.t list) Hashtbl.t) =
  Hashtbl.fold (fun file diags lwt ->
    notify_back#set_uri (DocumentUri.of_path file);
    Lwt.(<&>) lwt (notify_back#send_diagnostic diags)
  ) diags Lwt.return_unit

let handle_notification (s : session_data) (notify_back : Jsonrpc2.notify_back) (notif : notification) =
  let* _ = log_info notify_back (Format.asprintf "%a" print_notify notif) in
  match notif with
  | Message m ->
      let* _ =
        notify_back#send_log_msg ~type_:(message_notif_level m) (Format.asprintf "%a" print_msg m)
      in

      begin match m with
      | Parse_Or_Type_Error (s, e, msg) ->
          notify_back#send_diagnostic
            [
              Lsp.Types.Diagnostic.create () ~range:(locs_to_range s e) ~severity:Error
                ~source:"Why3" ~message:msg;
            ]
      | _ -> return ()
      end

  | Initialized _ -> return ()
  | Saved -> return ()
  (* | Dead _ -> IO_lwt.failwith "Server Died" *)
  | New_node (id, par_id, ty, nm, proved) -> (
      let n = Node (id, { name = nm; proved }, None, []) in
      add_node s par_id n;
      (* Ask for the task of every 'goal' node so that we can get the
         spans of the unsolved goals *)
      let diags = get_unsolved_tasks s in
      let* _ = send_all_diags notify_back diags in
      let* _ =
        notify_back#send_notification
          (UnknownNotification
             (NewNodeNotification.to_jsonrpc { uri = (session_path s); id; parent_id = par_id; name = nm; proved }))
      in
      match ty with
      | NGoal ->
          send_req s (Get_task (id, false, true));
          return ()
      | _ -> return ())
  | Task (id, task, locs, goal, lang) ->
      let task_info = { task; locations = locs; goal; lang } in

      edit_node s id (fun node ->
          let (Node (id, nm, _, children)) = node in
          Node (id, nm, Some task_info, children));
      let diags = get_unsolved_tasks s in
      send_all_diags notify_back diags
  | Node_change (id, upd) ->
      let* _ =
        notify_back#send_notification
          (UnknownNotification
             (UpdateNodeNotification.to_jsonrpc (UpdateNodeNotification.of_notif notif)))
      in

      edit_node s id (fun node ->
          let (Node (id, info, task, children)) = node in
          match upd with
          | Proved b ->
              let info = { info with proved = b } in
              Node (id, info, task, children)
          | Name_change nm' ->
              let info = { info with name = nm' } in
              Node (id, info, task, children)
          | _ -> node);
      (* Do some sort of coalescing to avoid spamming diagnostics *)
      let diags = get_unsolved_tasks s in
      send_all_diags notify_back diags
  | Remove id ->
      remove_node s id;
      let* _ =
        notify_back#send_notification
          (UnknownNotification
             (DeleteNodeNotification.to_jsonrpc { id }))
      in

      let diags = get_unsolved_tasks s in
      send_all_diags notify_back diags
  | Reset_whole_tree ->
      let _, _, t = s in
      t := None;
      let* _ =
        notify_back#send_notification
          (UnknownNotification
             (DeleteNodeNotification.to_jsonrpc { id = 0 }))
      in

      return ()
  (*
      | Next_Unproven_Node_Id _ -> IO_lwt.failwith "Next_Unproven_Node_Id"
      | Saving_needed _ -> IO_lwt.failwith "Saving_needed"
      | File_contents _ -> IO_lwt.failwith "File_contents"
      | Source_and_ce _ -> IO_lwt.failwith "Source_and_ce"
      | Ident_notif_loc _ -> IO_lwt.failwith "Ident_notif_loc"
  *)
  | _ -> return ()

let mk_server (notify_back : Jsonrpc2.notify_back) config env session_path : session_data =
  let open Itp_server in
  let requests = ref [] in
  let tree = ref None in

  let module P : Protocol = struct
    let notify msg = spawn (fun () -> handle_notification (Lsp.Uri.of_path session_path, requests, tree) notify_back msg)

    let get_requests () =
      let l = !requests in
      requests := [];
      List.rev l
  end in
  let module S : Controller_itp.Scheduler = struct
    let multiplier = 1
    let blocking = false

    (* [insert_idle_handler p f] inserts [f] as a new function to call
           on idle, with priority [p] *)
    let insert_idle_handler _ f =
      let rec promise () =
        let* () = Lwt_unix.sleep 0.125 in
        if f () then promise () else return ()
      in
      Linol_lwt.spawn (fun () -> promise ())

    (* [insert_timeout_handler ms t f] inserts [f] as a new function to call
           on timeout, with time step of [ms] and first call time as [t] *)
    let insert_timeout_handler ms (t : float) (f : unit -> bool) =
      let time = Unix.gettimeofday () in
      let sleep = t -. time in

      spawn (fun () ->
          let* () = Lwt_unix.sleep sleep in
          let rec promise () =
            if f () then
              let* () = Lwt_unix.sleep ms in
              promise ()
            else return ()
          in
          promise ())

    (* public function to register a task to run on idle *)
    let idle ~(prio : int) f = insert_idle_handler prio f

    (* public function to register a task to run on timeout *)
    let timeout ~ms f =
      assert (ms > 0);
      let ms = float ms /. 1000.0 in
      let time = Unix.gettimeofday () in
      insert_timeout_handler ms (time +. ms) f
  end in
  let module Server = Make (S) (P) in
  let _ = (module Server : SERVER) in
  Server.init_server config env session_path;
  (Lsp.Uri.of_path session_path, requests, tree)

type session_id = string

class why_lsp_server =
  object (self)
    inherit Linol_lwt.Jsonrpc2.server
    val mutable config = Obj.magic ()
    val mutable env = Obj.magic ()

    initializer
    let cli_opts = [] in
    let usage_str = "" in
    let config', env' =
      (* Can this be ditched entirely? *)
      Whyconf.Args.initialize cli_opts (fun _ -> ()) usage_str
    in
    config <- config';
    env <- env'

    val file_to_session : (Lsp.Types.DocumentUri.t, session_id) Hashtbl.t = Hashtbl.create 32
    val sessions : (session_id, session_data) Hashtbl.t = Hashtbl.create 32
    val buffers : (Lsp.Types.DocumentUri.t, unit) Hashtbl.t = Hashtbl.create 32
    method! private config_code_action_provider = `CodeActionOptions (CodeActionOptions.create ())

    method private get_server (uri : Lsp.Types.DocumentUri.t) =
      let sess_id = self#find_session uri in
      let cont = Hashtbl.find_opt sessions sess_id in

      match cont with
      | Some cont -> return cont
      | None -> failwith "ruh-roh session not properly initialized"

    method private _on_doc ~(notify_back : Linol_lwt.Jsonrpc2.notify_back)
        (uri : Lsp.Types.DocumentUri.t) (_contents : string) =
      let sess_id = self#find_session uri in
      let cont = Hashtbl.find_opt sessions sess_id in

      let srvr =
        match cont with
        | Some cont -> cont
        | None ->
            let s = mk_server notify_back config env sess_id in
            spawn (fun () -> log_info notify_back "Adding session");
            Hashtbl.add sessions sess_id s;
            s
      in
      send_req srvr Reload_req;
      spawn (fun () ->
          let* _ = Lwt_unix.sleep 0.5 in
          send_all_diags notify_back (get_unsolved_tasks srvr)
      );
      return ()

    method private find_session (uri : Lsp.Types.DocumentUri.t) : session_id =
      match Hashtbl.find_opt file_to_session uri with
      | Some sess_id -> sess_id
      | None ->
          let f = Queue.create () in
          Queue.push (DocumentUri.to_path uri) f;
          let sess = Server_utils.get_session_dir ~allow_mkdir:true f in
          Hashtbl.replace file_to_session uri sess;
          sess

    method on_notif_doc_did_open ~notify_back d ~content = self#_on_doc ~notify_back d.uri content

    method on_notif_doc_did_change ~notify_back d _c ~old_content:_old ~new_content =
      self#_on_doc ~notify_back d.uri new_content

    method on_notif_doc_did_close ~notify_back d =
      Hashtbl.remove buffers d.uri;
      notify_back#send_diagnostic []

    method private on_did_save_notif ~notify_back (n : DidSaveTextDocumentParams.t) =
      let* srvr = self#get_server n.textDocument.uri in
      send_req srvr Save_req;
      return ()

    method! on_req_code_action ~notify_back ~id c =
      let* srvr = self#get_server c.textDocument.uri in

      let tasks = get_tasks srvr in
      let rec search tasks =
        match tasks with
        | ((_, i, t) as n) :: ts -> begin
            match t.goal with
            | Some g ->
                (* Send multi-line diagnostics! *)
                let _, l1, c1, l2, c2 = Loc.get g in

                spawn (fun () -> log_info notify_back (
                  Format.asprintf "finding actions for [%d, %d] -> [%d, %d],  goal: [%d, %d] -> [%d, %d]" c.range.start.line c.range.start.character c.range.end_.line c.range.end_.character l1 c1 l2 c2)
                );

                if
                  (not i.proved)
                  && c.range.start.line = l1
                  && c.range.end_.line = l2
                  && c1 <= c.range.start.character && c.range.end_.character <= c2
                then Some n
                else search ts
            | None -> search ts
          end
        | [] -> None
      in
      let found_task = search tasks in
      let mk_command trans id =
        Command.create ~title:trans ~command:"why3.runTransformation"
          ~arguments:[ DocumentUri.yojson_of_t c.textDocument.uri; `Int id; `String trans ]
          ()
      in
      match found_task with
      | Some (id, _, _) ->
          let ss = Whyconf.get_strategies config in
          let open Wstdlib in
          let actions =
            Mstr.fold_left
              (fun acc k (_ : Whyconf.config_strategy) -> `Command (mk_command k id) :: acc)
              [] ss
          in
          return (Some (List.rev actions))
      | None -> return None

    (* TODO: ideally should be request but we don't have a meaninful response to give *)
    method private on_run_command_notif ~notify_back (n : RunTransformationNotification.t) =
      let* _ = log_info notify_back "Running command" in
      let* srvr = self#get_server n.uri in
      send_req srvr (Command_req (n.node, n.command));
      return ()

    method! on_notification_unhandled ~notify_back notif =
      let open Lsp.Import in
      match notif with
      | Lsp.Client_notification.UnknownNotification m -> begin
          match m.method_ with
          | "proof/runTransformation" -> begin
              let params = Json.message_params m RunTransformationNotification.of_yojson in

              match Result.join params with
              | Ok p -> self#on_run_command_notif ~notify_back p
              | Error e -> failwith e
            end
          | "proof/resetSession" -> begin
              let params = Json.message_params m ResetSessionNotification.of_yojson in
              match Result.join params with
              | Ok p ->
                  let* srvr = self#get_server p.uri in
                  send_req srvr Reset_proofs_req;
                  send_req srvr Reload_req;
                  return ()
              | Error e -> failwith e
            end
          | _ -> return ()
        end
      | Lsp.Client_notification.DidSaveTextDocument n -> self#on_did_save_notif ~notify_back n
      | _ -> return ()
  end
