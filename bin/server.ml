open Util
open Why3
open Notifications

let warn loc message =
  Lsp.Types.Diagnostic.create () ~range:(loc_to_range loc) ~severity:Warning ~source:"Why3" ~message

let error loc message =
  Lsp.Types.Diagnostic.create () ~range:(loc_to_range loc) ~severity:Error ~source:"Why3" ~message
(* TODO
   1. Kill whole server when one of the various async tasks dies
   2. Reduce the need to spawn as much?
*)

open Itp_communication
open Task_tree
open Linol_lwt

module type SERVER = sig
  val init_server : ?send_source:bool -> Whyconf.config -> Env.env -> string -> unit
end

let log_info n msg = n#send_log_msg ~type_:MessageType.Info msg

(* Publish a batch of diagnostics for a set of files *)
let send_all_diags (notify_back : Jsonrpc2.notify_back)
    (diags : (string, Diagnostic.t list) Hashtbl.t) =
  spawn (fun () -> log_info notify_back (Format.asprintf "sending tasks"));

  Hashtbl.fold
    (fun file diags lwt ->
      notify_back#set_uri (DocumentUri.of_path file);
      Lwt.( <&> ) lwt (notify_back#send_diagnostic diags))
    diags Lwt.return_unit

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

let log_info n msg = n#send_log_msg ~type_:MessageType.Info msg

open Notifications
open Why3_manager

let send_unsolved_tasks (m : manager) (notify_back : Jsonrpc2.notify_back) : unit Lwt.t =
  let diags = Hashtbl.create 32 in
  Hashtbl.iter
    (fun k v -> Hashtbl.add diags k (List.map (fun (l, info) -> error l info.name) v))
    (unsolved_tasks m);

  send_all_diags notify_back diags

let handle_notification (m : manager) (s : session) (notify_back : Jsonrpc2.notify_back)
    (notif : notification) =
  let* _ = log_info notify_back (Format.asprintf "notification: %a" print_notify notif) in
  let tasks = get_tasks s in
  spawn (fun () -> log_info notify_back (Format.asprintf "sending %d tasks" (List.length tasks)));
  match notif with
  | Message m -> (
      let* _ =
        notify_back#send_log_msg ~type_:(message_notif_level m) (Format.asprintf "%a" print_msg m)
      in

      match m with
      | Parse_Or_Type_Error (s, e, msg) ->
          notify_back#send_diagnostic
            [
              Lsp.Types.Diagnostic.create () ~range:(locs_to_range s e) ~severity:Error
                ~source:"Why3" ~message:msg;
            ]
      | _ -> return ())
  | Initialized _ -> return ()
  | Saved -> return ()
  (* | Dead _ -> IO_lwt.failwith "Server Died" *)
  | New_node (id, par_id, ty, nm, proved) -> (
      (* let n = Node (id, { name = nm; proved }, None, []) in *)
      add_node s par_id id { name = nm; proved };
      (* Ask for the task of every 'goal' node so that we can get the
         spans of the unsolved goals *)
      let* _ = send_unsolved_tasks m notify_back in
      let* _ =
        notify_back#send_notification
          (UnknownNotification
             (NewNodeNotification.to_jsonrpc
                { uri = session_path s; id; parent_id = par_id; name = nm; proved }))
      in
      match ty with
      | NGoal ->
          send_req s (Get_task (id, false, true));
          return ()
      | _ -> return ())
  | Task (id, task, locs, goal, lang) ->
      let task_info = { task; locations = locs; goal; lang } in
      edit_node s id (fun (node, _) -> (node, Some task_info));
      send_unsolved_tasks m notify_back
  | Node_change (id, upd) ->
      let* _ =
        notify_back#send_notification
          (UnknownNotification
             (UpdateNodeNotification.to_jsonrpc
                (UpdateNodeNotification.of_notif (session_path s) notif)))
      in

      edit_node s id (fun (info, task) ->
          match upd with
          | Proved b ->
              let info = { info with proved = b } in
              (info, task)
          | Name_change nm' ->
              let info = { info with name = nm' } in
              (info, task)
          | _ -> (info, task));

      send_unsolved_tasks m notify_back
  | Remove id ->
      remove_node s id;
      let* _ =
        notify_back#send_notification
          (UnknownNotification (DeleteNodeNotification.to_jsonrpc { id; uri = session_path s }))
      in

      send_unsolved_tasks m notify_back
  | Reset_whole_tree ->
      reset_tree s;
      let* _ =
        notify_back#send_notification
          (UnknownNotification (DeleteNodeNotification.to_jsonrpc { id = 0; uri = session_path s }))
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

let mk_server (notify_back : Jsonrpc2.notify_back) config env manager session : unit =
  let open Itp_server in
  let module P : Protocol = struct
    let notify msg = spawn (fun () -> handle_notification manager session notify_back msg)
    let get_requests () = get_requests session
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
  spawn (fun () ->
      log_info notify_back
        (Format.asprintf "Starting server for %s" (DocumentUri.to_path (session_path session))));

  Server.init_server config env (DocumentUri.to_path (session_path session))

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

    val manager : manager = new_manager ()
    method! private config_code_action_provider = `CodeActionOptions (CodeActionOptions.create ())

    method private _on_doc ~(notify_back : Linol_lwt.Jsonrpc2.notify_back)
        (uri : Lsp.Types.DocumentUri.t) (_contents : string) =
      let sess =
        find_or_create_session manager uri (fun man sess ->
            spawn (fun () ->
                log_info notify_back
                  (Format.asprintf "Creating server for %s"
                     (DocumentUri.to_path (session_path sess))));
            mk_server notify_back config env man sess)
      in
      send_req sess (Add_file_req (DocumentUri.to_path uri));
      send_req sess Reload_req;
      spawn (fun () -> let* _ = Lwt_unix.sleep 0.5 in

                       send_unsolved_tasks manager notify_back);
      return ()

    method on_notif_doc_did_open ~notify_back d ~content = self#_on_doc ~notify_back d.uri content

    method on_notif_doc_did_change ~notify_back d _c ~old_content:_old ~new_content =
      self#_on_doc ~notify_back d.uri new_content

    method on_notif_doc_did_close ~notify_back d =
      (* TODO: stop server *)
      notify_back#send_diagnostic []

    method private on_did_save_notif ~notify_back (n : DidSaveTextDocumentParams.t) =
      let sess =
        find_or_create_session manager n.textDocument.uri (fun man sess ->
            mk_server notify_back config env man sess)
      in
      send_req sess Save_req;
      return ()

    method! on_req_code_action ~notify_back ~id c =
      (* let* srvr = self#get_server c.textDocument.uri in *)
      let mk_command trans id =
        Command.create ~title:trans ~command:"why3.runTransformation"
          ~arguments:[ DocumentUri.yojson_of_t c.textDocument.uri; `Int id; `String trans ]
          ()
      in
      let task = ref None in

      for_tasks_at manager c.textDocument.uri c.range (fun (id, node, _) ->
          spawn (fun () -> log_info notify_back (Format.asprintf "checking %d %s" id node.name));
          if not node.proved then (
            task := Some id;
            Stop)
          else Continue);

      match !task with
      | None -> return None
      | Some id ->
          let ss = Whyconf.get_strategies config in
          let open Wstdlib in
          let actions =
            Mstr.fold_left
              (fun acc k (_ : Whyconf.config_strategy) -> `Command (mk_command k id) :: acc)
              [] ss
          in
          return (Some (List.rev actions))

    (* TODO: ideally should be request but we don't have a meaninful response to give *)
    method private on_run_command_notif ~notify_back (n : RunTransformationNotification.t) =
      let* _ = log_info notify_back "Running command" in
      let sess =
        find_or_create_session manager n.uri (fun man sess ->
            mk_server notify_back config env man sess)
      in
      send_req sess (Command_req (n.node, n.command));
      return ()

    method! on_unknown_request ~notify_back ~id (meth : string) (msg : _) =
      begin
        match meth with
        | "proof/resolveSession" ->
            let msg = Option.get msg in
            begin
              match ResolveSessionRequest.of_yojson (Jsonrpc.Message.Structured.to_json msg) with
              | Ok p ->
                  let sess = Option.map session_path (find_session manager p.uri) in

                  begin
                    match sess with
                    | Some a -> return (`Assoc [ ("uri", DocumentUri.yojson_of_t a) ])
                    | None -> return (`Assoc [ ("uri", `Null) ])
                  end
              | Error e -> failwith e
            end
        | _ -> failwith "unhandled request"
      end

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
          | "proof/reloadSession" -> begin
              let params = Json.message_params m ReloadSessionNotification.of_yojson in
              match Result.join params with
              | Ok p ->
                  let sess =
                    find_or_create_session manager p.uri (mk_server notify_back config env)
                  in

                  send_req sess Reload_req;
                  return ()
              | Error e -> failwith e
            end
          | "proof/resetSession" -> begin
              let params = Json.message_params m ResetSessionNotification.of_yojson in
              match Result.join params with
              | Ok p ->
                  let sess =
                    find_or_create_session manager p.uri (mk_server notify_back config env)
                  in

                  send_req sess Reset_proofs_req;
                  send_req sess Reload_req;
                  return ()
              | Error e -> failwith e
            end
          | _ -> return ()
        end
      | Lsp.Client_notification.DidSaveTextDocument n -> self#on_did_save_notif ~notify_back n
      | _ -> return ()
  end
