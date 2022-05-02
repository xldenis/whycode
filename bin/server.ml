let process cont uri contents = Error "hi :)"

open Backend
open Why3
open Uri

module RunTransformationNotification = struct
  type t =
    { command: string; args: string list }
    [@@deriving yojson] [@@yojson.allow_extra_fields]
end

let loc_to_range loc =
  let _, l, c1, c2 = Loc.get loc in
  Lsp.Types.Range.create (* why is this necessary?       ----------v *)
    ~start:(Lsp.Types.Position.create ~line:(l - 1) ~character:c1)
    ~end_:(Lsp.Types.Position.create ~line:(l - 1) ~character:c2)

let warn loc message =
  Lsp.Types.Diagnostic.create () ~range:(loc_to_range loc) ~severity:Warning
    ~source:"Why3" ~message

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

  (* seperate out the node body from the tree itself *)
  type task_tree =
    | Node of node_ID * string * task_info option * task_tree list

  let rec add_tree parent_id tree self =
    match self with
    | Node (id, nm, task, children) ->
        if parent_id = id then Node (id, nm, task, tree :: children)
        else Node (id, nm, task, List.map (add_tree parent_id tree) children)

  let rec remove_tree node_id self =
    match self with
    | Node (id, nm, task, children) ->
        if node_id = id then failwith "cannot remove root node"
        else
          let check (Node (id, _, _, _) as n) =
            if id = node_id then None else Some (remove_tree id n)
          in
          Node (id, nm, task, List.filter_map check children)

  let rec update_node node_id f self =
    let (Node (id, nm, task, children)) = self in
    if node_id = id then f self
    else Node (id, nm, task, List.map (update_node node_id f) children)

  let to_list self : (string * task_info option) list =
    let rec worker acc self =
      let (Node (_, nm, task, children)) = self in
      List.fold_left worker ((nm, task) :: acc) children
    in
    worker [] self
end

open Itp_communication
open TaskTree

type session_id = string
type session_data = ide_request list ref * task_tree option ref

let send_req (s : session_data) req =
  let r, _ = s in
  r := req :: !r

let add_node (s : session_data) p_id n =
  let _, t = s in
  match !t with
  | None -> t := Some n
  | Some tr -> t := Some (add_tree p_id n tr)

let remove_node s id : unit =
  let _, t = s in
  match !t with
  | None -> failwith "cannot remove from empty tree"
  | Some tr -> t := Some (remove_tree id tr)

let edit_node s id f : unit =
  let _, t = s in
  match !t with
  | None -> failwith "cannot edit empty tree"
  | Some tr -> t := Some (update_node id f tr)

let get_tasks s : (string * task_info) list =
  let _, t = s in
  match !t with
  | None -> []
  | Some tr ->
      List.filter_map
        (fun (nm, t) -> match t with Some t -> Some (nm, t) | None -> None)
        (to_list tr)

open Linol_lwt

module type SERVER = sig
  val init_server :
    ?send_source:bool -> Whyconf.config -> Env.env -> string -> unit
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
  | Saved -> "Saved"
  | Dead _ -> "Dead"
  | Task _ -> "Task"
  | File_contents _ -> "File_contents"
  | Source_and_ce _ -> "Source_and_ce"
  | Ident_notif_loc _ -> "Ident_notif_loc"

let handle_notification s (notify_back : Jsonrpc2.notify_back)
    (notif : notification) =
  let* _ = notify_back#send_log_msg ~type_:MessageType.Info (notif_str notif) in
  match notif with
  (* | Message _ -> IO_lwt.failwith "Message" *)
  | Initialized _ -> return ()
  | Saved -> return ()
  (* | Dead _ -> IO_lwt.failwith "Server Died" *)
  | New_node (id, par_id, ty, nm, _) -> (
      let n = Node (id, nm, None, []) in
      add_node s par_id n;
      (* Ask for the task of every 'goal' node so that we can get the
         spans of the unsolved goals *)
      match ty with
      | NGoal ->
          send_req s (Get_task (id, false, true));
          return ()
      | _ -> return ())
  | Task (id, task, locs, goal, lang) ->
      let task_info = { task; locations = locs; goal; lang } in

      edit_node s id (fun node ->
          let (Node (id, nm, info, children)) = node in
          Node (id, nm, Some task_info, children));
      return ()
  | Node_change (id, info) -> begin
      match info with
      | Name_change nm' ->
          edit_node s id (fun node ->
              let (Node (id, nm, info, children)) = node in
              Node (id, nm', info, children));
          return ()
      | _ -> return ()
    end
  | Remove id ->
      remove_node s id;
      return ()
  | Reset_whole_tree ->
      let _, t = s in
      t := None;
      return ()
  (*
      | Next_Unproven_Node_Id _ -> IO_lwt.failwith "Next_Unproven_Node_Id"
      | Saving_needed _ -> IO_lwt.failwith "Saving_needed"
      | File_contents _ -> IO_lwt.failwith "File_contents"
      | Source_and_ce _ -> IO_lwt.failwith "Source_and_ce"
      | Ident_notif_loc _ -> IO_lwt.failwith "Ident_notif_loc"
  *)
  | _ -> return ()

let mk_server (notify_back : Jsonrpc2.notify_back) config env session_path :
    session_data =
  let open Itp_server in
  let requests = ref [] in
  let tree = ref None in

  let module P : Protocol = struct
    let notify msg =
      spawn (fun () -> handle_notification (requests, tree) notify_back msg)

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
    let rec insert_idle_handler p f =
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
  let s = (module Server : SERVER) in
  Server.init_server config env session_path;
  (requests, tree)

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
      Whyconf.Args.initialize cli_opts (fun f -> ()) usage_str
    in
    config <- config';
    env <- env'

    val file_to_session : (Lsp.Types.DocumentUri.t, session_id) Hashtbl.t =
      Hashtbl.create 32

    val sessions : (session_id, session_data) Hashtbl.t = Hashtbl.create 32
    val buffers : (Lsp.Types.DocumentUri.t, unit) Hashtbl.t = Hashtbl.create 32

    method private config_code_action_provider =
      `CodeActionOptions (CodeActionOptions.create ())

    method private _on_doc ~(notify_back : Linol_lwt.Jsonrpc2.notify_back)
        (uri : Lsp.Types.DocumentUri.t) (contents : string) =
      let sess_id = self#find_session uri in
      let cont = Hashtbl.find_opt sessions sess_id in

      (* spawn (fun () ->

           notify_back#send_log_msg ~type_:Info (String.concat "\n" (Array.to_list (Unix.environment ())))

         ); *)
      let srvr =
        match cont with
        | Some cont -> cont
        | None ->
            let s = mk_server notify_back config env sess_id in
            Hashtbl.add sessions sess_id s;
            s
      in

      send_req srvr Reload_req;

      (* Hack to see it work *)
      spawn (fun () ->
          let* () = Lwt_unix.sleep 0.125 in

          let tasks = get_tasks srvr in
          let diags =
            List.filter_map
              (fun (nm, t) ->
                match t.goal with Some g -> Some (warn g nm) | None -> None)
              tasks
          in
          notify_back#send_diagnostic diags);
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

    method on_notif_doc_did_open ~notify_back d ~content =
      self#_on_doc ~notify_back d.uri content

    method on_notif_doc_did_change ~notify_back d _c ~old_content:_old
        ~new_content =
      self#_on_doc ~notify_back d.uri new_content

    method on_notif_doc_did_close ~notify_back:_ d =
      Hashtbl.remove buffers d.uri;
      Linol_lwt.Jsonrpc2.IO.return ()

    method on_req_code_action ~notify_back ~id c =
      let sess_id = self#find_session c.textDocument.uri in
      let cont = Hashtbl.find_opt sessions sess_id in

      let* srvr =
        match cont with
        | Some cont -> return cont
        | None -> failwith "ruh-roh session not properly initialized"
      in
      let tasks = get_tasks srvr in
      let rec search tasks =
        match tasks with
        | (_, t) :: ts -> begin
            match t.goal with
            | Some g ->
                let _, l, c1, c2 = Loc.get g in
                if
                  c.range.start.line = l - 1
                  && c.range.end_.line = l - 1
                  && c1 <= c.range.start.character
                  && c.range.end_.character <= c2
                then true
                else search ts
            | None -> search ts
          end
        | [] -> false
      in
      let match_task = search tasks in
      (* let strats = get_strategies config in *)
      let strats : string list =
        [ "Auto_level_0"; "Auto_level_2"; "Auto_level_2"; "Split_VC" ]
      in

      let actions =
        List.map
          (fun s -> `Command (Command.create s "why3.runTransformation" ()))
          strats
      in
      if match_task then return (Some actions) else return None

    method on_notification_unhandled ~notify_back notif =
      let open Lsp.Import in
      match notif with
      | Lsp.Client_notification.UnknownNotification m ->
        begin match m.method_ with
        | "proof/runTransformation" ->
          let params = Json.message_params m RunTransformationNotification.of_yojson in
          (* RunTransformationNotification params *)
          return ()
        | _ -> return ()
        end
      | _ -> return ()
  end
