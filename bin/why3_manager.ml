open Why3
open Itp_communication
open Task_tree

type session = { root : Lsp.Uri.t; input : ide_request list ref; tree : task_tree ref }

type manager = {
  file_to_session_root : (Lsp.Types.DocumentUri.t, string) Hashtbl.t;
  sessions : (string, session) Hashtbl.t;
}

let send_req (s : session) req = s.input := req :: !(s.input)
let session_path (s : session) : Lsp.Uri.t = s.root

let get_requests (s : session) : ide_request list =
  let l = !(s.input) in
  s.input := [];
  List.rev l

let add_node (s : session) p_id id node = add_tree p_id !(s.tree) id node None
let remove_node (s : session) id : unit = remove_tree id !(s.tree)
let edit_node (s : session) id f : unit = update_node id !(s.tree) f

let get_tasks (s : session) : (node_ID * node_info * task_info) list =
  List.filter_map
    (fun (id, nm, t) -> match t with Some t -> Some (id, nm, t) | None -> None)
    (Task_tree.to_list !(s.tree))

let reset_tree (s : session) : unit = s.tree := new_tree ()

let unsolved_tasks_sess (srvr : session) acc : (string, (_ * Task_tree.node_info) list) Hashtbl.t =
  let tasks = get_tasks srvr in
  List.fold_left
    (fun acc (_, info, t) ->
      match t.goal with
      | Some g ->
          let file, _, _, _, _ = Loc.get g in
          if info.proved then (
            let prev : _ option = Hashtbl.find_opt acc file in
            Hashtbl.replace acc file (Option.value prev ~default:[]);
            acc)
          else
            let prev : _ option = Hashtbl.find_opt acc file in
            let next = (g, info) :: Option.value prev ~default:[] in
            Hashtbl.replace acc file next;
            acc
      | None -> acc)
    acc tasks

(*
  Build a map containing the unsolved goals of every file who's spans are found in the sessions
*)
let unsolved_tasks (manager : manager) =
  Hashtbl.fold (fun _ s acc -> unsolved_tasks_sess s acc) manager.sessions (Hashtbl.create 32)

module type SERVER = sig
  val init_server : ?send_source:bool -> Whyconf.config -> Env.env -> string -> unit
end

open Linol_lwt

let loc_contains loc doc (needle : Range.t) =
  let f, l1, c1, l2, c2 = Loc.get loc in
  f = DocumentUri.to_path doc
  && l1 <= needle.start.line + 1
  && needle.end_.line + 1 <= l2
  && c1 <= needle.start.character && needle.end_.character <= c2

type control_flow = Stop | Continue

let rec for_task_at tasks doc loc func =
  match Seq.uncons tasks with
  | None -> ()
  | Some ((id, node, info), tasks) -> (
      let next =
        match info with
        | Some ({ goal = Some goal; _ } as info) ->
            if loc_contains goal doc loc then func (id, node, info) else Continue
        | _ -> Continue
      in

      match next with Continue -> for_task_at tasks doc loc func | Stop -> ())

let for_tasks_at manager doc (loc : Range.t) func =
  Hashtbl.iter
    (fun _ sess -> for_task_at (Task_tree.to_seq !(sess.tree)) doc loc func)
    manager.sessions

let find_session manager uri =
  let sess =
    Option.bind
      (Hashtbl.find_opt manager.file_to_session_root uri)
      (Hashtbl.find_opt manager.sessions)
  in

  sess

let find_or_create_session manager uri (create : manager -> session -> unit) =
  let sess =
    Option.bind
      (Hashtbl.find_opt manager.file_to_session_root uri)
      (Hashtbl.find_opt manager.sessions)
  in
  match sess with
  | Some s -> s
  | None ->
      let f = Queue.create () in
      Queue.push (DocumentUri.to_path uri) f;
      let sess = Server_utils.get_session_dir ~allow_mkdir:true f in
      Hashtbl.replace manager.file_to_session_root uri sess;
      let session =
        { root = Lsp.Uri.of_path sess; input = ref []; tree = ref (Task_tree.new_tree ()) }
      in
      Hashtbl.add manager.sessions sess session;
      create manager session;
      session

(* let rec fold_sessions _ = failwith "todo" *)

let new_manager _ = { file_to_session_root = Hashtbl.create 32; sessions = Hashtbl.create 32 }
