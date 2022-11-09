open Why3
open Itp_communication
open Task_tree
type manager
type session

val find_or_create_session : manager -> Lsp.Types.DocumentUri.t -> (manager -> session -> unit) -> session

val send_req : session -> ide_request -> unit

val add_node : session -> node_ID -> node_ID -> Task_tree.node_info -> unit

val edit_node : session -> int -> (node_info * task_info option -> node_info * task_info option) -> unit

val remove_node : session -> int -> unit

val reset_tree : session -> unit

val get_requests : session -> ide_request list

val get_tasks : session -> (node_ID * Task_tree.node_info * Task_tree.task_info) list

(* val task_count : session -> int *)

val unsolved_tasks : manager -> (string, (Loc.position * Task_tree.node_info) list) Hashtbl.t

val session_path : session -> Linol_lwt.DocumentUri.t

type control_flow = Stop | Continue

val for_tasks_at: manager -> Linol_lwt.DocumentUri.t -> Linol_lwt.Range.t -> ((node_ID * Task_tree.node_info * Task_tree.task_info) -> control_flow) -> unit
(* val fold_sessions : ('a -> session -> 'a) -> 'a  -> manager -> 'a *)

val new_manager : unit -> manager

