type task_tree

open Why3
open Itp_communication

type task_info = {
  task : string;
  locations : (Loc.position * color) list;
  goal : Loc.position option;
  lang : string;
}

type node_info = { name : string; proved : bool }


val add_tree : node_ID -> task_tree -> node_ID -> node_info -> task_info option -> unit

val remove_tree : node_ID -> task_tree -> unit

val update_node : node_ID -> task_tree -> (node_info * task_info option -> node_info * task_info option) -> unit

val new_tree : unit -> task_tree

val size : task_tree -> int

val to_list : task_tree -> (node_ID * node_info * task_info option) list

val to_seq : task_tree -> (node_ID * node_info * task_info option) Seq.t
