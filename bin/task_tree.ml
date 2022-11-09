open Why3
open Itp_communication

type task_info = {
  task : string;
  locations : (Loc.position * color) list;
  goal : Loc.position option;
  lang : string;
}

type node_info = { name : string; proved : bool }

(* seperate out the node body from the tree itself *)
type node = {
  parent : node_ID;
  id : node_ID;
  info : node_info;
  task : task_info option;
  children : node_ID list;
}

type task_tree = (node_ID, node) Hashtbl.t

let size = Hashtbl.length
let new_tree () = Hashtbl.create 10

let rec add_tree parent_id tree (id : node_ID) info task =
  try
    let parent = Hashtbl.find tree parent_id in
    Hashtbl.replace tree parent_id { parent with children = id :: parent.children };
    Hashtbl.add tree id { parent = parent.id; id; info; task; children = [] }
  with Not_found ->
    (* why3 never actually sends some nodes... the official ide does the same thing *)
    Hashtbl.add tree id { parent = parent_id; id; info; task; children = [] }

let rec remove_tree node_id self =
  let node = Hashtbl.find self node_id in
  let parent = Hashtbl.find_opt self node.parent in
  Hashtbl.remove self node_id;
  match parent with
  | Some parent ->
      Hashtbl.replace self parent.id
        { parent with children = List.filter (fun n -> node.id <> n) parent.children }
  | None -> ()

let rec update_node node_id self f =
  let node = Hashtbl.find self node_id in

  let info, task = f (node.info, node.task) in
  Hashtbl.replace self node_id { node with info; task }

let to_seq tree = Seq.map (fun (_, node) -> (node.id, node.info, node.task)) (Hashtbl.to_seq tree)
let to_list tree = List.of_seq (to_seq tree)
