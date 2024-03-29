open Why3
open Session_itp

type controller
type id = int

val session : controller -> Session_itp.session
val env : controller -> Env.env
val strategies : controller -> string list

(* Returns a list of transformation names accompanied by a description of their arguments (I think) *)
val transformations : controller -> (string * string) list
val unproved_tasks : controller -> id list
val all_tasks : controller -> id list
val reload : controller -> unit
val replay : controller -> unit Lwt.t
val reset : controller -> unit
val save : controller -> unit
val run_strategy : controller -> string -> id -> unit Lwt.t
val run_transform : controller -> string -> string list -> id -> unit Lwt.t

type task = { expl : string; loc : Loc.position; proved : bool }

val task : controller -> id -> task
(** Fetch the task corresponding to [id], if there is one. Raises [Not_found] if no task exists, because the id is detached *)

val task_body : controller -> id -> string

type tree_elem = { expl : string; proved : bool; id : id; parent : id option }
[@@deriving to_yojson]

val file_tree_as_list : controller -> (file * tree_elem list) list
val from_file : mkdir:bool -> Whyconf.config -> Env.env -> string -> controller * string * bool
val name_for_id : controller -> id -> string
val is_detached : controller -> id -> bool
