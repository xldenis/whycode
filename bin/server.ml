let process cont uri contents =
  Error "hi :)"

open Backend
open Why3
open Uri

(*
  1. Map files to sessions
  2. When a new session is discovered ( doesn't exist), create it
  3. Create a controller for each session

  TODO: build the 'server infos': prover list, transofmrations, strategies, and commands
 *)

type session_id = string
type session_data = unit

module type SERVER = sig end

let first_class_server config env session_path =
  let open Itp_server in
  let module P = ( struct
    let notify msg = assert false
    let get_requests () = assert false
   end : Protocol) in
  let module S = (struct
    let blocking = true
    let multiplier = 1
    let timeout ~ms _ = assert false
    let idle ~prio _ = assert false
  end : Controller_itp.Scheduler) in
  let module Server = Make(S)(P) in
  let s = (module Server : SERVER) in s

(* build a 'session controller' *)
let build_controller config env session_path =
  let ses = Session_itp.load_session session_path in
  let c = Controller_itp.create_controller config env ses in
  Server_utils.load_strategies c;
  c

class why_lsp_server = object(self)
	inherit Linol_lwt.Jsonrpc2.server
(*
  val files : string Queue.t = Queue.create ()

  initializer
    let cli_opts = [] in
    let usage_str = "" in
    let config, env =
      (* Can this be ditched entirely? *)
      Whyconf.Args.initialize cli_opts (fun f -> Queue.add f files) usage_str
    in ()
 *)   (*  let dir = try
        Server_utils.get_session_dir ~allow_mkdir:true files
      with Invalid_argument s ->
        Format.eprintf "Error: %s@." s;
        Whyconf.Args.exit_with_usage cli_opts usage_str
    in
    S.init_server config env dir *)

	(* one env per document *)


  val file_to_session : (Lsp.Types.DocumentUri.t, session_id) Hashtbl.t = Hashtbl.create 32
  val sessions : (session_id, session_data) Hashtbl.t = Hashtbl.create 32

  val buffers: (Lsp.Types.DocumentUri.t, unit) Hashtbl.t = Hashtbl.create 32

  method private _on_doc
      ~(notify_back:Linol_lwt.Jsonrpc2.notify_back)
      (uri:Lsp.Types.DocumentUri.t) (contents:string) =
      let sess_id = self#find_session uri in
      let cont = Hashtbl.find_opt sessions sess_id in
      let cont = match cont with
        | Some cont -> cont
        | None -> assert false
        (* build_controller config env sess_id *)
      in

      match process cont uri (Some contents) with
      | Ok state ->
        Hashtbl.replace buffers uri state;
                  notify_back#send_diagnostic []
      | Error msg ->
        Linol_lwt.Jsonrpc2.IO.failwith (
          Format.asprintf "Internal why3 error: %s" msg
        )
    (* TODO: unescape uri/translate it to a correct path ? *)
(*     match Loop.process uri (Some contents) with
    | Ok state ->
      let diags = state.solve_state in
      Hashtbl.replace buffers uri state;
      notify_back#send_diagnostic diags
    | Error msg ->
      Linol_lwt.Jsonrpc2.IO.failwith (
        Format.asprintf "Internal dolmen error: %s" msg
      ) *)

  method private find_session (uri: Lsp.Types.DocumentUri.t) : session_id =
    match Hashtbl.find_opt file_to_session uri with
    | Some sess_id -> sess_id
    | None ->
      let f = Queue.create () in
      Queue.push (Uri.path (Uri.of_string uri)) f;
      let sess = Server_utils.get_session_dir ~allow_mkdir:true f in
      Hashtbl.replace file_to_session uri sess;
      sess


	method on_notif_doc_did_open ~notify_back d ~content =
    self#_on_doc ~notify_back d.uri content

  method on_notif_doc_did_change ~notify_back d _c ~old_content:_old ~new_content =
    self#_on_doc ~notify_back d.uri new_content

  method on_notif_doc_did_close ~notify_back:_ d =
    Hashtbl.remove buffers d.uri;
    Linol_lwt.Jsonrpc2.IO.return ()

	end

