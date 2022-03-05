let process uri contents =
  Ok ()

class why_lsp_server =
	object(self)
		inherit Linol_lwt.Jsonrpc2.server

	(* one env per document *)
  val buffers: (Lsp.Types.DocumentUri.t, unit) Hashtbl.t = Hashtbl.create 32

  method private _on_doc
      ~(notify_back:Linol_lwt.Jsonrpc2.notify_back)
      (uri:Lsp.Types.DocumentUri.t) (contents:string) =
      match process uri (Some contents) with
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

	method on_notif_doc_did_open ~notify_back d ~content =
    self#_on_doc ~notify_back d.uri content

  method on_notif_doc_did_change ~notify_back d _c ~old_content:_old ~new_content =
    self#_on_doc ~notify_back d.uri new_content

  method on_notif_doc_did_close ~notify_back:_ d =
    Hashtbl.remove buffers d.uri;
    Linol_lwt.Jsonrpc2.IO.return ()

	end

