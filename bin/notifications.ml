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

  type t = {
    uri : Lsp.Uri.t; [@to_yojson uri_to_yojson]
    id : int;
    parent_id : int;
    name : string;
    proved : bool;
  }
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
  type t = { id : int } [@@deriving to_yojson]

  let to_jsonrpc n : Jsonrpc.Message.notification =
    {
      method_ = "proof/removeTreeNode";
      id = ();
      params = Some (Jsonrpc.Message.Structured.of_json (to_yojson n));
    }
end
