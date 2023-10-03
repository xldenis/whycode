let uri_of_yojson j =
  try Ok (Lsp.Types.DocumentUri.t_of_yojson j) with _ -> Error "could not parse uri"

let uri_to_yojson j = Lsp.Types.DocumentUri.yojson_of_t j

let range_of_yojson r =
  try Ok (Lsp.Types.Range.t_of_yojson r) with _ -> Error "could not parse uri"

(* improve parser to use json records instead *)
type target = [ `Range of (Lsp.Types.Range.t[@of_yojson range_of_yojson]) | `Node of int ]
[@@deriving of_yojson]

module RunTransformationRequest = struct
  type t = {
    command : string;
    target : target option;
    uri : Lsp.Types.DocumentUri.t; [@of_yojson uri_of_yojson]
  }
  [@@deriving of_yojson] [@@yojson.allow_extra_fields]

  let create com target uri = { command = com; target; uri }
end

module ResolveSessionRequest = struct
  type t = { uri : Lsp.Types.DocumentUri.t [@of_yojson uri_of_yojson] } [@@deriving of_yojson]

  type response = { uri : Lsp.Types.DocumentUri.t [@to_yojson uri_to_yojson] }
  [@@deriving to_yojson]
end

module ListTransformationsRequest = struct
  type t = { uri : Lsp.Types.DocumentUri.t [@of_yojson uri_of_yojson] } [@@deriving of_yojson]

  (* let yojosn_of_command c = Lsp.Types.Command.yojson_of_t c *)

  (* type response = { transformations: [ `Command of( Lsp.Types.Command.t [@to_yojson yojosn_of_command]) ] list } *)
  type response = { transformations : (string * string) list } [@@deriving to_yojson]
end

module StartProofNotification = struct
  type t = { uri : Lsp.Types.DocumentUri.t [@of_yojson uri_of_yojson] } [@@deriving of_yojson]

  type response = { uri : Lsp.Types.DocumentUri.t [@to_yojson uri_to_yojson] }
  [@@deriving to_yojson]
end

module ResetSessionNotification = struct
  type t = { uri : Lsp.Types.DocumentUri.t [@of_yojson uri_of_yojson] }
  [@@deriving of_yojson] [@@yojson.allow_extra_fields]
end

module ReloadSessionNotification = struct
  type t = { uri : Lsp.Types.DocumentUri.t [@of_yojson uri_of_yojson] }
  [@@deriving of_yojson] [@@yojson.allow_extra_fields]
end

module ReplaySessionNotification = struct
  type t = { uri : Lsp.Types.DocumentUri.t [@of_yojson uri_of_yojson] }
  [@@deriving of_yojson] [@@yojson.allow_extra_fields]
end

module ShowTaskRequest = struct
  type t = { uri : Lsp.Types.DocumentUri.t; [@of_yojson uri_of_yojson] target : target }
  [@@deriving of_yojson] [@@yojson.allow_extra_fields]
end

module PublishTreeNotification = struct
  type t = {
    uri : Lsp.Types.DocumentUri.t; [@to_yojson uri_to_yojson]
    elems : Controller.tree_elem list;
  }
  [@@deriving to_yojson]
end
