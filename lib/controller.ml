open Why3
open Controller_itp
open Session_itp

type controller = Controller_itp.controller
type id = int

let session : controller -> Session_itp.session = assert false

let strategies : controller -> string list = assert false

let reload : controller -> unit = assert false

let unproved_tasks : controller -> proofNodeID list = assert false

let replay : controller -> unit Lwt.t = assert false

let save : controller -> unit Lwt.t = assert false

let reset : controller -> unit = assert false

let run_strategy : controller -> string -> Session_itp.proofNodeID -> unit Lwt.t = assert false

let from_file : mkdir:bool -> string -> controller * string = assert false

let pan_to_id  : id Hpan.t = Hpan.create 17
let pn_to_id   : id Hpn.t = Hpn.create 17
let tn_to_id   : id Htn.t = Htn.create 17
let th_to_id   : id Ident.Hid.t = Ident.Hid.create 7
let file_to_id : id Hfile.t = Hfile.create 3

let id_from_pan  pan  = Hpan.find pan_to_id pan
let id_from_pn   pn   = Hpn.find pn_to_id pn
let id_from_tn   tn   = Htn.find tn_to_id tn
let id_from_th   th   = Ident.Hid.find th_to_id (theory_name th)
let id_from_file file = Hfile.find file_to_id (file_id file)
