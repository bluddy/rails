open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

type status =
  | Ready
  | Tick of int
  [@@deriving yojson,show]

type info =
  | EmptyBits (* not sure what this signifies *)

type interaction = Meet | Msg [@@deriving yojson]
type send_rcv = Send | Rcv [@@deriving yojson]
type kind =
  | With_role of {
    inter: interaction;
    send_rcv: send_rcv;
    role: Role.Id.t;
  }
  | Misc
  | Terminal
  [@@deriving yojson]

type t = {
  role: Role.Id.t;
  status: status;
  num_id: int;
  text: string;
  bits: int;
  item_bits: int;
  efficiency: int;
  kind: kind;
  incapacitated_ok: bool; (* OK to have agent out of action *)
} [@@deriving yojson]

module Id = Event_id

module Map = Event_id.Map

type map = t Map.t [@@deriving yojson]

let has_role event = match event.kind with
  | With_role _ -> true
  | _ -> false

let is_ready event = match event.status with Ready -> true | _ -> false
let status v = v.status
let update_status status v = {v with status}
let set_tick tick v = {v with status=Tick tick}



