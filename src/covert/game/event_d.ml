open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

type status =
  | Ready
  | Tick of int
  [@@deriving yojson,show]

type t = {
  role: Role.Id.t;
  status: status;
  num_id: int;
  text: string;
  bits: int;
  item_bits: int;
  efficiency: int;
} [@@deriving yojson, show]

module Id = Event_id

module Map = Event_id.Map

type map = t Map.t [@@deriving yojson]
