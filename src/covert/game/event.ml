open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

type t = {
  role: Role.Id.t;
  tick_or_status: int;
  num_id: int;
  text: string;
  bits: int;
  item_bits: int;
  efficiency: int;
} [@@deriving yojson]

module Id = Engine.Int_id.Make()

