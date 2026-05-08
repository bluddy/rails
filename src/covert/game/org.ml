open Ppx_yojson_conv_lib.Yojson_conv.Primitives

open Containers
module C = Constants

module Id = Engine.Int_id.Make() (* Id in current session *)
module Global_id = Engine.Int_id.Make() (* Global id among all orgs in game *)

type t = {
  global_id: Global_id.t option;
  short_name: string;
  name: string;
  connect: int * int;
  strength: int;
  hq_build_cost: int;
  bits: int;
  agent_name_offset: int; (* offset into the name list *)
} [@@deriving show, ord, yojson]

let random ?(start=0) r = Random.int_range start (C.num_orgs - 1) r |> Id.of_int

module Map = Utils.Map.Make(struct
  type t = Id.t [@@deriving yojson]
  let compare = Id.compare
end)

