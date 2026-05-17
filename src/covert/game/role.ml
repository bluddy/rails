open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Containers

type t = {
  agent: Agent.Id.t;
  discover_val: int;
  name: string;
  clue_seed: int;
  role_bits: int;
  known: Known_data.Set.t; [@opaque]
  clue_rand: int;
  rank: Rank.t;
  some_num: int;
} [@@deriving yojson, show]

module Id = Engine.Int_id.Make()
