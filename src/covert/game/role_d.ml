open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Containers

type ctr = {
  tick: int;
  discovery_val: int;
} [@@deriving yojson, show]

type t = {
  agent: Agent_id.t;
  discover_val: int;
  name: string;
  clue_seed: int;
  bits: int;
  known: Known_data.Set.t; [@opaque]
  clue_rand: int;
  rank: Rank.t;
  some_num: int;
  ctr: ctr;
} [@@deriving yojson, show]

module Id = Engine.Int_id.Make()

module Map = struct
  include Utils.Map.Make(struct 
    type t = Id.t [@@deriving yojson]
    let compare = Id.compare
  end)
  let of_ordered_list l = List.mapi (fun i x -> Id.of_int i, x) l |> of_list
end

module Set = Utils.Set.Make(struct
  type t = Id.t [@@deriving yojson]
  let compare = Id.compare
end)

type map = t Map.t [@@deriving yojson]
