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

let make agent discover_val name bits clue_rand rank some_num =
  {
    agent;
    discover_val;
    name;
    clue_seed=0;
    bits;
    known=Known_data.Set.empty;
    clue_rand;
    rank;
    some_num;
    ctr= {
      tick= -1;
      discovery_val=0;
    };
}

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

let first = Id.of_int 0

let loc_bit v =
  let bits = v.bits in
  if bits land 0x10 > 0 then `Loc_enemy2 else
  if bits land 0x20 > 0 then `Loc_enemy else
  if bits land 0x2 > 0 then `Loc_ally else
  `Loc_any

let org_bit v =
  let bits = v.bits in
  if bits land 0x40 > 0 then `Org_enemy2 else
  if bits land 0x80 > 0 then `Org_enemy else
  if bits land 0x1 > 0 then `Org_ally else
  `Org_any

let mastermind_bit v = v.bits land 0x100

