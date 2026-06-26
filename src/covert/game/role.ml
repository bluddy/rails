open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Containers
module Gen = Engine.My_gen
module String = Engine.String

include Role_d

let make agent discover_val name bits clue_rand rank some_num can_relocate =
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
      tick=None;
      discovery_val=0;
    };
    can_relocate;
}

let first = Id.of_int 0

let ctr_tick v = v.ctr.tick
let ctr_discovery_val v = v.ctr.discovery_val
let update_tick tick ctr = {ctr with tick}
let update_ctr_tick tick v = {v with ctr={v.ctr with tick}}
let update_ctr_discovery discovery_val v = {v with ctr={v.ctr with discovery_val}}

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

let mastermind_bit v = v.bits land 0x100 > 0

(* Special hard-coded action *)
let hardcoded_action_bit1 v = v.bits land 0x200 > 0

let from_stream ~num_roles s =
  Iter.fold (fun acc _ ->
    let agent = Gen.get_wordi s |> Agent_id.of_int in
    let discover_val = Gen.get_wordi s in
    let name = Gen.take 32 s |> Gen.to_stringi |> String.remove_nulls in
    let _clue_seed = Gen.get_wordi s in
    let bits = Gen.get_wordi s in
    let _known_data = Gen.get_wordi s in
    let clue_rand = Gen.get_wordi s in
    let rank = Gen.get_wordi s |> Rank.of_enum |> Option.get_exn_or "invalid rank" in
    let some_num = Gen.get_wordi s in
    let can_relocate = bits land 0x8 > 0 in
    let role = make agent discover_val name bits clue_rand rank some_num can_relocate in
    print_endline @@ show role;
    role::acc
  )
  []
  Iter.(0 -- (num_roles - 1)) |> List.rev

let random r v =
  let length = Map.cardinal v in
  Random.int length r |> Id.of_int

module S = struct

  let to_agent v role_id = (Map.find role_id v).agent

  let update event_id fn v = Map.update event_id (Option.map fn) v

  let update_ctr event_id fn v = update event_id (fun v -> {v with ctr=fn v.ctr}) v

  let random_with_diff r diff v =
    let max = Map.cardinal v - (Difficulty.to_enum diff) / 4 in
    Random.int max r |> Id.of_int

  let make_red_herring r agent events v =
    let add role v =
      let id = Map.cardinal v |> Id.of_int in
      Map.add id role v
    in
    let find_clue_rand clue_rand v =
      Map.find_pred (fun _ role -> role.clue_rand land 0x7e = clue_rand land 0x7e) v
    in
    let discover_val, clue_seed = Utils.do_while
      (fun () ->
        let discover_val = Random.int_range 3 6 r in
        let clue_seed = Random.int 32767 r in
        discover_val, clue_seed)
      (fun (_, clue_seed) ->
        find_clue_rand clue_seed v |> Option.is_some)
    in
    let num_events = Event_id.Map.cardinal events in
    let tick = Random.int (num_events/2) r + 4 in
    let clue_rand = Random.int 7 r in
    let rank = Rank.random r in
    let role = {
      agent;
      discover_val;
      name="Red Herring";
      clue_seed;
      bits=0;
      known=Known_data.Set.empty;
      clue_rand;
      rank;
      ctr={tick=Some tick; discovery_val=0};
      some_num=0;
      can_relocate=false;
    }
    in
    add role v

end
