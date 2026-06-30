open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Containers
module C = Constants
module Bul = Bulletin_d

include Case_d

let hq_kind (v:t) org_id loc_id =
  Hq.get_kind org_id loc_id v.d.locs (G.orgs v) (G.roles v) (G.agents v) v.s.mm v.world

let hq_known_to_org org1_id org2_id loc_id v =
  Hq.known_to_org org1_id org2_id loc_id (G.locs v) (G.orgs v) (G.roles v) (G.agents v) v.s.mm v.world

let clear_autoescape v = {v with agent_autoescape=None}

let create_action time kind v actions =
  Action.S.create time kind (G.events v) (G.roles v) (G.agents v) actions

let event_to_text (s:Services.t) event v =
  Event.to_text s.resources (G.crime v) (G.step v) (G.roles v) (G.agents v) (G.orgs v) (G.locs v) event

let double_agent_at_loc v loc =
  Loc.Set.mem loc (G.double_agents v)

let check_escape_jail (s:Services.t) agent_id v =
  let pass_test = match v.agent_autoescape with
  | Some agent when Agent.Id.(agent = agent_id) -> true
  | _ -> false
  in
  let pass_test = pass_test ||
    Random.int 8 s.random > (v.world.difficulty |> Difficulty.to_enum)
  in
  let agent = Agent.Map.find agent_id v.d.agents in
  let mm = Agent.Map.find Agent.mastermind v.d.agents in
  if pass_test &&
    Agent.is_arrested agent &&
    Difficulty.(v.world.difficulty > National_threat) &&
    not @@ Agent.is_known `Known_jailbreak agent &&
    not @@ Agent.is_arrested mm then
      {v with agent_autoescape=Some agent_id; agent_jailbreak=Some agent_id}
    else
      v

