open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Containers
module C = Constants

include Case_d

let hq_kind (v:t) org_id loc_id =
  Hq.get_kind org_id loc_id v.d.locs v.d.orgs v.d.roles v.d.agents v.s.mm v.world

let hq_known_to_org org1_id org2_id loc_id v =
  Hq.known_to_org org1_id org2_id loc_id v.d.locs v.d.orgs v.d.roles v.d.agents v.s.mm v.world

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

let time_pass (s:Services.t) ?(force_tick=false) minutes (v:t) =
  let do_tick, time =
    let time = v.time in
    let time' = Time.update minutes time in
    Time.should_do_tick time time' || force_tick, time'
  in
  if do_tick then
    let time = Time.do_tick time in
    let factor = Difficulty.to_enum v.world.difficulty + 3 in
    let agents = Agent.S.reduce_anxiety factor v.d.agents in
    let enemy_anxiety = v.enemy_anxiety - v.enemy_anxiety/factor in
    let agent_autoescape = None in
    let v = {v with time; d={v.d with agents}; enemy_anxiety; agent_autoescape} in
    v
  else
    {v with time}


