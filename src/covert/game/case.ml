open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Containers
module C = Constants

include Case_d

let hq_kind (v:t) org_id loc_id =
  Hq.get_kind org_id loc_id v.d.locs v.d.orgs v.d.roles v.d.agents v.s.mm v.world

let hq_known_to_org org1_id org2_id loc_id v =
  Hq.known_to_org org1_id org2_id loc_id v.d.locs v.d.orgs v.d.roles v.d.agents v.s.mm v.world

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


