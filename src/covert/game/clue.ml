open! Containers

include Clue_d

let create org_id loc_id role_id known roles locs orgs agents clues =
  let agent_id = Role.S.to_agent roles role_id in
  let connect = match known with
  | `Known_face -> Connect_face agent_id
  | `Known_agent -> Connect_agent agent_id
  | `Known_org -> Connect_org (Agent.S.to_org agents agent_id)
  | `Known_role -> Connect_role role_id
  | `Known_loc -> Connect_loc (Agent.S.to_loc agents agent_id)
  in
  let orgs = match known with
  | `Known_org ->
      let org_id = Agent.S.to_org agents agent_id in
      orgs
      |> Org.S.incr_activity org_id
      |> Org.S.add_known org_id
  | _ -> orgs
  in
  let locs = match known with
  | `Known_loc -> Loc.S.incr_activity (Agent.S.to_loc agents agent_id) locs
  | _ -> locs
  in
  let clue = {
    org=org_id;
    loc=loc_id;
    role=role_id;
    connect;
    name_idx=0;
  }
  in
  clue, orgs, locs

