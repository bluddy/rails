open! Containers

include Clue_d
module Sub= Subst_engine

let create_rand_num (s:Services.t) role_id difficulty roles agents =
  let role = Role.Map.find role_id roles in
  let clue_random = (role.Role.clue_seed land 0xFF) / 2 in
  let num =
    if Role.S.test_with_diff_div_4 difficulty role_id roles then
      let agent_id = Role.S.to_agent roles role_id in
      let id_code = (Agent.Map.find agent_id agents).Agent.id_code in
      let v1 = (id_code lsr 3) mod 4 in
      let v2 = id_code mod 6 in
      let pat = Printf.sprintf "*C0%d0%d" v1 v2 in
      let clue_txt = Hashtbl.find s.resources.text `Clues in
      let txt = Sub.get_lines ~pat clue_txt |> Option.get_exn_or "Clue text not found" in
      let pats = Sub.Pattern.([Key, ""; Name, ""; Org, ""; City, ""; Role, ""]) in
      let txt = Sub.subst_pat pats txt in
      let c = ((txt.[1] |> Char.to_int) land 0xF) lsl 8 in
      c
    else
      role.clue_rand lsl 8
  in
  num + clue_random

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

