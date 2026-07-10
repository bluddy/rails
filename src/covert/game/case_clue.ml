open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Containers
module C = Constants

open Case

let clue_find_to_discover random role_id roles (v:t) =
  let role = Role.Map.find role_id roles in
  if Known_data.Set.all_standard role.Role.known then None else
  if Difficulty.lowest (G.difficulty v) &&
    not @@ Role.check_known [`Known_role] role then Some `Known_role else
  if role.clue_rand = 2 (* From OG *)
    && Difficulty.(G.difficulty v < Regional_conflict)
    && not @@ Role.check_known [`Known_loc] role then Some `Known_loc else
  let known = Utils.do_while
    (fun () -> Known_data.random random)
    (fun known -> Role.check_known [known] role)
  in
  Some known

let clue_gen ?(in_org_id=Org.cia) in_loc_id clue_amt clue_type (v:t) =
  let agents, orgs, locs, roles = G.agents v, G.orgs v, G.locs v, G.roles v in
  let clue_amt = clue_amt + 1 in
  Agent.Map.fold (fun agent_id agent (chosen_agent, agents, roles) ->
    let org_id = Agent.S.to_org agents agent_id in
    let loc_id = Agent.S.to_loc agents agent_id in
    let org_to_cia_dist = Org.connection orgs org_id in_org_id in
    let loc_to_agent_loc_dist = Loc.connection locs loc_id in_loc_id in
    let chosen_agent = if org_to_cia_dist = 0 && loc_to_agent_loc_dist = 0 then Some agent_id else chosen_agent in
    let clue_div_dist = (clue_amt / ((loc_to_agent_loc_dist + 2) * (org_to_cia_dist + 2))) / 256 in
    let clue_div_dist2 = (clue_amt / ((loc_to_agent_loc_dist + 6) * (org_to_cia_dist + 3))) / 64 in
    let diff_factor = 10000 / ((G.difficulty v |> Difficulty.to_enum) + 2) / clue_div_dist  in
    let diff_factor_2 = diff_factor * diff_factor in
    let discover_val = ((Agent.G.discover_val agent / 2) + diff_factor + 1) / diff_factor_2 in
    let agents = Agent.S.update agent_id (Agent.U.discover_val discover_val) agents in
    let roles =
      Role.Set.fold (fun role_id (roles as acc) ->
        let role = Role.Map.find role_id roles in
        match Role.G.ctr_tick role with
        | Some tick when tick <= v.time.tick ->
            let w = 5000/((G.difficulty v |> Difficulty.to_enum) + 3) in
            let role = Role.U.ctr_discovery_add (w / clue_div_dist2) role in
            let rec loop role =
              let needed_val = Known_data.Set.to_discover_val (Role.G.known role) in
              let needed_val = (needed_val + 2) * (needed_val + 2) * 32 in
              let disc = Role.G.ctr_discovery role * ((Role.G.discover role) + 2) in
              if disc <= needed_val then role else
              if Known_data.Set.all_standard role.known then role else
              (* TODO: reveal clue *)
              loop role
            in
            let role = loop role in
            Role.Map.add role_id role roles
        | _ -> acc)
      agent.roles
      roles
    in
    chosen_agent, agents, roles)
  agents
  (None, agents, roles)

