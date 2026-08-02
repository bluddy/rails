open! Containers

  (* Propagate knowledge from agents to actions when we view the action *)
let propagate_known action_id (case:Case_d.t) =
  (* From action_print_and_propagate_known *)
  let actions, agents = Case_d.G.actions case, Case_d.G.agents case in
  match
    let action = Action.Map.find action_id actions in
    Action.G.agent1 action, Action.G.agent2 action with
  | Some agent_id1, Some agent_id2 ->
    let action_id1 = action_id in
    let action_id2 = Action.S.same_time_idx action_id actions |> Option.get_exn_or "failed to find pair action" in
    let filter_known ~do_loc = List.filter_map @@ function
      | `Known_agent when not do_loc -> Some `Known_agent
      | `Known_org when not do_loc -> Some `Known_org
      | `Known_loc when do_loc -> Some `Known_loc
      | _ -> None
    in
    let agent_to_action ~do_loc agent_id action_id agents actions =
      let known = Agent.S.G.known agent_id agents |> Known_data.Set.to_list |> filter_known ~do_loc in
      Action.S.add_known known action_id actions
    in
    let action_to_agent ~do_loc action_id agent_id agents actions =
      let known = Action.S.G.known action_id actions |> Action.Known.Set.to_list |> filter_known ~do_loc in
      Agent.S.add_known known agent_id agents
    in
    let propagate agent_id1 agent_id2 action_id1 action_id2 agents actions =
      let action1 = Action.Map.find action_id1 actions in
      let agent2 = Agent.Map.find agent_id2 agents in
      if (not (Action.is_known `Known_agent action1 && Agent.is_known `Known_agent agent2))
        && Action.is_known_all [`Known_org; `Known_loc] action1
        && Agent.is_known_all [`Known_org; `Known_loc] agent2 then
          let actions = agent_to_action ~do_loc:false agent_id2 action_id1 agents actions in
          let actions = agent_to_action ~do_loc:false agent_id1 action_id2 agents actions in
          let agents = action_to_agent ~do_loc:false action_id1 agent_id2 agents actions in
          let action_loc2 = Action.S.G.loc2 action_id1 actions |> Option.get_exn_or "oops" in
          if Loc.Id.(Agent.S.G.loc agent_id2 agents = action_loc2) then
            let actions = agent_to_action ~do_loc:true agent_id2 action_id1 agents actions in
            let agents = action_to_agent ~do_loc:true action_id1 agent_id2 agents actions in
            agents, actions
          else
            agents, actions
      else
        agents, actions
    in
    let agents, actions = propagate agent_id1 agent_id2 action_id1 action_id2 agents actions in
    let agents, actions = propagate agent_id2 agent_id1 action_id2 action_id1 agents actions in
    {case with d={case.d with agents; actions}}

  | _ -> case

let get_text action_id (case: Case_d.t) =
  let actions, events, agents, orgs, locs =
    Case.G.(actions case, events case, agents case, orgs case, locs case) in
  let action = Action.Map.find action_id actions in
  match action.kind with
  | Event_based (event_id, ev) ->
      let agent_id, loc_id = ev.agent2, ev.loc2 in (* Action talks about agent 2 *)
      let event = Event.Map.find event_id events in
      let name =
        if Action.is_known `Known_agent action then
          Agent.S.name_if_known agent_id agents
        else "someone"
      in
      let org =
        if Action.is_known `Known_org action then
          let org = Agent.S.to_org agents agent_id |> fun org_id -> Org.Map.find org_id orgs |> Org.G.name in
          " of the "^org
        else ""
      in
      let loc =
        if Action.is_known `Known_loc action then
          let loc = Loc.Map.find loc_id locs |> Loc.G.city in
          " in "^loc
        else ""
      in
      let time =
        if Action.is_known `Known_time action then
          let time = Time.print_month_day case.time in
          " on "^time
        else ""
      in
      Printf.sprintf "%s %s %s %s %s." event.text name org loc time

  | _ -> ""






