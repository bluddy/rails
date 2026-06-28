open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Containers
module C = Constants
module Bul = Bulletin_d

open Case

let update_time minutes (v:t) =
  let time = Time.update minutes v.time in
  {v with time}

  (* This is the main time-waiting function. It processes many things
     that could happen while time passes
  *)
let do_tick (s:Services.t) ?(force_tick=false) ?(sleeping=false) (v:t) =
  let check_process_event event_id actions =
    let num_actions = Action.S.num actions in
    Event.S.check_process_event event_id (G.roles v) (G.agents v) ~num_actions @@ G.events v
  in
  let time = Time.do_tick v.time in
  let v =
    let factor = Difficulty.to_enum v.world.difficulty + 3 in
    let agents = Agent.S.reduce_anxiety_all factor @@ G.agents v in
    let enemy_anxiety = v.enemy_anxiety - v.enemy_anxiety/factor in
    let agent_autoescape = None in
    {v with time; d={v.d with agents}; enemy_anxiety; agent_autoescape}
  in
  let handle_msg msg v bs = match msg with
    | `Check_escape(_, agent_id) -> check_escape_jail s agent_id v, bs
    | `Come_out_of_hiding agent_id ->
        let actions = create_action v.time (Action.Agent_out_of_hiding agent_id) v (G.actions v) in
        let bs =
          let name = Agent.S.name_if_known agent_id (G.agents v) in
          Bul.Agent_out_of_hiding {name}::bs
        in
        (* Note: OG didn't do this. *)
        let agents = Agent.S.update agent_id Agent.go_free (G.agents v) in
        {v with d={v.d with actions; agents}}, bs
    | _ -> v, bs
  in
  let bs = [] in
  let v, bs, event_run_cnt =
    Event.Map.fold_not_last (fun event_id _ (v, bs, cnt) ->
      let ret = check_process_event event_id (G.actions v) in
      let v, bs = handle_msg (fst ret) v bs in
      let cnt = match snd ret with `Cant_run -> cnt | _ -> cnt + 1 in
      v, bs, cnt)
      (G.events v)
      (v, bs, 0)
  in
  if Random.int 10 s.random >= Difficulty.to_enum v.world.difficulty + 4
    && Action.Map.cardinal (G.actions v) > 5
    && event_run_cnt > 0
    && not force_tick then v, bs, `None else
  let v, bs, event_run_cnt =
    if event_run_cnt <= Difficulty.to_enum v.world.difficulty then
      (* Update event_run_cnt *)
      let v = clear_autoescape v in
      let v, cnt, bs, _ =
        Event.Map.fold_not_last (fun event_id _ ((v, cnt, bs, can_run) as acc) ->
          if can_run then acc else
          let ret = check_process_event event_id (G.actions v) in
          let v, bs = handle_msg (fst ret) v bs in
          let cnt, can_run = match snd ret with `Cant_run -> cnt, can_run | _ -> 1, true in
          v, cnt, bs, can_run)
          (G.events v)
          (v, event_run_cnt, bs, false)
      in
      v, bs, cnt
    else
      v, bs, event_run_cnt
  in
  let rec agent_hiding_loop ~already_hid v bs =
    (* If we already hid, higher chance of another agent hiding *)
    let rec find_random_agent n =
      if n >= 999 then None else
      let rand_role = Role.S.random_with_diff s.random (G.difficulty v) (G.roles v) in
      let rand_agent = Role.S.to_agent (G.roles v) rand_role  in
      match v.agent_autoescape with
      | Some id when Agent.Id.(id = rand_agent) -> find_random_agent (n+1)
      | _ -> Some rand_agent
    in
    match find_random_agent 0 with
    | Some agent_id when Action.Map.cardinal (G.actions v) > 8 ->
      let agent = Agent.Map.find agent_id (G.agents v) in
      if Loc.Id.(v.cur_loc <> agent.loc)
        || Org.Id.(v.cur_org <> agent.org)
        || sleeping then
        let role_done = Event.Map.find_pred (fun event_id event ->
          let role_agent_id = Event.S.to_role (G.events v) event_id |> Role.S.to_agent (G.roles v) in
          Agent.Id.(agent_id = role_agent_id) && Event.is_ready event)
          (G.events v)
          |> Option.is_none
        in
        if (role_done || event_run_cnt = 0 && Option.is_none v.agent_autoescape)
           && (already_hid
            || Agent.check_known [`Known_loc; `Known_org; `Known_name; `Known_photo] agent) then
            let agents = Agent.S.go_into_hiding agent_id (G.agents v) in
            let bs =
              let name = Agent.S.name_if_known agent_id agents in
              if Agent.is_known_any agent then Bul.Agent_hiding {name}::bs else bs
            in
            let actions =
              let kind = Action.Agent_hide agent_id in
              create_action v.time kind v @@ G.actions v
            in
            let v = {v with d={v.d with agents; actions}} in
            agent_hiding_loop ~already_hid:true v bs
        else v, bs, `GenSuccess
      else v, bs, `GenSuccess
    | _ -> v, bs, `GenFailed
  in
  let v, bs, ret = agent_hiding_loop ~already_hid:false v bs in
  match ret with
  | `GenFailed when event_run_cnt = 0 -> v, bs, `Case_over
  | _ when time.minutes > 40000 (* one month *) -> v, bs, `Case_over
  | _ when force_tick -> v, bs, `None
  | _ ->
  let rec random_event_loop v bs =
    let event_id = Event.S.random s.random @@ G.events v in
    let ret = check_process_event event_id @@ G.actions v in
    let v, bs = handle_msg (fst ret) v bs in
    match snd ret with
    | `Cant_run -> random_event_loop v bs
    | `Ok -> failwith "don't know what to do with ok here" (* TODO *)
    | `Must_run run_event_id -> v, event_id, run_event_id, bs
  in
  let v, event_id, run_event_id, bs = random_event_loop v bs in
  let event = Event.Map.find event_id @@ G.events v in
  let set_tick_and_ctr_tick event_id v =
    let events = Event.S.update event_id (Event.set_tick time.tick) @@ G.events v in
    let role_id = Event.S.to_role events event_id in
    let roles = Role.S.update_ctr role_id (fun ctr -> match ctr.tick with
      | None -> {ctr with tick=Some time.tick}
      | _ -> ctr)
      @@ G.roles v
    in
    {v with d={v.d with roles; events}}
  in
  let v =
    if Event.has_role event then
      if event.incapacitated_ok &&
        (Event.S.to_role (G.events v) run_event_id
        |> Agent.S.of_role (G.roles v) (G.agents v) |> snd
        |> Agent.is_at_large |> not) then
          update_events (Event.S.update event_id @@ Event.set_tick 0) v
      else
        (* long path here *)
        let run_event = Event.Map.find run_event_id @@ G.events v in
        if Event.is_ready run_event then
          let v = v
            |> set_tick_and_ctr_tick event_id
            |> set_tick_and_ctr_tick run_event_id
          in
          let v =
            let events =
              Event.S.with_same_num_id event_id (G.events v)
              |> List.filter (fun (id, _) -> Event.Id.(id <> event_id && id <> run_event_id))
              |> List.fold_left (fun acc (id, _) ->
                  Event.S.update id (Event.update_status Unused) acc)
                (G.events v)
            in
            U.events events v
          in
          let event_to_agent event_id =
            Event.S.to_role (G.events v) event_id |> Role.S.to_agent (G.roles v)
          in
          let v =
            if Agent.Id.(event_to_agent event_id = event_to_agent run_event_id) then
              let events = (G.events v)
                |> Event.S.update event_id (Event.update_status Unused)
                |> Event.S.update run_event_id (Event.update_status Unused)
              in
              U.events events v
            else v
          in
          v
        else
          update_events (Event.S.update event_id @@ Event.update_status run_event.status) v
    else
      set_tick_and_ctr_tick event_id v
  in
  let handle_agent_items event_id event actions items bs =
    let actions = create_action time (Action.Event_based event_id) v actions in
    let agent_id = Event.S.to_role (G.events v) event_id |> Role.S.to_agent (G.roles v) in
    let actions, items, bs =
      List.fold_left (fun (actions, items, bs) item_id ->
        let items = Item.S.update item_id items (Item.set_agent agent_id) in
        let bulletin = Random.int 2 s.random >= Difficulty.to_enum (G.difficulty v)
                      && Event.is_misc event
                      && Action.S.num (G.actions v) > 5
        in
        let loc = Agent.S.to_loc (G.agents v) agent_id in
        let bs = if bulletin then Bul.Item_spotted{item=item_id; loc}::bs else bs in
        let actions = create_action time (Action.Item_spotted(item_id, loc)) v actions
        in
        actions, items, bs)
      (actions, items, bs)
      event.items.rcv
    in
    let items = List.fold_left (fun items item_id ->
      Item.S.update item_id items @@ Item.clear_agent)
      items
      event.items.send
    in
    actions, items, bs
  in
  let handle_msg_bulletin event locs orgs actions roles agents bs =
    match event.Event.kind with
    | Terminal -> failwith "terminal event encountered" 
    | Misc ->
        let text, org_id = event_to_text s event v in
        let orgs = Org.S.add_known org_id orgs in
        let bs = (Bulletin_d.Text text)::bs in
        locs, orgs, bs
    | With_role {inter=Msg; rcv_role;_} when Action.S.num actions > 5 ->
        let loc_id = event.role |> Role.S.to_agent roles |> Agent.S.to_loc agents in
        let locs = Loc.S.incr_activity loc_id locs in
        let rand2 = Random.int 2 s.random in
        if rand2 = 0 then
          let bs = (Bulletin_d.Satellite_from loc_id)::bs in
          locs, orgs, bs
        else
          let rcv_loc_id = rcv_role |> Role.S.to_agent roles |> Agent.S.to_loc agents in
          let bs = (Bulletin_d.Satellite_to rcv_loc_id)::bs in
          locs, orgs, bs
    | _ -> locs, orgs, bs
  in
  let handle_reveal_action event_id event actions v bs =
    let agent_id = event.Event.role |> Role.S.to_agent (G.roles v) in
    let agent = Agent.Map.find agent_id (G.agents v) in
    let loc_id, org_id = agent.Agent.loc, agent.org in
    let hq, hqs = Hq.S.get_or_gen org_id loc_id (G.hqs v) in
    let reveal_action =
      Agent.is_double_agent agent ||
      not (Agent.is_arrested agent) && hq.num_wiretaps > Random.int 50 s.random 
    in
    (* TODO: may need action_event_based_print_summary *)
    let actions, agents, bs =
      if reveal_action then
        Action.Map.fold (fun action_id action ((actions, agents, bs) as acc) ->
          match action.Action.kind with
          | Event_based event_id2 when Event.Id.(event_id2 = event_id)
            && action.time = v.time.Time.minutes ->
              let actions = Action.S.update action_id Action.U.known_all actions in
              let double = Agent.is_double_agent agent in
              let agents =
                let add_known_role rcv_agent agents = match event.kind with
                  | With_role {rcv_role;_} ->
                      Agent.S.add_role_known rcv_agent rcv_role agents
                  | _ -> agents
                in
                match Action.G.rcv action with
                | Some {rcv_agent; _} when double ->
                    agents
                    |> Agent.S.add_known_data rcv_agent `Known_loc
                    |> Agent.S.add_known_data rcv_agent `Known_org
                    |> add_known_role rcv_agent
                | Some {rcv_agent; _} when Event.is_meeting event ->
                    add_known_role rcv_agent agents
                | _ -> agents
              in
              let src = if double then `Double_agent else `Wiretap in
              let agent = if double then `Gender(agent.gender) else `Name(Agent.S.name_if_known agent_id agents) in
              let event_text = if double || Event.is_meeting event then
                  event_to_text s event v |> fst |> Option.some
                else None
              in
              let bs = Bul.Action_reveal {src; agent; org=org_id; loc=loc_id; event_text}::bs in
              actions, agents, bs
          | _ -> acc)
        actions
        (actions, (G.agents v), bs)
      else
        actions, (G.agents v), bs
    in
    actions, agents, hqs, bs
  in
  let handle_arrival event_id event actions agents roles bs =
    let agent_id = event.Event.role |> Role.S.to_agent roles in
    match event.kind with
    | With_role {inter=Meet; tx=Rcv; _} when Agent.S.is_known `Known_photo agent_id agents ->
      Action.Map.fold (fun action_id action ((actions, bs) as acc) ->
        match action.Action.kind with
        | Event_based event_id2 when Event.Id.(event_id2 = event_id) &&
          not @@ Action.send_loc_eq_rcv_loc action ->
            let bs =
              let loc_home = Action.G.send_loc action in
              let loc_trip = Action.G.rcv_loc action in
              let name = Agent.S.name_if_known agent_id agents in
              Bul.Agent_visit {loc_home; loc_trip; name}::bs
            in
            let actions = Action.S.add_known [`Known_loc; `Known_time] action_id actions in
            actions, bs
        | _ -> acc)
      actions
      (actions, bs)
    | _ -> actions, bs
  in
  let v, bs =
    Event.Map.fold (fun event_id event ((v, bs) as acc) ->
      if Event.check_tick event time.tick then
        let actions, items, bs = handle_agent_items event_id event (G.actions v) (G.items v) bs in
        let locs, orgs, bs = handle_msg_bulletin event (G.locs v) (G.orgs v) (G.actions v) (G.roles v) (G.agents v) bs in
        let actions, agents, hqs, bs =
          if Event.has_role event then
            handle_reveal_action event_id event actions v bs
          else
            actions, (G.agents v), (G.hqs v), bs
        in
        let actions, bs = handle_arrival event_id event actions agents (G.roles v) bs in
        {v with d={v.d with actions; items; locs; orgs; agents; hqs}}, bs
      else acc)
      (G.events v)
      (v, bs)
  in
  let handle_agent_relocate agents roles actions bs =
    let agent_to_move =
      Role.Map.find_pred_v (fun role_id role ->
        let agent_id = Role.S.to_agent roles role_id in
        let agent = Agent.Map.find agent_id agents in
        if role.Role.can_relocate
          && Agent.is_known `Known_loc agent
          && Agent.G.anxiety agent > Random.int (128/(Difficulty.to_enum v.world.difficulty + 1)) s.random
          && Loc.Id.(v.cur_loc <> agent.loc) then
          let rec try_loop n =
            if n >= 4 then None else
            let dest_loc_id = Loc.random s.random in
            let hq_kind = hq_kind v (agent.org) dest_loc_id in
            if Option.is_none hq_kind then try_loop (n+1) else
            let remove_agent, ok = match Agent.S.get agent.org dest_loc_id agents with
              | Some problem_agent_id ->
                  let problem_agent = Agent.Map.find problem_agent_id agents in
                  if Agent.has_role problem_agent || Agent.is_known_any problem_agent then
                    None, false
                  else
                    Some problem_agent_id, true
              | None -> None, true
            in
            if not ok then try_loop (n+1)
            else
              Some (agent_id, dest_loc_id, remove_agent)
          in
          try_loop 0
        else None)
      roles
    in
    match agent_to_move with
    | None -> agents, actions, bs
    | Some (agent_id, dest_loc_id, agent_to_remove) ->
        let agents = match agent_to_remove with
         | Some agent_id -> Agent.Map.remove agent_id agents
         | None -> agents
        in
        let agent = Agent.Map.find agent_id agents in
        let old_loc_id = agent.loc in
        let agents = agents
          |> Agent.S.update agent_id
            (fun agent -> agent
            |> Agent.U.loc dest_loc_id
            |> Agent.remove_known_data `Known_loc
            |> Agent.reduce_anxiety 2)
        in
        let name = Agent.S.name_if_known agent_id agents in
        let bs = Bul.Agent_leave {name; old_loc=old_loc_id}::bs in
        let actions =
          let kind = Action.Agent_leave(agent_id, old_loc_id) in
          create_action v.time kind v actions
        in
        agents, actions, bs
  in
  let agents, actions, bs = handle_agent_relocate (G.agents v) (G.roles v) (G.actions v) bs in
  {v with d={v.d with agents; actions}}, bs, `None

let time_pass_big (s:Services.t) ?(force_tick=false) ?sleeping minutes (v:t) =
  let time = v.time in
  let v = update_time minutes v in
  let todo_tick = Time.should_do_tick time v.time || force_tick in
  if not todo_tick then
    v, [], `None
  else
    let v, bs, status = do_tick s ~force_tick ?sleeping v in
    v, List.rev bs, status

