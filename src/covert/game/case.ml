open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Containers
module C = Constants

include Case_d

let hq_kind (v:t) org_id loc_id =
  Hq.get_kind org_id loc_id v.d.locs (orgs v) (roles v) (agents v) v.s.mm v.world

let hq_known_to_org org1_id org2_id loc_id v =
  Hq.known_to_org org1_id org2_id loc_id (locs v) (orgs v) (roles v) (agents v) v.s.mm v.world

let clear_autoescape v = {v with agent_autoescape=None}

let create_action time kind v = Action.S.create time kind (events v) (roles v) (agents v) (actions v)

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

let time_pass (s:Services.t) ?(force_tick=false) ~sleeping minutes (v:t) =
  let do_tick, time =
    let time = v.time in
    let time' = Time.update minutes time in
    Time.should_do_tick time time' || force_tick, time'
  in
  let v = {v with time} in
  if not do_tick then v, `None else
  let old_actions = actions v in
  let check_process_event event_id actions =
    let num_actions = Action.S.num actions in
    Event.S.check_process_event event_id (roles v) (agents v) ~num_actions @@ events v
  in
  let v =
    let time = Time.do_tick time in
    let factor = Difficulty.to_enum v.world.difficulty + 3 in
    let agents = Agent.S.reduce_anxiety factor v.d.agents in
    let enemy_anxiety = v.enemy_anxiety - v.enemy_anxiety/factor in
    let agent_autoescape = None in
    {v with time; d={v.d with agents}; enemy_anxiety; agent_autoescape}
  in
  let handle_msg msg v = match msg with
    | `Check_escape(_, agent_id) -> check_escape_jail s agent_id v
    | `Come_out_of_hiding agent_id ->
        let actions =
          Action.S.create v.time (Action.Agent_out_of_hiding agent_id) (events v) (roles v) (agents v) (actions v)
        in
        (* Note: OG didn't do this. *)
        let agents = Agent.S.update_agent agent_id (agents v) @@ Agent.go_free in
        {v with d={v.d with actions; agents}}
    | _ -> v
  in
  let v, event_run_cnt =
    Event.Map.fold_not_last (fun event_id _ (v, cnt) ->
      let ret = check_process_event event_id (actions v) in
      let v = handle_msg (fst ret) v in
      let cnt = match snd ret with `Cant_run -> cnt | _ -> cnt + 1 in
      v, cnt)
      (events v)
      (v, 0)
  in
  if Random.int 10 s.random >= Difficulty.to_enum v.world.difficulty + 4
    && Action.Map.cardinal (actions v) > 5
    && event_run_cnt > 0
    && not force_tick then v, `None else
  let v, event_run_cnt =
    if event_run_cnt <= Difficulty.to_enum v.world.difficulty then
      (* Update event_run_cnt *)
      let v = clear_autoescape v in
      let v, cnt, _ =
        Event.Map.fold_not_last (fun event_id _ ((v, cnt, can_run) as acc) ->
          if can_run then acc else
          let ret = check_process_event event_id (actions v) in
          let v = handle_msg (fst ret) v in
          let cnt, can_run = match snd ret with `Cant_run -> cnt, can_run | _ -> 1, true in
          v, cnt, can_run)
          (events v)
          (v, event_run_cnt, false)
      in
      v, cnt
    else
      v, event_run_cnt
  in
  (* If we already hid, higher chance of another agent hiding *)
  let rec agent_hiding_loop ~already_hid v =
    let rec find_random_agent n =
      if n >= 999 then None else
      let rand_role = Role.S.random_with_diff s.random (difficulty v) (roles v) in
      let rand_agent = Role.S.to_agent (roles v) rand_role  in
      match v.agent_autoescape with
      | Some id when Agent.Id.(id = rand_agent) -> find_random_agent (n+1)
      | _ -> Some rand_agent
    in
    match find_random_agent 0 with
    | Some agent_id when Action.Map.cardinal (actions v) > 8 ->
      let agent = Agent.Map.find agent_id (agents v) in
      if Loc.Id.(v.cur_loc <> agent.loc)
        || Org.Id.(v.cur_org <> agent.org)
        || sleeping then
        let role_done = Event.Map.find_pred (fun event_id event ->
          let role_agent_id = Event.S.to_role (events v) event_id |> Role.S.to_agent (roles v) in
          Agent.Id.(agent_id = role_agent_id) && Event.is_ready event)
          (events v)
          |> Option.is_none
        in
        if (role_done || event_run_cnt = 0 && Option.is_none v.agent_autoescape)
           && (already_hid
            || Agent.check_known [`Known_loc; `Known_org; `Known_name; `Known_photo] agent) then
            let agents = Agent.S.go_into_hiding agent_id (agents v) in
            let actions =
              let bulletin = Agent.is_known_any agent in
              let kind = Action.Agent_hide {agent=agent_id; bulletin} in
              create_action v.time kind v
            in
            let v = {v with d={v.d with agents; actions}} in
            agent_hiding_loop ~already_hid:true v
        else v, `GenSuccess
      else v, `GenSuccess
    | _ -> v, `GenFailed
  in
  let v, ret = agent_hiding_loop ~already_hid:false v in
  match ret with
  | `GenFailed when event_run_cnt = 0 -> v, `Case_over
  | _ when time.minutes > 40000 (* one month *) -> v, `Case_over
  | _ when force_tick -> v, `None
  | _ ->
  let rec random_event_loop v =
    let event_id = Event.S.random s.random @@ events v in
    let ret = check_process_event event_id @@ actions v in
    let v = handle_msg (fst ret) v in
    match snd ret with
    | `Cant_run -> random_event_loop v
    | `Ok -> failwith "don't know what to do with ok here" (* TODO *)
    | `Must_run run_event_id -> v, event_id, run_event_id
  in
  let v, event_id, run_event_id = random_event_loop v in
  let event = Event.Map.find event_id @@ events v in
  let set_tick_and_ctr_tick event_id v =
    let events = Event.S.update event_id (Event.set_tick time.tick) @@ events v in
    let role_id = Event.S.to_role events event_id in
    let roles = Role.S.update_ctr role_id (fun ctr -> match ctr.tick with
      | None -> {ctr with tick=Some time.tick}
      | _ -> ctr)
      @@ roles v
    in
    {v with d={v.d with roles; events}}
  in
  let v =
    if Event.has_role event then
      if event.incapacitated_ok &&
        (Event.S.to_role (events v) run_event_id
        |> Agent.S.of_role (roles v) (agents v) |> snd
        |> Agent.is_at_large |> not) then
          update_events (Event.S.update event_id @@ Event.set_tick 0) v
      else
        (* long path here *)
        let run_event = Event.Map.find run_event_id @@ events v in
        if Event.is_ready run_event then
          let v = v
            |> set_tick_and_ctr_tick event_id
            |> set_tick_and_ctr_tick run_event_id
          in
          let v =
            let events =
              Event.S.with_same_num_id event_id (events v)
              |> List.filter (fun (id, _) -> Event.Id.(id <> event_id && id <> run_event_id))
              |> List.fold_left (fun acc (id, _) ->
                  Event.S.update id (Event.update_status Unused) acc)
                (events v)
            in
            set_events events v
          in
          let event_to_agent event_id =
            Event.S.to_role (events v) event_id |> Role.S.to_agent (roles v)
          in
          let v =
            if Agent.Id.(event_to_agent event_id = event_to_agent run_event_id) then
              let events = (events v)
                |> Event.S.update event_id (Event.update_status Unused)
                |> Event.S.update event_id (Event.update_status Unused)
              in
              set_events events v
            else v
          in
          v
        else
          update_events (Event.S.update event_id @@ Event.update_status run_event.status) v
    else
      set_tick_and_ctr_tick event_id v
  in
  Event.Map.fold (fun id event acc ->
    if Event.check_tick event time.tick then
      let action = create_action time (Action.Event_based id) v in
      List.fold_left (fun acc item_id ->
      )
      acc
      v.item.rcv


    else acc)
  v.events
  v, `None


