open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

include Event_d

module Subst = Subst_engine
module Pat = Subst_engine.Pattern

(* Note: last event is Terminal kind *)

let check_tick v tick = match v.status with Tick t when t=tick -> true | _ -> false

let gen_msg_num crime step v =
  let event_idx = v.num_id in
  let type_ = Crime.Step.get_type crime step in
  Printf.sprintf "*MSG%02d%02d" event_idx type_

let to_text text_dta crime step roles agents orgs locs v =
  let victim, obj = Crime.Step.get_victim_and_obj crime step in
  let pats = [Pat.Victim, victim; Pat.Object, obj] in
  let agent_id = v.role |> Role.S.to_agent roles in
  let org = agent_id |> Agent.S.to_org agents |> fun id -> Org.Map.find id orgs in
  let pats = (Pat.SndOrg, org.Org.name)::pats in
  let loc = agent_id |> Agent.S.to_loc agents |> fun id -> Loc.Map.find id locs in
  let pats = (Pat.SndLoc, loc.Loc.city)::pats in
  let msg_num = gen_msg_num crime step v in
  let s = Subst.get_lines ~pat:msg_num text_dta |> Option.get_exn_or "Text not found" in
  Subst.subst_pat pats s

module S = struct

  let to_role events event_id = (Map.find event_id events).role

  let num v =
    try (Map.max_binding v |> fst |> Id.to_int) + 1 with Not_found -> 0

  let random r v = Random.int (num v) r |> Id.of_int

  let update event_id fn v = Map.update event_id (Option.map fn) v

  let with_same_num_id event_id v =
    let event = Map.find event_id v in
    let num_id = event.num_id in
    Map.filter (fun _ event -> event.num_id = num_id) v |> Map.to_list

  (* Emulate this test in code *)

  let check_process_event event_id roles agents ~num_actions v =
    let event_has_prev_ready_same_role_ event_id v =
      let event = Map.find event_id v in
      let rec loop n =
        if Id.(event_id = n) then false else
        let event2 = Map.find n v in
        if is_ready event2 && Role.Id.(event.role = event2.role) then true
        else loop @@ Id.of_int (Id.to_int n+1)
      in
      loop @@ Id.of_int 0
    in
    let event = Map.find event_id v in
    let cant_run = (`None, `Cant_run) in
    match event.status with
    | Ready ->
        let agent_id, agent = Agent.S.of_role roles agents event.role in
        if Agent.is_double_agent agent && event.efficiency = 0 then cant_run else
        if event_has_prev_ready_same_role_ event_id v then cant_run else
        begin match event.kind with
        | With_role {role; _} ->
          let role_id = role in
          let rcv_agent_id, rcv_agent = Agent.S.of_role roles agents role in
          let flag = match rcv_agent.status with
            | Agent.At_large _ -> false
            | Agent.Double_agent -> false
            | _ when not event.incapacitated_ok -> false
            | _ -> true
          in
          let problem_event =
            Map.find_pred (fun _ event2 ->
              Role.Id.(event2.role = role_id) && event2.num_id <> event.num_id && is_ready event2)
              v
          in
          if Option.is_some problem_event && not flag then cant_run else
          let event_to_run =
            Map.find_pred (fun _ event2 ->
              Role.Id.(event2.role = role_id) && event2.num_id = event.num_id)
              v
            |> function
            | Some event_id -> `Must_run event_id
            | None -> `Cant_run
          in

          (* NOTE: In OG, there was a bug here that prevented an agent from
             coming out of hiding due to the order: we checked arrested first *)
          begin match agent.status, rcv_agent.status with
          | In_hiding, Arrested -> (`Come_out_of_hiding(agent_id), event_to_run)
          | Arrested, In_hiding -> (`Come_out_of_hiding(rcv_agent_id), event_to_run)
          | Agent.Arrested, _ -> (`Check_escape(event_id, agent_id), `Cant_run)
          | _, Agent.Arrested -> (`Check_escape(event_id, rcv_agent_id), `Cant_run)
          | _ -> (`None, event_to_run)
          end

        | Misc | Terminal ->
            if num_actions > 5 then
              begin match agent.status with
              | At_large _ | Double_agent -> (`None, `Ok)
              | _ -> (`Check_escape(event_id, agent_id), `Cant_run)
              end
            else cant_run
        end

    | _ -> cant_run

end
