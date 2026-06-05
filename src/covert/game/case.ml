open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Containers
module C = Constants

include Case_d

let hq_kind (v:t) org_id loc_id =
  Hq.get_kind org_id loc_id v.d.locs v.d.orgs v.d.roles v.d.agents v.s.mm v.world

let hq_known_to_org org1_id org2_id loc_id v =
  Hq.known_to_org org1_id org2_id loc_id v.d.locs v.d.orgs v.d.roles v.d.agents v.s.mm v.world

let create (srv:Services.t) ?last_crime_choice (w:World.t) =
  let crime_choice =
    if Difficulty.lowest w.difficulty && w.time_months = 0 then Crime.tutorial
    else
      let rec loop () =
        let crime_num = Crime.random srv.random in
        match last_crime_choice with
        | Some crime when Crime.Id.equal crime_num crime -> loop ()
        | _ -> crime_num
      in
      loop ()
  in
  let rec loop gen_data n =
    (* should only reload region on try%8 *)
    let region, locs, orgs = match gen_data with
    | Some (region, locs, orgs) when n mod 8 <> 0 -> region, locs, orgs
    | _ ->
      (* Regenerate *)
      let region = Region.random srv.random in
      let locs, orgs = Region.load_from_file region in
      let locs, orgs = Loc.Map.of_simple_list locs, Org.Map.of_simple_list orgs in
      region, locs, orgs
    in

    let mm_org = Org.random srv.random ~start:4 in
    let mm_loc = Loc.random srv.random in

    let data = Some (region, locs, orgs) in

    match Org.global_id_of_id mm_org orgs with
    | Some g_id when not @@ Org.Global_set.mem g_id w.caught_mms ->
        let mm_agent = Mastermind.agent_of_org mm_org mm_loc orgs in
        if not @@ Crime.check_org_support crime_choice mm_org orgs
          then loop data (n+1)
        else
          region, locs, orgs, mm_agent

    | _ -> loop data (n+1)
  in
  let region, locs, orgs, mm = loop None 0 in
  {
    s={
      crime=crime_choice;
      failed_steps=Crime.Step.Set.empty;
      step=Crime.Step.none;
      region;
      mm;
    };
    world=w;

    time=Time.default;
    cur_loc=Loc.washington;
    cur_org=Org.cia;
    enemy_anxiety=0;
    d={
      double_agents=Loc.Set.empty;
      roles=Role.Map.empty;
      agents=Agent.Map.empty;
      events=Event.Map.empty;
      hqs=Hq.Map.empty;
      locs;
      orgs;
      actions=Action.Map.empty;
    };
  }

let choose_next_step_ (srv:Services.t) (v:t) =
  let rec loop n =
    if n > 999 then None else
    let step = Random.int 6 srv.random |> Crime.Step.of_int in
    if not @@ Crime.Step.is_valid v.s.crime step then loop (n+1) else
    if Crime.Step.is_last v.s.crime step && (
      let all_steps = Crime.Step.get_all v.s.crime |> Crime.Step.Set.of_list in
      let all_steps_less_failed_steps = Crime.Step.Set.diff all_steps v.s.failed_steps in
      let num_steps_less_failed_steps = Crime.Step.Set.cardinal all_steps_less_failed_steps in
      num_steps_less_failed_steps > 1) then loop (n+1) else
    match v.s.step with
    | s when Crime.Step.equal step s && n >= 98 -> loop (n+2)
    | _ -> Some step in
  loop 0

  (* When we just want to pick the next step, but might need to regen if we fail *)
let step_and_recreate_if_needed (srv:Services.t) w v =
  let last_crime_choice = v.s.crime in
  let rec loop v =
    match choose_next_step_ srv v with
    | Some step ->
        assert (not @@ Crime.Step.equal step Crime.Step.none);
        {v with s={v.s with step}}
    | None ->
        let v = create ~last_crime_choice srv w in
        loop v
  in
  loop v

  (* Check if we failed all the other steps other than the one we're doing *)
let failed_other_steps (v:t) =
  assert (not @@ Crime.Step.equal v.s.step Crime.Step.none);
  let all = Crime.Step.get_all v.s.crime |> Crime.Step.Set.of_list in
  let remove_failed = Crime.Step.Set.diff all v.s.failed_steps in
  let remove_cur = Crime.Step.Set.remove v.s.step remove_failed in
  Crime.Step.Set.is_empty remove_cur

  (* Used only for creation *)
type chosen_ = {
  enemy_org1: Org.Id.t;
  enemy_org2: Org.Id.t;
  ally_org: Org.Id.t;
  enemy_loc1: Loc.Id.t;
  enemy_loc2: Loc.Id.t;
  ally_loc: Loc.Id.t;
} [@@deriving show]

  (* Given a role, fill it in *)
let make_agent_for_role_ (s:Services.t) role_id chosen roles agents (v:t) =
  let role = Role.Map.find role_id roles in
  Printf.printf "make_agent_for_role_ %s\nchosen: %s\n" (Role.Id.show role_id) (show_chosen_ chosen);
  let rec loop n =
    if n > 999 then None else
    let org_id = match Role.org_bit role with
      | `Org_enemy2 -> Some chosen.enemy_org2
      | `Org_enemy -> Some chosen.enemy_org1
      | `Org_ally -> Some chosen.ally_org
      | `Org_any ->
          let org_id = Org.random s.random in
          if (Org.connection v.d.orgs org_id chosen.enemy_org1 > 12 ||
            (Org.Map.find org_id v.d.orgs).connect |> fst <= 4 ||
            Org.Id.(org_id = Org.cia))
          then None
          else Some org_id
    in
    let loc_id = match Role.loc_bit role with
      | `Loc_enemy2 -> Some chosen.enemy_loc2
      | `Loc_enemy -> Some chosen.enemy_loc1
      | `Loc_ally -> Some chosen.ally_loc
      | `Loc_any ->
          let loc_id = Loc.random s.random in
          if Loc.connection v.d.locs loc_id chosen.enemy_loc1 < 8 then None else Some loc_id
    in
    if Option.is_none org_id || Option.is_none loc_id then loop (n+1) else
    let org_id, loc_id =
      let o, l = Option.get_exn_or "oops" org_id, Option.get_exn_or "oops" loc_id in
      (* Printf.printf "org_id %s, loc_id %s\n" (Org.Id.show o) (Loc.Id.show l); *)
      o, l
    in
    if hq_kind v org_id loc_id |> Option.is_none then loop (n+1) else
    (* Check mastermind *)
    let is_mm = Role.mastermind_bit role in
    let org_id, loc_id = if is_mm then v.s.mm.org, v.s.mm.loc else org_id, loc_id in
    if not is_mm && Loc.Id.(v.s.mm.loc = loc_id) && Org.Id.(v.s.mm.org = org_id) then loop (n+1) else
    let loc_id =
      (* hardcoded *)
      if Difficulty.lowest v.world.difficulty && Role.Id.(role_id = Role.first)
      then Loc.washington else loc_id
    in
    let agent_id, agents = Agent.S.get_or_gen s org_id loc_id ~mm_agent:v.s.mm agents v.d.orgs in
    (* check no role *)
    let agent = Agent.Map.find agent_id agents in
    if Role.Set.not_empty agent.roles then loop (n+1) else begin
    let agents = Agent.S.add_role agent_id role_id agents in
    (* If we know anything about the MM then we know the role of the mm *)
    let agents = if is_mm && Known_data.Set.not_empty agent.known
      then Agent.S.add_role_known agent_id role_id agents
      else agents
    in
    (* Some hardcoded stuff *)
    let agents = if Role.hardcoded_action_bit1 role then
        Agent.S.remove_known_data (Agent.Id.of_int 16) `Known_photo agents
      else
        agents
    in
    Some (agent_id, agents)
    end
  in
  match loop 0 with
  | None -> None
  | Some (agent_id, agents) ->
      let clue_seed = Random.int 32767 s.random in
      let roles = Role.Map.update role_id
        (Option.map (fun role -> Role.{role with clue_seed; agent=agent_id}))
        roles
      in
      Some (roles, agents)

let update_events_roles_agents (s:Services.t) world (v:t) =
  let typ = Crime.Step.get_type v.s.crime v.s.step in
  let roles, events = Crime.load_from_file typ in
  let roles = Role.Map.of_ordered_list roles in
  let events = Event.Map.of_ordered_list events in
  let diff_num = Difficulty.to_enum world.World.difficulty in
  let double_agents =
    Iter.fold (fun acc _ ->
      let loc = Loc.random s.random in
      Loc.Set.add loc acc)
    Loc.Set.empty
    Iter.(0 -- (diff_num-1))
    |> Loc.Set.remove Loc.washington
  in
  let connection_to_cia org = Org.connection v.d.orgs org Org.cia in
  let choose_orgs_locs () =
    let gen_org ?start test =
      Utils.try_do ~init:(fun () -> Org.random ?start s.random) test
    in
    let gen_loc test = Utils.try_do ~init:(fun () -> Loc.random s.random) test
    in
    let ally_org =
      gen_org ~start:2 @@ fun org -> connection_to_cia org > 10
    in
    let enemy_org1 =
      gen_org @@ fun org ->
        (connection_to_cia org < 8) ||
        (Org.connection v.d.orgs org v.s.mm.org > 8) ||
        (let org_d = Org.Map.find org v.d.orgs in fst org_d.connect <= 4)
    in
    let enemy_org2 =
      gen_org @@ fun org ->
        (Org.connection v.d.orgs org enemy_org1 > 8) ||
        (let org_d = Org.Map.find org v.d.orgs in fst org_d.connect <= 4)
    in
    let enemy_loc1 = gen_loc @@ fun loc ->
      (Org.loc_connection v.d.orgs v.d.locs Org.cia loc < 8) ||
      (hq_kind v enemy_org1 loc |> Option.is_none) ||
      (Loc.Id.(loc = v.s.mm.loc))
    in
    let enemy_loc2 = gen_loc @@ fun loc ->
      (Loc.connection v.d.locs enemy_loc1 loc > 12) ||
      (hq_kind v enemy_org2 loc |> Option.is_none) ||
      (Loc.Id.(loc = v.s.mm.loc))
    in
    let ally_loc = gen_loc @@ fun loc ->
      (Org.loc_connection v.d.orgs v.d.locs Org.cia loc > 10) ||
      (hq_kind v ally_org loc |> Option.is_none)
    in
    { enemy_org1; enemy_org2; ally_org; enemy_loc1; enemy_loc2; ally_loc }
  in
  let roles, agents, orgs =
    let rec loop orgs n =
      if n > 1000 then failwith "Failed to assign roles to agents" else
      let chosen = choose_orgs_locs () in
      let orgs = Org.Map.map (Org.randomize_connection s.random) orgs in
      print_endline @@ Org.S.show_connect orgs;
      let roles_agents =
        Role.Map.fold
          (fun role_id _ acc ->
            Option.bind acc
              (fun (roles, agents) ->
                make_agent_for_role_ s role_id chosen roles agents v))
          roles
          (Some (roles, Agent.Map.empty))
      in
      match roles_agents with
      | Some (roles, agents) -> roles, agents, orgs
      | None -> loop orgs (n+1)
    in
    loop v.d.orgs 0
  in
  let orgs =
    if Difficulty.(v.world.difficulty < Regional_conflict) then
      (* Reveal an involved org *)
      let role_id = Role.random s.random roles in
      let role = Role.Map.find role_id roles in
      let agent_id = role.agent in
      let agent = Agent.Map.find agent_id agents in
      Org.S.add_known agent.org orgs
    else
      orgs
  in
  {v with d={v.d with orgs; roles; agents; events; double_agents}}

let create_known_hqs (v:t) =
  let orgs, locs = v.d.orgs, v.d.locs in
  (* All hqs known to cia *)
  let hqs, orgs, locs =
    Org.S.loc_fold (fun org_id loc_id _ _ ((hqs, orgs, locs) as acc) ->
      let known = hq_known_to_org Org.cia org_id loc_id v in
      if known || Org.Id.(org_id = Org.local_contact) then
        let hq = Hq.create org_id loc_id |> Hq.add_known `Known_org in
        let hqs = Hq.Map.add (org_id, loc_id) hq hqs in
        let orgs = Org.S.add_known org_id orgs in
        let locs = Loc.S.add_known_hq loc_id org_id locs in
        hqs, orgs, locs
      else
        acc)
    orgs
    locs
    (Hq.Map.empty, orgs, locs)
  in
  let add_building org_id loc_id (hqs, locs) =
    match loc_id with
    | Some loc_id ->
        if Hq.Map.mem (org_id, loc_id) hqs then hqs, locs
        else
          let hq = Hq.create org_id loc_id |> Hq.add_known `Known_org in
          let hqs = Hq.Map.add (org_id, loc_id) hq hqs in
          let locs = Loc.S.add_known_hq loc_id org_id locs in
          hqs, locs
    | None -> hqs, locs
  in
  let min_dist_locs org_id =
    let _, min_loc, min_loc2 =
      Loc.Map.fold (fun loc_id _ ((min_dist, min_loc, _) as acc) ->
        match hq_kind v org_id loc_id with
        | None -> acc
        | Some _ ->
            let dist = Org.loc_connection v.d.orgs v.d.locs org_id loc_id in
            if dist < min_dist then (dist, Some loc_id, min_loc)
            else acc)
      locs
      (999, None, None)
    in
    min_loc, min_loc2
  in
  let count_known_to_cia org_id =
    Loc.Map.fold (fun loc_id _ ctr ->
      let known = hq_known_to_org Org.cia org_id loc_id v in
      if known then ctr + 1 else ctr)
    locs 0
  in
  (* Add closest buildings *)
  let hqs, locs =
    Org.Map.fold (fun org_id _ (hqs, locs) ->
      let min_loc, min_loc2 = min_dist_locs org_id in
      (hqs, locs)
      |> add_building org_id min_loc
      |> add_building org_id min_loc2)
    orgs
    (hqs, locs)
  in
  (* Check for too few buildings *)
  (* NOTE: looks like this doesn't do anything. We already did the min locs in prev iters
     and we're not changing anything - we're not going any 'deeper' in min dist
   *)
  let hqs, locs =
    Org.Map.fold (fun org_id _ ((hqs, locs) as acc) ->
      (* start from org = 4 for some reason *)
      if Org.Id.to_int org_id < 4 then acc else
      let min_loc, min_loc2 = min_dist_locs org_id in
      let count = count_known_to_cia org_id in
      (hqs, locs)
      |> (fun a -> if count < 2 then add_building org_id min_loc a else a)
      |> (fun a -> if count < 3 then add_building org_id min_loc2 a else a))
    orgs
    (hqs, locs)
  in
  {v with d={v.d with hqs; locs}}

let create_red_herrings (s:Services.t) (v:t) =
  let add_herring (agents, roles) _ =
    let org_id, loc_id = Utils.do_while
      (fun () ->
        let org_id = Utils.do_while
          (fun () -> Org.random s.random)
          (fun org_id -> Org.Id.(v.s.mm.org = org_id))
        in
        let loc_id = Utils.do_while
          (fun () -> Loc.random s.random)
          (fun loc_id -> hq_kind v org_id loc_id |> Option.is_none)
        in
        org_id, loc_id)
      (fun (org_id, loc_id) -> Agent.S.get org_id loc_id agents
        |> Option.is_some)
    in
    let agent_id, agents =
      Agent.S.get_or_gen s org_id loc_id agents v.d.orgs ~mm_agent:v.s.mm
    in
    let roles = Role.S.make_red_herring s.random agent_id v.d.events roles in
    agents, roles
  in
  let num_to_add = Difficulty.to_enum v.world.difficulty * 4
  in
  let agents, roles =
    Iter.fold add_herring
      (v.d.agents, v.d.roles)
      Iter.(0 -- (num_to_add - 1))
  in
  {v with d={v.d with agents; roles}}

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
    let v = {v with time; d={v.d with agents}; enemy_anxiety} in
    v
  else
    {v with time}


