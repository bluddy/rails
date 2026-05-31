open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Containers
module C = Constants

type t = {
  (* constant in the case *)
  crime: Crime.Id.t;
  failed_steps: Crime.Step.Set.t;
  step: Crime.Step.t;
  region: Region.t;
  mm: Agent.t;
  world: World.t;

  (* dynamic in the case *)
  cur_loc: Loc.Id.t;
  cur_org: Org.Id.t;
  locs: Loc.map;
  orgs: Org.map;
  enemy_anxiety: int;
  double_agents: Loc.Set.t;
  roles: Role.map;
  agents: Agent.map;
  events: Event.map;
} [@@deriving yojson]

let agent_of_role v role_id =
  try
    let role = Role.Map.find role_id v.roles in
    let agent_id = role.agent in
    let agent = Agent.Map.find agent_id v.agents in
    Some agent
  with
  Not_found -> None

let hq_type v org_id loc_id =
  let dist = Org.loc_connection v.orgs v.locs org_id loc_id in
  let loc = Loc.Map.find loc_id v.locs in
  let org = Org.Map.find org_id v.orgs in
  let hq_type = loc.lawless + 4 * org.strength / (dist + 1) in
  let hq_type = if hq_type > 0 then (6 * hq_type) / org.hq_build_cost + 1 else hq_type in
  let hq_type = Hq.of_enum hq_type in
  let hq_type =
    if Loc.Id.equal loc_id v.mm.loc && Org.Id.equal org_id v.mm.org then Some Hq.Hideout
    else hq_type
  in
  (* Hardcoded *)
  let hq_type = match hq_type, v.world.difficulty with
  | None, Difficulty.Local_disturbance when Loc.Id.(loc_id = Loc.washington) ->
      begin match agent_of_role v Role.first
        |> Option.map (fun a -> a.Agent.org) with
      | Some org when Org.Id.(org = org_id) -> Some Hq.Hideout
      | _ -> None
      end
  | x, _ -> x
  in
  hq_type

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
    crime=crime_choice;
    failed_steps=Crime.Step.Set.empty;
    step=Crime.Step.none;
    region;
    mm;
    double_agents=Loc.Set.empty;
    world=w;

    cur_loc=Loc.washington;
    cur_org=Org.cia;
    locs;
    orgs;
    enemy_anxiety=0;
    roles=Role.Map.empty;
    agents=Agent.Map.empty;
    events=Event.Map.empty;
  }

let choose_next_step_ (srv:Services.t) (v:t) =
  let rec loop n =
    if n > 999 then None else
    let step = Random.int 6 srv.random |> Crime.Step.of_int in
    if not @@ Crime.Step.is_valid v.crime step then loop (n+1) else
    if Crime.Step.is_last v.crime step && (
      let all_steps = Crime.Step.get_all v.crime |> Crime.Step.Set.of_list in
      let all_steps_less_failed_steps = Crime.Step.Set.diff all_steps v.failed_steps in
      let num_steps_less_failed_steps = Crime.Step.Set.cardinal all_steps_less_failed_steps in
      num_steps_less_failed_steps > 1) then loop (n+1) else
    match v.step with
    | s when Crime.Step.equal step s && n >= 98 -> loop (n+2)
    | _ -> Some step in
  loop 0

  (* When we just want to pick the next step, but might need to regen if we fail *)
let step_and_recreate_if_needed (srv:Services.t) w v =
  let last_crime_choice = v.crime in
  let rec loop v =
    match choose_next_step_ srv v with
    | Some step ->
        assert (not @@ Crime.Step.equal step Crime.Step.none);
        {v with step}
    | None ->
        let v = create ~last_crime_choice srv w in
        loop v
  in
  loop v

  (* Check if we failed all the other steps other than the one we're doing *)
let failed_other_steps (v:t) =
  assert (not @@ Crime.Step.equal v.step Crime.Step.none);
  let all = Crime.Step.get_all v.crime |> Crime.Step.Set.of_list in
  let remove_failed = Crime.Step.Set.diff all v.failed_steps in
  let remove_cur = Crime.Step.Set.remove v.step remove_failed in
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
          if (Org.connection v.orgs org_id chosen.enemy_org1 > 12 ||
            (Org.Map.find org_id v.orgs).connect |> fst <= 4 ||
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
          if Loc.connection v.locs loc_id chosen.enemy_loc1 < 8 then None else Some loc_id
    in
    if Option.is_none org_id || Option.is_none loc_id then loop (n+1) else
    let org_id, loc_id =
      let o, l = Option.get_exn_or "oops" org_id, Option.get_exn_or "oops" loc_id in
      (* Printf.printf "org_id %s, loc_id %s\n" (Org.Id.show o) (Loc.Id.show l); *)
      o, l
    in
    if hq_type v org_id loc_id |> Option.is_none then loop (n+1) else
    (* Check mastermind *)
    let is_mm = Role.mastermind_bit role in
    let org_id, loc_id = if is_mm then v.mm.org, v.mm.loc else org_id, loc_id in
    if not is_mm && Loc.Id.(v.mm.loc = loc_id) && Org.Id.(v.mm.org = org_id) then loop (n+1) else
    let loc_id =
      (* hardcoded *)
      if Difficulty.lowest v.world.difficulty && Role.Id.(role_id = Role.first)
      then Loc.washington else loc_id
    in
    let agent_id, agents = Agent.get_or_gen s org_id loc_id ~mm_agent:v.mm agents v.orgs in
    (* check no role *)
    let agent = Agent.Map.find agent_id agents in
    if Role.Set.not_empty agent.roles then loop (n+1) else begin
    let agents = Agent.add_role agent_id role_id agents in
    (* If we know anything about the MM then we know the role of the mm *)
    let agents = if is_mm && Known_data.Set.not_empty agent.known
      then Agent.add_role_known agent_id role_id agents
      else agents
    in
    (* Some hardcoded stuff *)
    let agents = if Role.hardcoded_action_bit1 role then
        Agent.remove_known_data (Agent.Id.of_int 16) `Known_photo agents
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
  let typ = Crime.Step.get_type v.crime v.step in
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
  let connection_to_cia org = Org.connection v.orgs org Org.cia in

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
        (Org.connection v.orgs org v.mm.org > 8) ||
        (let org_d = Org.Map.find org v.orgs in fst org_d.connect <= 4)
    in
    let enemy_org2 =
      gen_org @@ fun org ->
        (Org.connection v.orgs org enemy_org1 > 8) ||
        (let org_d = Org.Map.find org v.orgs in fst org_d.connect <= 4)
    in
    let enemy_loc1 = gen_loc @@ fun loc ->
      (Org.loc_connection v.orgs v.locs Org.cia loc < 8) ||
      (hq_type v enemy_org1 loc |> Option.is_none) ||
      (Loc.Id.(loc = v.mm.loc))
    in
    let enemy_loc2 = gen_loc @@ fun loc ->
      (Loc.connection v.locs enemy_loc1 loc > 12) ||
      (hq_type v enemy_org2 loc |> Option.is_none) ||
      (Loc.Id.(loc = v.mm.loc))
    in
    let ally_loc = gen_loc @@ fun loc ->
      (Org.loc_connection v.orgs v.locs Org.cia loc > 10) ||
      (hq_type v ally_org loc |> Option.is_none)
    in
    { enemy_org1; enemy_org2; ally_org; enemy_loc1; enemy_loc2; ally_loc }
  in
  let roles, agents, orgs =
    let rec loop orgs n =
      if n > 1000 then failwith "Failed to assign roles to agents" else
      let chosen = choose_orgs_locs () in
      let orgs = Org.Map.map (Org.randomize_connection s.random) orgs in
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
    loop v.orgs 0
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
  {v with orgs; roles; agents; events; double_agents}

