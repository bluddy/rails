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
} [@@deriving yojson]

let agent_of_role v role_id =
  let role = Role.Map.find role_id v.roles in
  let agent_id = role.agent in
  let agent = Agent.Map.find agent_id v.agents in
  agent

let calc_hq_type v org_id loc_id =
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
  let hq_type = match hq_type, v.world.difficulty with
  | None, Difficulty.Local_disturbance when
      Loc.Id.(loc_id = Loc.washington) &&
      Org.Id.((agent_of_role v Role.first).org = org_id) -> Some Hq.Hideout
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

    match Org.global_id_of_id orgs mm_org with
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

let create_data (s:Services.t) world (v:t) =
  let typ = Crime.Step.get_type v.crime v.step in
  let roles, events = Crime.load_from_file typ in
  let roles = Role.Map.of_ordered_list roles in
  let events = Role.Map.of_ordered_list events in
  let diff_num = Difficulty.to_enum world.World.difficulty in
  let double_agents =
    Iter.fold (fun acc i ->
      let loc = Loc.random s.random in
      Loc.Set.add loc acc)
    Loc.Set.empty
    Iter.(0 -- (diff_num-1))
    |> Loc.Set.remove Loc.washington
  in
  let agents = Agent.Map.empty in
  let connection_to_cia org = Org.connection v.orgs org Org.cia in

  let gen_org ?start test =
    let rec loop n =
      let org = Org.random ?start s.random in
      if n >= 999 then org else
      if test org then loop (n+1)
      else org
    in
    loop 0
  in
  let gen_loc test =
    let rec loop n =
      let loc = Loc.random s.random in
      if n >= 999 then loc else
      if test loc then loop (n+1)
      else loc
    in
    loop 0
  in
  let ally_org =
    gen_org ~start:2 @@ fun org -> connection_to_cia org > 10
  in
  let enemy_org =
    gen_org @@ fun org ->
      (connection_to_cia org < 8) ||
      (Org.connection v.orgs org v.mm.org > 8) ||
      (let org_d = Org.Map.find org v.orgs in fst org_d.connect <= 4)
  in
  let enemy_org2 =
    gen_org @@ fun org ->
      (Org.connection v.orgs org enemy_org > 8) ||
      (let org_d = Org.Map.find org v.orgs in fst org_d.connect <= 4)
  in
  let enemy_loc = gen_loc @@ fun loc ->
    (Org.loc_connection v.orgs v.locs Org.cia loc < 8) ||
    (calc_hq_type v enemy_org loc |> Option.is_none) ||
    (Loc.Id.(loc = v.mm.loc))
  in
  let enemy_loc2 = gen_loc @@ fun loc ->
    (Loc.connection v.locs enemy_loc loc > 12) ||
    (calc_hq_type v enemy_org2 loc |> Option.is_none) ||
    (Loc.Id.(loc = v.mm.loc))
  in
  let ally_loc = gen_loc @@ fun loc ->
    (Org.loc_connection v.orgs v.locs Org.cia loc > 10) ||
    (calc_hq_type v ally_org loc |> Option.is_none)
  in
  ()

