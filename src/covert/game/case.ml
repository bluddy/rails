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

  (* dynamic in the case *)
  cur_loc: Loc.Id.t;
  cur_org: Org.Id.t;
  locs: Loc.map;
  orgs: Org.map;
  enemy_anxiety: int;
  double_agents: Loc.Set.t;
} [@@deriving yojson]

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

    cur_loc=Loc.washington;
    cur_org=Org.cia;
    locs;
    orgs;
    enemy_anxiety=0;
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
  let connection_to_cia org = Org.get_connection v.orgs org Org.cia in
  let ally_org =
    let rec loop n =
      let org = Org.random ~start:2 s.random in
      if n >= 999 then org else
      if connection_to_cia org > 10 then loop (n + 1)
      else org
    in
    loop 0
  in
  let enemy_org =
    let rec loop n =
      let org = Org.random s.random in
      if n >= 999 then org else
      if connection_to_cia org < 8 then loop (n + 1) else
      if Org.get_connection v.orgs org v.mm.org > 8 then loop (n + 1) else
      let org_d = Org.Map.find org v.orgs in
      if fst org_d.connect <= 4 then loop (n + 1) else
      org
    in
    loop 0
  in
  let enemy_org2 =
    let rec loop n =
      let org = Org.random s.random in
      if n >= 999 then org else
      if Org.get_connection v.orgs org enemy_org > 8 then loop (n+1) else
      let org_d = Org.Map.find org v.orgs in
      if fst org_d.connect <= 4 then loop (n + 1) else
      org
    in
    loop 0
  in
  let enemy_loc =
    let rec loop n =
      let loc = Loc.random s.random in
      if n >= 999 then loc else
      if Org.loc_connection v.orgs v.locs Org.cia loc < 8 then loop (n+1) else
      if Loc.Id.equal loc v.mm.loc then loop (n+1) else
      loc
    in
    loop 0
  in
  ()

