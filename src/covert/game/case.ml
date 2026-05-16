open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Containers
module C = Constants

type t = {
  crime: Crime.Id.t;
  region: Region.t;
  locs: Loc.map;
  orgs: Org.map;
  mm: Agent.t;
  failed_steps: Crime.Step.Set.t;
  step: Crime.Step.t;
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
    region;
    locs;
    orgs;
    mm;
    failed_steps=Crime.Step.Set.empty;
    step=Crime.Step.none;
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


