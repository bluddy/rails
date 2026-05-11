open! Containers
module C = Constants

type t = {
  crime_choice: Crime.Id.t;
  region: Region.t;
  locs: Loc.map;
  orgs: Org.map;
  mm: Agent.t;
  steps_done: Crime.Step.Set.t;
}

let create (srv:Services.t) ~last_crime_choice (w:World.t) =
  let crime_choice =
    if Difficulty.lowest w.difficulty && w.time_months = 0 then Crime.tutorial
    else
      let rec loop () =
        let crime_num = Crime.random srv.random in
        if Crime.Id.equal crime_num last_crime_choice then loop () else crime_num
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
    crime_choice;
    region;
    locs;
    orgs;
    mm;
    steps_done=Crime.Step.Set.empty;
  }

  (*
let choose_next_step (srv:Services.t) (v:t) =
  let rec loop n =
    let step = Crime.Step.random srv.random in
    if Crime.Step.Set.mem step v.steps_done then loop (n+1) else
    if Crime.has_step v.crime_chosen step
  in
  *)


