open Containers

module C = Constants

type case = {
  case_choice: int;
  locs: Loc.map;
  orgs: Org.map;
  mm: Agent.t;
}

type t = {
  caught_mms: Org.Global_set.t;
  gender: Gender.t;
  codename: string;
  difficulty: Difficulty.t;
}

(* First time *)
let default (info: Start_menu.info) = {
  caught_mms=Org.Global_set.empty;
  gender=info.gender;
  codename=info.name;
  difficulty=info.difficulty;
}

let create_case (srv:Services.t) ~last_case_choice v =
  let case_choice =
    if Difficulty.lowest v.difficulty then 0
    else
      let rec loop () =
        let case_num = Random.int C.num_cases srv.random in
        if case_num = last_case_choice then loop () else case_num
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

    match Org.global_id_of_id orgs mm_org with
    | Some g_id when not @@ Org.Global_set.mem g_id v.caught_mms ->
        region, locs, orgs, mm_org, mm_loc
    | _ -> loop (Some (region, locs, orgs)) (n+1)
  in
  let region, locs, orgs, mm_org, mm_loc = loop None 0 in
  let mm_agent = Mastermind.agent_of_org mm_org mm_loc orgs in
  ()

