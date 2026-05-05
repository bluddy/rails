open Containers

module C = Constants

type t = {
  gender: Gender.t;
  codename: string;
  difficulty: Difficulty.t;
  case_choice: int;
}

let create (srv:Services.t) ~last_case_choice (info:Start_menu.info) =
  let crime_choice =
    if Difficulty.lowest info.difficulty then 0
    else
      let rec loop () =
        let case_num = Random.int C.num_cases srv.random in
        if case_num = last_case_choice then loop ()
        else case_num
      in
      loop ()
  in
  let region = Region.random srv.random in
  let locs, orgs = Region.load_from_file region in
  ()
