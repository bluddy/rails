open Containers

module C = Constants

type t = {
  gender: Gender.t;
  codename: string;
  difficulty: Difficulty.t;
  case_choice: int;
}

let create srv ~last_case_choice info =
  let crime_choice =
    if Difficulty.lowest info.difficulty then 0
    else
      let rec loop () =
        let case_num = Random.int C.num_cases in
        if case_num = v.case_choice then loop ()
        else case_num
      in
      loop ()
  in
  let new_region () =
    Region.random srv.random in
  in
