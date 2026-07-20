open! Containers

type standard = [
  | `Known_face
  | `Known_agent
  | `Known_org
  | `Known_loc
  | `Known_role
][@@deriving enum]

type t = [
  | `Known_face
  | `Known_agent
  | `Known_org
  | `Known_loc
  | `Known_role
  | `Known_extra
  | `Known_jailbreak
  | `Known_rank
  | `Known_recruit_loc
  | `Known_recruited_by
][@@deriving eq, enum, ord, yojson, show]

let to_base2 v = 1 lsl (to_enum v)

let clue_discover_vals = [
    0; 1; 1; 2;
    1; 2; 2; 3;
    1; 2; 2; 3;
    2; 3; 3; 4;
    1; 2; 2; 3;
    2; 3; 3; 4;
    2; 3; 3; 4;
    3; 4; 4; 5;
  ] |> Array.of_list

let standard = [`Known_loc; `Known_org; `Known_agent; `Known_face; `Known_role]

let random ?(max=`Known_role) r =
  let max = to_enum max in
  Random.int (max + 1) r
  |> of_enum |> Option.get_exn_or "oops"

let random_standard r = Random.choose_return standard r

module Set = struct
  include Utils.Set.Make(struct
    type data = t [@@deriving yojson]
    type t = data [@@deriving yojson]
    let compare = compare
  end)

  let to_base2 v = fold (fun x acc -> acc + to_base2 x) v 0

  let known_discover_set = standard |> of_list

  let to_discover_val v =
    let base2 = to_base2 v in
    clue_discover_vals.(base2)

  let all_standard v = mem_all standard v

end

