open! Containers

type data = [
  | `Known_photo
  | `Known_name
  | `Known_org
  | `Known_loc
  | `Known_involved
  | `Known_extra
  | `Known_jailbreak
  | `Known_rank
  | `Known_recruit_loc
  | `Known_recruited_by
][@@deriving eq, ord, yojson, show]

let to_base2 = function
  | `Known_photo -> 1
  | `Known_name -> 2
  | `Known_org -> 4
  | `Known_loc -> 8
  | `Known_involved -> 0x10
  | `Known_extra -> 0x20
  | `Known_jailbreak -> 0x40
  | `Known_rank -> 0x100
  | `Known_recruit_loc -> 0x200
  | `Known_recruited_by -> 0x400

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

let standard = [`Known_loc; `Known_org; `Known_name; `Known_photo; `Known_involved]

module Set = struct
  include Utils.Set.Make(struct
    type t = data [@@deriving yojson]
    let compare = compare_data
  end)

  let to_base2 v = fold (fun x acc -> acc + to_base2 x) v 0

  let known_discover_set = [`Known_loc; `Known_org; `Known_name; `Known_photo; `Known_involved]
    |> of_list

  let to_discover_val v =
    let base2 = to_base2 v in
    clue_discover_vals.(base2)

  let all_standard v = mem_all standard v

end

