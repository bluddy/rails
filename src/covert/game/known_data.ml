open! Containers

type data = [
  | `Known_recruit_loc
  | `Known_recruited_by
  | `Known_loc
  | `Known_org
  | `Known_rank
  | `Known_name
  | `Known_photo
  | `Known_extra
  | `Known_involved
  | `Known_jailbreak
][@@deriving eq,ord, yojson, show]

module Set = Utils.Set.Make(struct
  type t = data [@@deriving yojson]
  let compare = compare_data
end)
