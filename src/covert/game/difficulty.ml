open Containers

type t =
  | Local_disturbance
  | National_threat
  | Regional_conflict
  | Global_crisis
  [@@deriving enum]

let show = function
  | Local_disturbance -> "Local Disturbance"
  | National_threat -> "National Threat"
  | Regional_conflict -> "Regional Conflict"
  | Global_crisis -> "Global Crisis"

let list = Iter.map (fun i -> of_enum i |> Option.get_exn_or "oops") Iter.(0 -- 3)
  |> List.of_iter


