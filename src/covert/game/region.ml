open Containers
module C = Constants

type t =
  | Europe
  | Africa
  | CentralAmerica
  [@@deriving enum, yojson]

let random r =
  Utils.Random.int C.num_regions r
  |> of_enum |> Option.get_exn_or "oops"
