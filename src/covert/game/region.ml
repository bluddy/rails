open Containers
module C = Constants
module Gen = Engine.My_gen
module String = Engine.String

type t =
  | Europe
  | Africa
  | Central_america
  [@@deriving enum, yojson]

let random r =
  Utils.Random.int C.num_regions r
  |> of_enum |> Option.get_exn_or "oops"

let load_from_file region =
  let filename = Printf.sprintf "./data/covert/WORLD%d.DTA" (to_enum region) in
  let s = Utils.stream_of_file filename in
  let num_locs = Gen.get_wordi s in
  let num_orgs = Gen.get_wordi s in
  let locs = Loc.from_stream num_locs s in
  let orgs = Org.from_stream num_orgs s in
  locs, orgs

