open Containers
module C = Constants
module Gen = Engine.My_gen

type t =
  | Europe
  | Africa
  | CentralAmerica
  [@@deriving enum, yojson]

let random r =
  Utils.Random.int C.num_regions r
  |> of_enum |> Option.get_exn_or "oops"

let load_from_file region =
  let filename = "WORLD" ^ (to_enum region |> Int.to_string) ^ ".DTA" in
  let s = Utils.stream_of_file filename in
  let num_locs = Gen.get_wordi s in
  let num_orgs = Gen.get_wordi s in
  let locs =
    Iter.fold (fun acc _ ->
      let city = s |> Gen.take 12 |> Gen.to_stringi in
      let country = s |> Gen.take 12 |> Gen.to_stringi in
      let connect = Gen.get_wordi s in
      let connect = connect land 0xF, connect land 0xF0 in
      let lawless = Gen.get_wordi s in 
      let known_buildings = Gen.get_wordi s in
      let some_buildings = Gen.get_wordi s in
      let x = Gen.get_bytei s in
      let y = Gen.get_bytei s in
      let loc = Location.{
        city;
        country;
        connect;
        lawless;
        known_buildings;
        some_buildings;
        loc=(x,y)}
      in
      loc::acc
    )
    []
    Iter.(0 -- (num_locs - 1))
    |> List.rev
  in
  locs

