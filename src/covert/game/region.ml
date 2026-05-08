open Containers
module C = Constants
module Gen = Engine.My_gen
module String = Engine.String

type t =
  | Europe
  | Africa
  | CentralAmerica
  [@@deriving enum, yojson]

let random r =
  Utils.Random.int C.num_regions r
  |> of_enum |> Option.get_exn_or "oops"

let load_from_file region =
  let filename = "./data/covert/WORLD" ^ (to_enum region |> Int.to_string) ^ ".DTA" in
  let s = Utils.stream_of_file filename in
  let num_locs = Gen.get_wordi s in
  let num_orgs = Gen.get_wordi s in
  let locs =
    Iter.fold (fun acc _ ->
      let city = s |> Gen.take 12 |> Gen.to_stringi |> String.remove_nulls in
      let country = s |> Gen.take 12 |> Gen.to_stringi |> String.remove_nulls in
      let connect = Gen.get_wordi s in
      let connect = connect land 0xF, connect land 0xF0 in
      let lawless = Gen.get_wordi s in 
      let known_buildings = Gen.get_wordi s in
      let some_buildings = Gen.get_wordi s in
      let x = Gen.get_bytei s in
      let y = Gen.get_bytei s in
      let loc = Loc.{
        city;
        country;
        connect;
        lawless;
        known_buildings;
        some_buildings;
        loc=(x,y)}
      in
      print_endline @@ Loc.show loc;
      loc::acc
    )
    []
    Iter.(0 -- (num_locs - 1)) |> List.rev
  in
  let orgs =
    Iter.fold (fun acc _ ->
      let short_name = Gen.take 6 s |> Gen.to_stringi |> String.remove_nulls in
      let name = Gen.take 20 s |> Gen.to_stringi |> String.remove_nulls in
      let connect = Gen.get_wordi s in
      let connect = connect land 0xF, connect land 0xF0 in
      let strength = Gen.get_wordi s in
      let hq_build_cost = Gen.get_wordi s in
      let bits = Gen.get_wordi s in
      let global_id = bits land 0xFF in 
      let global_id = if global_id = 255 then None else Org.Global_id.of_int global_id |> Option.some in

      let agent_name_offset = Gen.get_bytei s in
      let _ = Gen.get_bytei s in
      let org = Org.{
        short_name;
        name;
        connect;
        strength;
        hq_build_cost;
        bits;
        agent_name_offset;
        global_id;
      }
      in
      print_endline @@ Org.show org;
      org::acc
    )
    []
    Iter.(0 -- (num_orgs - 1)) |> List.rev
  in
  locs, orgs

