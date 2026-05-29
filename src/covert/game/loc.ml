open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Containers
module Gen = Engine.My_gen
module String = Engine.String
module C = Constants

type t = {
  city: string;
  country: string;
  connect: int * int;
  lawless: int;
  known_buildings: int;
  some_buildings: int;
  loc: int * int;
  activity: int;
} [@@deriving show, yojson]

module Id = Engine.Int_id.Make()

let random r = Random.int C.num_locs r |> Id.of_int

let washington = Id.of_int 1

module Map = struct
  include Utils.Map.Make(struct
  type t = Id.t [@@deriving yojson]
  let compare = Id.compare
end)

  let of_simple_list l =
    List.mapi (fun i x -> Id.of_int i, x) l |> of_list
end

type map = t Map.t [@@deriving yojson]

module Set = Utils.Set.Make(struct
  type t = Id.t [@@deriving yojson]
  let compare = Id.compare
end)

let connection locs loc1 loc2 =
  let loc1_d = Map.find loc1 locs in
  let loc2_d = Map.find loc2 locs in
  Utils.classic_dist loc1_d.connect loc2_d.connect

let from_stream num_locs s =
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
    let loc = {
      city;
      country;
      connect;
      lawless;
      known_buildings;
      some_buildings;
      loc=(x,y);
      activity=0;
    }
    in
    print_endline @@ show loc;
    loc::acc
  )
  []
  Iter.(0 -- (num_locs - 1)) |> List.rev

