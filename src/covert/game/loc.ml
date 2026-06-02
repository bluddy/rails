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
  known_hqs: Org_id.Set.t;
  some_hqs: int;
  loc: int * int;
  activity: int;
} [@@deriving yojson]

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
    let connect = connect land 0xF, connect land 0xF0 lsr 4 in
    let lawless = Gen.get_wordi s in 
    let _known_hqs = Gen.get_wordi s in
    let some_hqs = Gen.get_wordi s in
    let x = Gen.get_bytei s in
    let y = Gen.get_bytei s in
    let loc = {
      city;
      country;
      connect;
      lawless;
      known_hqs=Org_id.Set.empty;
      some_hqs;
      loc=(x,y);
      activity=0;
    }
    in
    print_endline @@ (yojson_of_t loc |> Yojson.Safe.to_string);
    loc::acc
  )
  []
  Iter.(0 -- (num_locs - 1)) |> List.rev

let add_known_hq org_id v =
  {v with known_hqs=Org_id.Set.add org_id v.known_hqs}

module S = struct
  let update_ loc_id locs fn =
    Map.update loc_id (Option.map fn) locs

  let add_known_hq loc_id org_id v =
    update_ loc_id v (add_known_hq org_id)
end
