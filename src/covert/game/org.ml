open Ppx_yojson_conv_lib.Yojson_conv.Primitives

open Containers
module C = Constants
module Gen = Engine.My_gen
module String = Engine.String

module Id = Org_id
module Global_id = Engine.Int_id.Make() (* Global id among all orgs in game *)

type t = {
  global_id: Global_id.t option; (* only present for criminal orgs *)
  short_name: string;
  name: string;
  connect: int * int;
  strength: int;
  hq_build_cost: int;
  bits: int;
  agent_name_offset: int; (* offset into the name list *)
  activity: int;
  known_involved: bool;
} [@@deriving show, ord, yojson]

(* skip cops=0 by default *)
let random ?(start=1) r = Random.int_range start (C.num_orgs - 1) r |> Id.of_int

let local_contact = Id.of_int 0
let cia = Id.of_int 1

module Map = struct
  include Utils.Map.Make(struct
  type t = Id.t [@@deriving yojson, ord]
end)

  let of_simple_list l =
    List.mapi (fun i x -> Id.of_int i, x) l |> of_list
end

type map = t Map.t [@@deriving yojson]

module Set = Org_id.Set

module Global_set = struct
  include Utils.Set.Make(struct
  type t = Global_id.t [@@deriving yojson, ord]
end)
end

let global_id_of_id id orgs = (Map.find id orgs).global_id

let get_name_offset v = v.agent_name_offset
let get_global_id v = v.global_id
let get_bits v = v.bits

let connection orgs org1 org2 =
  let org1_d = Map.find org1 orgs in
  let org2_d = Map.find org2 orgs in
  Utils.classic_dist org1_d.connect org2_d.connect

let loc_connection orgs locs org loc =
  let org_d = Map.find org orgs in
  let loc_d = Loc.Map.find loc locs in
  Utils.classic_dist org_d.connect loc_d.Loc.connect

let randomize_connection r org =
  let x, y = org.connect in
  let dx = Random.int_range (-2) 2 r in
  let dy = Random.int_range (-2) 2 r in
  let x, y = x + dx, y + dy in
  {org with connect=(x, y)}

let from_stream num_orgs s =
  Iter.fold (fun acc _ ->
    let short_name = Gen.take 6 s |> Gen.to_stringi |> String.remove_nulls in
    let name = Gen.take 20 s |> Gen.to_stringi |> String.remove_nulls in
    let connect = Gen.get_wordi s in
    let connect = connect land 0x0F, (connect land 0xF0) lsr 4 in
    let strength = Gen.get_wordi s in
    let hq_build_cost = Gen.get_wordi s in
    let bits = Gen.get_wordi s in
    let global_id = bits land 0xFF in 
    let global_id = if global_id = 255 then None else Global_id.of_int global_id |> Option.some in
    let bits = bits lsr 8 in (* upper bits *)
    let agent_name_offset = Gen.get_bytei s in
    let _ = Gen.get_bytei s in
    let org = {
      short_name;
      name;
      connect;
      strength;
      hq_build_cost;
      bits;
      agent_name_offset;
      global_id;
      activity=0;
      known_involved=false;
    }
    in
    print_endline @@ show org;
    org::acc
  )
  []
  Iter.(0 -- (num_orgs - 1)) |> List.rev

let add_known v = {v with known_involved=true}

module S = struct

  let do_ org_id orgs fn =
    Map.update org_id (Option.map fn) orgs

  let add_known org_id orgs = do_ org_id orgs add_known

  let find_one_known orgs =
    Map.find_pred (fun _ org -> org.known_involved) orgs

  let get_name orgs org_id = (Map.find org_id orgs).name

  let show_connect orgs =
    let buf = Buffer.create 10 in
    Map.iter (fun i org ->
      Buffer.add_string buf @@ Printf.sprintf "%s %s: (%d, %d)\n" (Id.show i) (org.name) (fst org.connect) (snd org.connect))
    orgs;
    Buffer.contents buf

  let loc_fold f (orgs:map) (locs:Loc.map) acc =
    Map.fold (fun org_id org acc ->
      Loc.Map.fold (fun loc_id loc acc ->
        f org_id loc_id org loc acc)
      locs
      acc)
    orgs
    acc

end
