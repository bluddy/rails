open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Containers

type kind =
  | Hideout
  | Agent
  | Safehouse
  | Active_cel
  | Office
  [@@deriving yojson]

let kind_of_enum = function
  | 1 -> Some Hideout
  | 2 -> Some Agent
  | 3 -> Some Safehouse
  | 4 -> Some Active_cel
  | 5 -> Some Office
  | _ -> None

let enum_of_kind = function
  | None -> 0
  | Some Hideout -> 1
  | Some Agent -> 2
  | Some Safehouse -> 3
  | Some Active_cel -> 4
  | Some Office -> 5

type known = [
  `Known_org
] [@@deriving yojson, ord]

module KSet= Utils.Set.Make(struct
  type t = known [@@deriving yojson]
  let compare = compare_known
end)

type t = {
  org: Org.Id.t;
  loc: Loc.Id.t;
  known: KSet.t;
  num_wiretaps: int;
  rooms: int; (* Needs fleshing out *)
} [@@deriving yojson]

let create org_id loc_id =
  {
    org=org_id;
    loc=loc_id;
    known=KSet.empty;
    num_wiretaps=0;
    rooms=0;
  }

(* Unlike the OG, we use an (org, loc) map.
   There are no building IDs.
 *)

module Map = Utils.Map.Make(struct
  type t = Org.Id.t * Loc.Id.t [@@deriving yojson, ord]
end)

type map = t Map.t [@@deriving yojson]

let add_known known v =
  {v with known=KSet.add known v.known}

let get_kind org_id loc_id locs orgs roles agents mm world =
  let dist = Org.loc_connection orgs locs org_id loc_id in
  let loc = Loc.Map.find loc_id locs in
  let org = Org.Map.find org_id orgs in
  let hq_type = loc.lawless + 4 * org.strength / (dist + 1) in
  let hq_type = if hq_type > 0 then (6 * hq_type) / org.hq_build_cost + 1 else hq_type in
  let hq_type = kind_of_enum hq_type in
  let hq_type =
    if Loc.Id.equal loc_id mm.Agent.loc && Org.Id.equal org_id mm.org then Some Hideout
    else hq_type
  in
  (* Hardcoded *)
  let hq_type = match hq_type, world.World.difficulty with
  | None, Difficulty.Local_disturbance when Loc.Id.(loc_id = Loc.washington) ->
      begin match Agent.S.of_role Role.first roles agents
        |> Option.map (fun a -> a.Agent.org) with
      | Some org when Org.Id.(org = org_id) -> Some Hideout
      | _ -> None
      end
  | x, _ -> x
  in
  hq_type

let known_to_org org1_id org2_id loc_id locs orgs roles agents mm world =
  let org_loc_dist = Org.loc_connection orgs locs org1_id loc_id in
  let org_dist = Org.connection orgs org1_id org2_id in
  let dist = (org_dist * 3) / 2 + org_loc_dist in
  let hq_type = get_kind org2_id loc_id locs orgs roles agents mm world
  |> enum_of_kind in
  hq_type * 4 + 16 > dist


module S = struct
  let get org_id loc_id hqs =
    Map.get (org_id, loc_id) hqs

  let get_or_gen org_id loc_id hqs =
    match get org_id loc_id hqs with
    | Some hq -> hq, hqs
    | None ->
        let hq = create org_id loc_id in
        hq, Map.add (org_id, loc_id) hq hqs

  let do_update_ hq_id hqs fn =
    Map.update hq_id (Option.map fn) hqs

  let add_known hq_id known hqs =
    do_update_ hq_id hqs @@ add_known known

end

