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

module Id = Engine.Int_id.Make()

module Map = Utils.Map.Make(struct
  type t = Id.t [@@deriving yojson]
  let compare = Id.compare
end)

type map = t Map.t [@@deriving yojson]

let add_known known v =
  {v with known=KSet.add known v.known}

module S = struct
  let get org_id loc_id hqs =
    Map.find_pred (fun _ hq ->
      Loc.Id.(hq.loc = loc_id) && Org.Id.(hq.org = org_id))
    hqs

  let get_or_gen org_id loc_id hqs =
    match get org_id loc_id hqs with
    | Some hq_id -> hq_id, hqs
    | None ->
        let hq = create org_id loc_id in
        let id = Map.cardinal hqs |> Id.of_int in
        id, Map.add id hq hqs

  let do_update_ hq_id hqs fn =
    Map.update hq_id (Option.map fn) hqs

  let add_known hq_id known hqs =
    do_update_ hq_id hqs @@ add_known known

end

