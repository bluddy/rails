open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Containers

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
  let get org_id loc_id bldgs =
    Map.find_pred (fun _ bldg ->
      Loc.Id.(bldg.loc = loc_id) && Org.Id.(bldg.org = org_id))
    bldgs

  let get_or_gen org_id loc_id bldgs =
    match get org_id loc_id bldgs with
    | Some bldg_id -> bldg_id, bldgs
    | None ->
        let bldg = create org_id loc_id in
        let id = Map.cardinal bldgs |> Id.of_int in
        id, Map.add id bldg bldgs

  let do_update_ bldg_id bldgs fn =
    Map.update bldg_id (Option.map fn) bldgs

  let add_known bldg_id known bldgs =
    do_update_ bldg_id bldgs @@ add_known known

end

