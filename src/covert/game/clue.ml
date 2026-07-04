open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Containers

type means =
  | Photo
  | Wiretap
  | Surveillance
  | File_search
  | Local_informant
  | Interpol_db
  | Local_authorities
  [@@deriving enum]

let means_list = Iter.map (fun i -> means_of_enum i |> Option.get_exn_or "oops")
  Iter.(0 -- (means_to_enum Local_authorities)) |> Iter.to_list

type connect =
  | Face of Agent_d.Id.t
  | Agent of Agent_d.Id.t
  | Org of Org.Id.t
  | Loc of Loc.Id.t
  | Role of Role_d.Id.t
  [@@deriving yojson]

type t = {
  org: Org.Id.t;
  loc: Loc.Id.t;
  role: Role.Id.t;
  connect: connect;
  rand_seed: int;
  discovery_val: int;
} [@@deriving yojson]

module Id = Engine.Int_id.Make()

module Map = struct
  include Utils.Map.Make(struct 
    type t = Id.t [@@deriving yojson]
    let compare = Id.compare
  end)
end

type map = t Map.t

