open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Containers

type status =
  | Arrested
  | In_hiding
  | Double_agent
  | Escaped
  | Exchanged
  | Out_of_hiding
  | At_large of {anxiety: int}
  | In_custody
  [@@deriving yojson]

type t = {
  gender: Gender.t;
  org: Org.Id.t;
  name: string;
  last_name: string;
  id_code: int; (* Used to generate the name, sex and picture *)
  loc: Loc.Id.t;
  known: Known_data.Set.t;
  roles: Role_d.Set.t;
  roles_known: Role_d.Set.t;
  status: status;
} [@@deriving yojson]

module Id = Agent_id

module Map = Utils.Map.Make(struct
  type t = Id.t [@@deriving yojson, ord]
end)

type map = t Map.t [@@deriving yojson]
