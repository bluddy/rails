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

type t = {
  org: Org.Id.t;
  loc: Loc.Id.t;
  role: Role.Id.t;
  connect: int * int;
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
