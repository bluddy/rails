open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
module LocuSet = Utils.LocuSet

module Id = Int_id.Make(struct end)

type info = {
  mutable count: int;
  mutable double: Track.double;
} [@@deriving yojson, show]

type t = {
  info: (Id.t, info) Utils.Hashtbl.t;
  stations: (Utils.loc * Dir.upper, Id.t) Utils.Hashtbl.t;
} [@@deriving yojson, show]


