open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Containers

(* Break cycle *)

module Id = Engine.Int_id.Make() (* Id in current session *)

include Id

module Set = Utils.Set.Make(struct
  type t = Id.t [@@deriving yojson, ord]
end)
