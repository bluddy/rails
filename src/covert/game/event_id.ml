open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Id = Engine.Int_id.Make()

include Id

module Map = struct
  include Utils.Map.Make(struct 
    type t = Id.t [@@deriving yojson]
    let compare = Id.compare
  end)

  let of_ordered_list l = List.mapi (fun i x -> Id.of_int i, x) l |> of_list
end
