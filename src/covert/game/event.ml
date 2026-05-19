open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

type t = {
  role: Role.Id.t;
  tick_or_status: int;
  num_id: int;
  text: string;
  bits: int;
  item_bits: int;
  efficiency: int;
} [@@deriving yojson, show]

module Id = Engine.Int_id.Make()

module Map = struct
  include Utils.Map.Make(struct 
    type t = Id.t [@@deriving yojson]
    let compare = Id.compare
  end)

  let of_ordered_list l = List.mapi (fun i x -> Id.of_int i, x) l |> of_list
end

type map = t Map.t

