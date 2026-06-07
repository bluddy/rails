open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Id = Engine.Int_id.Make()

include Id

module Map = struct
  include Utils.Map.Make(struct
    type t = Id.t [@@deriving yojson, ord]
  end)

  let of_ordered_list l = List.mapi (fun i x -> Id.of_int i, x) l |> of_list

  let fold_not_last f v acc =
    let len = cardinal v in
    let rec loop n acc =
      if n >= len - 1 then acc
      else
        let id = Id.of_int n in
        let x = find id v in
        let acc = f id x acc in
        loop (n+1) acc
    in
    loop 0 acc
end
