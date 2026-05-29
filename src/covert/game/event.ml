open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
module String = Engine.String
module Gen = Engine.My_gen

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

let from_stream ~num_events s =
  Iter.fold (fun acc _ ->
    let role = Gen.get_bytei s |> Role.Id.of_int in
    let _junk = Gen.get_bytei s in
    let tick_or_status = Gen.get_wordi s in
    let num_id = Gen.get_wordi s in
    let text = Gen.take 32 s |> Gen.to_stringi |> String.remove_nulls in
    let bits = Gen.get_wordi s in
    let item_bits = Gen.get_wordi s in
    let efficiency = Gen.get_wordi s in
    let event = {
      role;
      tick_or_status;
      num_id;
      text;
      bits;
      item_bits;
      efficiency;
    } in
    print_endline @@ show event;
    event::acc
  )
  []
  Iter.(0 -- (num_events - 1)) |> List.rev

