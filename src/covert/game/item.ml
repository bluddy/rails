open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Containers
module String = Engine.String
module Gen = Engine.My_gen

type t = {
  name: string;
  img: int;
  agent: Agent_id.t option;
}
[@@deriving yojson]

let set_agent agent_id v = {v with agent=Some agent_id}
let clear_agent v = {v with agent=None}

module Id = Engine.Int_id.Make()

let from_stream s =
  Iter.fold (fun acc _ ->
    let name = Gen.take 16 s |> Gen.to_stringi |> String.remove_nulls in
    let img = Gen.get_wordi s in
    let agent = Gen.get_wordi s 
    |> fun n -> if n = 0xFF then None else Agent_id.of_int n |> Option.some
    in
    {name; img; agent}::acc
  )
  []
  Iter.(0 -- 3) |> List.rev

module Map = struct
  include Utils.Map.Make(struct
    type t = Id.t [@@deriving yojson, ord]
  end)
  let of_ordered_list l = List.mapi (fun i x -> Id.of_int i, x) l |> of_list
end

type map = t Map.t [@@deriving yojson]

module S = struct
  let update item_id items fn = Map.update item_id (Option.map fn) items
end
