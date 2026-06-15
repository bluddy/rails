open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Containers
module String = Engine.String
module Gen = Engine.My_gen

type t = {
  name: string;
  img: int;
  agent: Agent_id.t;
}
[@@deriving yojson]

let set_agent agent_id v = {v with agent=agent_id}

module Id = Engine.Int_id.Make()

let from_stream s =
  Iter.fold (fun acc _ ->
    let name = Gen.take 16 s |> Gen.to_stringi |> String.remove_nulls in
    let img = Gen.get_wordi s in
    let agent = Gen.get_wordi s |> Agent_id.of_int in
    {name; img; agent}::acc
  )
  []
  Iter.(0 -- 3) |> List.rev

module Map = Utils.Map.Make(struct
    type t = Id.t [@@deriving yojson, ord]
  end)

type map = t Map.t [@@deriving yojson]

module S = struct
  let update item_id items fn = Map.update item_id (Option.map fn) items
end
