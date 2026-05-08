open Containers
module C = Constants

type t = {
  city: string;
  country: string;
  connect: int * int;
  lawless: int;
  known_buildings: int;
  some_buildings: int;
  loc: int * int;
} [@@deriving show]

module Id = Engine.Int_id.Make()

let random r = Random.int C.num_locs r |> Id.of_int

module Map = struct
  include Utils.Map.Make(struct
  type t = Id.t [@@deriving yojson]
  let compare = Id.compare
end)

  let of_simple_list l =
    List.mapi (fun i x -> Id.of_int i, x) l |> of_list
end

type map = t Map.t

