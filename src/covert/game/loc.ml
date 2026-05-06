
type t = {
  city: string;
  country: string;
  connect: int * int;
  lawless: int;
  known_buildings: int;
  some_buildings: int;
  loc: int * int;
} [@@deriving show]

module Idx = Engine.Int_id.Make()
