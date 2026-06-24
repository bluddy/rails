(* Types for bulletin messages of different sorts *)
open! Containers

type t =
  | Agent_out_of_hiding of {name: string} (* Might not know name at time *)
  | Agent_hiding of {name: string}
  | Item_spotted of {item: Item.Id.t; loc: Loc.Id.t}
  | Text of string
  | Satellite_from of Loc.Id.t
  | Satellite_to of Loc.Id.t
