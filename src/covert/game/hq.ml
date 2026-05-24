open! Containers

type t =
  | Hideout
  | Agent
  | Safehouse
  | Active_cel
  | Office
  [@@deriving yojson]

let of_enum = function
  | 1 -> Some Hideout
  | 2 -> Some Agent
  | 3 -> Some Safehouse
  | 4 -> Some Active_cel
  | 5 -> Some Office
  | _ -> None

