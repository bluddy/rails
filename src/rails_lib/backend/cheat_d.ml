open! Containers

  type t =
  | Add500Cash
  | CreatePriorityShipment
  | CancelPriorityShipment
  | CreateAi
  [@@deriving show, yojson]
